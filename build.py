import os
import re
import string
import subprocess
import pathlib

import ReQLprotodef as protodef


def lint_dir(d):
    for test in pathlib.Path(d).glob('*.lua'):
        if '.pre' in test.suffixes:
            continue
        returncode = subprocess.call(['luac', str(test.relative_to(d))], cwd=d)
        if returncode:
            exit(returncode)


def lint():
    build()

    print('linting driver')

    lint_dir('src')

    print('linting tests')

    lint_dir('spec')

    print('linting successful')


def build():
    print('building rethinkdb.lua')

    ast_constants = list({
        term for term in dir(protodef.Term.TermType)
        if not term.startswith('_')
    } - {'DATUM', 'IMPLICIT_VAR'})

    ast_constants.sort()

    ast_method_names = {
        name: {
            'AND': 'and_', 'BRACKET': 'index', 'ERROR': 'error_',
            'FUNCALL': 'do_', 'JAVASCRIPT': 'js', 'NOT': 'not_', 'OR': 'or_'
        }.get(name, name.lower())
        for name in ast_constants
    }
    ast_classes = [
        '{0} = ast({0!r}, {{tt = {1}, st = {2!r}}})'.format(
            name,
            getattr(protodef.Term.TermType, name),
            ast_method_names[name]
        ) for name in ast_constants
    ]

    def const_args(num):
        args = ['arg{}'.format(n) for n in range(num)]
        pieces = [
            '{} = function(',
            ', '.join(args + ['opts']),
            ') return {}(',
            ', '.join(['opts'] + args),
            ') end'
        ]
        return ''.join(pieces)

    ast_methods_w_opt = dict(
        {
            name: '{} = function(...) return {}(get_opts(...)) end'
            for name in (
                'CIRCLE', 'DELETE', 'DISTINCT', 'EQ_JOIN', 'FILTER', 'GET_ALL',
                'GET_INTERSECTING', 'GET_NEAREST', 'GROUP', 'HTTP',
                'INDEX_CREATE', 'INDEX_RENAME', 'ISO8601', 'JAVASCRIPT',
                'ORDER_BY', 'RANDOM', 'REPLACE', 'SLICE', 'TABLE',
                'TABLE_CREATE', 'UPDATE'
            )
        },
        BETWEEN=const_args(3),
        BETWEEN_DEPRECATED=const_args(3),
        DISTANCE=const_args(2),
        DURING=const_args(3),
        FILTER=const_args(2),
        INSERT=const_args(2),
        UPDATE=const_args(2)
    )
    ast_methods = [
        ast_methods_w_opt.get(
            name, '{} = function(...) return {}({{}}, ...) end'
        ).format(
            ast_method_names[name], name
        ) for name in ast_constants
    ]

    ast_constants.reverse()

    lines = ['local {}'.format(ast_constants.pop())]
    while ast_constants:
        name = ast_constants.pop()
        if len(lines[-1]) + len(name) < 77:
            lines[-1] += ', {}'.format(name)
        else:
            lines.append('local {}'.format(name))

    class BuildFormat(string.Formatter):
        fspec = re.compile('--\[\[(.+?)\]\]')

        def parse(self, string):
            last = 0
            for match in self.fspec.finditer(string):
                yield string[last:match.start()], match.group(1), '', 's'
                last = match.end()
            yield string[last:], None, None, None

    formatter = {
        'AstClasses': '\n'.join(ast_classes),
        'AstMethods': ',\n  '.join(ast_methods),
        'AstNames': '\n'.join(lines),
        'Query': protodef.Query.QueryType,
        'Response': protodef.Response.ResponseType,
        'Term': protodef.Term.TermType,
    }

    for path in pathlib.Path('src').glob('*.pre.lua'):
        path.with_suffix('').with_suffix('.lua').write_text(
            BuildFormat().vformat(path.read_text(), (), formatter)
        )

    print('building successful')


if __name__ == '__main__':
    lint()
