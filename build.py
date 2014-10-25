import sys
if sys.version_info[0] < 3:
    print('Support for building with Python2 is not provided.')
    exit(0)

import argparse
import os
import struct
import subprocess


def spec(args):
    if not args.f:
        lint(args)

    import time

    io = subprocess.Popen(
        'rethinkdb', cwd='spec',
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
    )
    time.sleep(4)
    res = subprocess.call('busted')
    io.terminate()
    io.wait()
    exit(res)


def clean(args):
    import shutil
    shutil.rmtree('__pycache__', ignore_errors=True)
    shutil.rmtree('spec/rethinkdb_data', ignore_errors=True)


def lint(args):
    if not args.f:
        build(args)

    print('linting rethinkdb.lua')

    returncode = subprocess.call(['luac', 'rethinkdb.lua'], cwd='src')
    if returncode:
        exit(returncode)

    print('linting tests')

    for test in os.listdir('spec'):
        if test.endswith('.lua'):
            returncode = subprocess.call(['luac', test], cwd='spec')
            if returncode:
                exit(returncode)

    print('linting successful')


def build(args):
    import re
    import string

    import ReQLprotodef as protodef

    print('building rethinkdb.lua')

    name_re = re.compile('(^|_)(\w)')
    ast_constants = {
        term for term in dir(protodef.Term.TermType)
        if not term.startswith('__')
    } - {'DATUM', 'IMPLICIT_VAR'}
    ast_class_names = {
        name: {
            'FUNCALL': 'Do', 'ISO8601': 'ISO8601',
            'JAVASCRIPT': 'JavaScript', 'TO_ISO8601': 'ToISO8601',
            'UUID': 'UUID'
        }.get(name, name_re.sub(lambda m: m.group(2).upper(), name.lower()))
        for name in ast_constants
    }
    ast_method_names = {
        name: {
            'BRACKET': 'index', 'ERROR': 'error_', 'FUNCALL': 'do_',
            'JAVASCRIPT': 'js', 'MAKE_ARRAY': 'array', 'NOT': 'not_'
        }.get(name, name.lower())
        for name in ast_constants
    }
    ast_classes = [
        '{0} = ast({0!r}, {{tt = {1}, st = {2!r}}})'.format(
            ast_class_names[name],
            getattr(protodef.Term.TermType, name),
            ast_method_names[name]
        ) for name in ast_constants
    ]
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
        BETWEEN=
        '{} = function(self, left, right, opts) return {}(opts, self, left, right) end',
        DISTANCE='{} = function(self, g, opts) return {}(opts, self, g) end',
        DURING=
        '{} = function(t1, t2, t3, opts) return {}(opts, t1, t2, t3) end',
        INSERT='{} = function(tbl, doc, opts) return {}(opts, tbl, doc) end',
        UPDATE='{} = function(tbl, doc, opts) return {}(opts, tbl, doc) end'
    )
    ast_methods = [
        ast_methods_w_opt.get(
            name,
            '{} = function(...) return {}({{}}, ...) end'
        ).format(
            ast_method_names[name],
            ast_class_names[name]
        ) for name in ast_constants
    ]

    ast_class_names = list(ast_class_names.values())
    ast_class_names.sort(reverse=True)

    lines = ['local {}'.format(ast_class_names.pop())]
    while ast_class_names:
        name = ast_class_names.pop()
        if len(lines[-1]) + len(name) < 77:
            lines[-1] += ', {}'.format(name)
        else:
            lines.append('local {}'.format(name))

    ast_classes.sort()
    ast_methods.sort()

    def encode_magic(num):
        return "'\\{}'".format('\\'.join(map(str, struct.pack(
            "<L", num
        ))))

    class BuildFormat(string.Formatter):
        fspec = re.compile('--\[\[(.+?)\]\]')

        def parse(self, string):
            last = 0
            for match in self.fspec.finditer(string):
                yield string[last:match.start()], match.group(1), '', 's'
                last = match.end()
            yield string[last:], None, None, None

    with open('src/rethinkdb.pre.lua') as io:
        s = io.read()
    s = BuildFormat().vformat(s, (), {
        'AstClasses': '\n'.join(ast_classes),
        'AstMethods': ',\n  '.join(ast_methods),
        'AstNames': '\n'.join(lines),
        'Protocol': encode_magic(protodef.VersionDummy.Protocol.JSON),
        'Query': protodef.Query.QueryType,
        'Response': protodef.Response.ResponseType,
        'Term': protodef.Term.TermType,
        'Version': encode_magic(protodef.VersionDummy.Version.V0_3)
    })
    with open('src/rethinkdb.lua', 'w') as io:
        io.write(s)

    print('building successful')


def install(args):
    if not args.f:
        lint(args)

    returncode = subprocess.call(['luarocks', 'make'])
    if returncode:
        exit(returncode)

    print('install successful')


def main():
    parser = argparse.ArgumentParser(description='Build Lua-ReQL.')

    parser.add_argument('action', nargs='?', default='spec')
    parser.add_argument('-f', action='store_true')

    args = parser.parse_args()

    {
        'spec': spec,
        'lint': lint,
        'clean': clean,
        'install': install,
        'build': build
    }[args.action](args)

if __name__ == '__main__':
    main()
