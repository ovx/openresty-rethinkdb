import argparse
import struct
import subprocess


def test(args):
    if not args.f:
        lint(args)

    import unittest
    import time

    res = unittest.TestResult()
    io = subprocess.Popen(['rethinkdb'], cwd='tests')
    time.sleep(4)
    unittest.defaultTestLoader.discover('./tests').run(res)
    for error in res.errors:
        print(error[0])
        print(error[1])
    for fail in res.failures:
        print(fail[0])
        print(fail[1])
    print('''
tests run: {}
errors: {}
failures: {}
skipped: {}'''.format(
        res.testsRun, len(res.errors), len(res.failures), len(res.skipped)))
    io.terminate()
    io.wait()
    if not res.wasSuccessful():
        exit(1)


def clean(args):
    import shutil
    shutil.rmtree('__pycache__', ignore_errors=True)
    shutil.rmtree('tests/__pycache__', ignore_errors=True)
    shutil.rmtree('tests/rethinkdb_data', ignore_errors=True)


def lint(args):
    if not args.f:
        build(args)

    print('linting rethinkdb.lua')

    returncode = subprocess.call(['luac', 'rethinkdb.lua'], cwd='src')
    if returncode:
        print('`luac rethinkdb.lua` '
              'returned:', returncode)
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
            'BRACKET': 'index', 'FUNCALL': 'do_', 'JAVASCRIPT': 'js',
            'MAKE_ARRAY': 'array', 'NOT': 'not_'
        }.get(name, name.lower())
        for name in ast_constants
    }
    ast_classes = [
        '{0} = ast(\'{0}\', {{tt = {1}, st = \'{2}\'}})'.format(
            ast_class_names[name],
            getattr(protodef.Term.TermType, name),
            ast_method_names[name]
        ) for name in ast_constants
    ]
    ast_methods_w_opt = {
        name: '{} = function(...) return {}(get_opts(...)) end'
        for name in (
            'BETWEEN', 'CIRCLE', 'DELETE', 'DISTANCE', 'DISTANCE', 'DISTINCT',
            'DURING', 'EQ_JOIN', 'FILTER', 'GET_ALL', 'GET_INTERSECTING',
            'GET_NEAREST', 'GROUP', 'HTTP', 'INDEX_CREATE', 'INDEX_RENAME',
            'INSERT', 'ISO8601', 'JAVASCRIPT', 'ORDER_BY', 'RANDOM', 'REPLACE',
            'SLICE', 'TABLE', 'TABLE', 'TABLE_CREATE', 'TABLE_CREATE', 'UPDATE'
        )
    }
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
        print('`luarocks make` returned:', returncode)
        exit(returncode)


def main():
    parser = argparse.ArgumentParser(description='Build Lua-ReQL.')

    parser.add_argument('action', nargs='?', default='test')
    parser.add_argument('-f', action='store_true')

    args = parser.parse_args()

    {
        'test': test,
        'lint': lint,
        'clean': clean,
        'install': install,
        'build': build
    }[args.action](args)

if __name__ == '__main__':
    main()
