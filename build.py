import argparse
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

    print('linting src:')

    returncode = subprocess.call([
        'luac', 'ast.lua', 'errors.lua', 'net.lua', 'rethinkdb.lua', 'util.lua'
    ], cwd='src')
    if returncode:
        print('`luac ast.lua errors.lua net.lua rethinkdb.lua util.lua` '
              'returned:', returncode)
        exit(returncode)

    print('linting successful')


def build(args):
    import re
    import string

    import ReQLprotodef as protodef

    print('building templates:')

    class BuildFormat(string.Formatter):
        fspec = re.compile('--\[\[(.+?)\]\]')

        def parse(self, string):
            last = 0
            for match in self.fspec.finditer(string):
                yield string[last:match.start()], match.group(1), '', 's'
                last = match.end()
            yield string[last:], None, None, None

    class ASTChecker(BuildFormat):
        terms_found = set()
        def get_field(self, name, args, kwargs):
            if name.startswith('Term.'):
                self.terms_found.add(name[5:])
            return super(ASTChecker, self).get_field(name, args, kwargs)

        def check_unused_args(self, used, args, kwargs):
            expected = {
                term for term in dir(protodef.Term.TermType)
                if not term.startswith('__')
            } - {'DATUM', 'IMPLICIT_VAR'}
            unused = expected - self.terms_found
            if unused:
                raise ValueError('Found {} unused terms.'.format(unused))

    print('building ast.lua')

    with open('src/ast.pre.lua') as io:
        s = io.read()
    s = ASTChecker().vformat(s, (), {
        'Term': protodef.Term.TermType
    })
    with open('src/ast.lua', 'w') as io:
        io.write(s)

    print('building net.lua')

    with open('src/net.pre.lua') as io:
        s = io.read()
    s = BuildFormat().vformat(s, (), {
        'Protocol': protodef.VersionDummy.Protocol,
        'Query': protodef.Query.QueryType,
        'Response': protodef.Response.ResponseType,
        'Term': protodef.Term.TermType,
        'Version': protodef.VersionDummy.Version
    })
    with open('src/net.lua', 'w') as io:
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
