import argparse
import subprocess
import time
import string


def test(args):
    import unittest
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
    shutil.rmtree('build', ignore_errors=True)
    shutil.rmtree('tests/__pycache__', ignore_errors=True)
    shutil.rmtree('tests/rethinkdb_data', ignore_errors=True)


def lint(args):
    pass


def build(args):
    import ReQLprotodef as protodef
    import re
    class BuildFormat(string.Formatter):
        def parse(self, format_string):
            last_end = 0
            for match in re.finditer('--\[\[(.+?)\]\]', format_string):
                yield format_string[last_end:match.start()], match.group(1), '', 's'
                last_end = match.end()
            yield format_string[last_end:], None, None, None
    with open('src/ast.pre.lua') as io:
        s = io.read()
    s = BuildFormat().vformat(s, (), {
        'Term': protodef.Term.TermType
    })
    with open('src/ast.lua', 'w') as io:
        io.write(s)
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



def install(args):
    returncode = subprocess.call(['luarocks', 'make'])
    if returncode:
        print('`luarocks make` returned:', returncode)
        exit(returncode)


def main():
    parser = argparse.ArgumentParser(description='Process some integers.')

    parser.add_argument('action', nargs='?', default='build')
    parser.add_argument('-j', type=int)

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
