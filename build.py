import argparse
import os
import shutil
import subprocess
import sys
import time


def test(args):
    import unittest
    res = unittest.TestResult()
    io = subprocess.Popen(['rethinkdb'], cwd='build')
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
    shutil.rmtree('build', ignore_errors=True)
    shutil.rmtree('tests/__pycache__', ignore_errors=True)


def lint(args):
    pass


def build(args):
    pass


def install(args):
    with subprocess.Popen(['luarocks', 'make']) as io:
        if io.wait():
            print(repr(cmd), "returned:", io.returncode)
            exit(io.returncode)


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
