# Copyright 2014 Adam Grandquist, all rights reserved.

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


def clean(args):
    shutil.rmtree('build', ignore_errors=True)
    shutil.rmtree('tests/__pycache__', ignore_errors=True)


def lint(args):
    pass


def build(args):
    debug = os.getenv('DEBUG')
    plat = os.getenv('PLAT', args.plat)

    incl = os.getenv('LUAINC', args.incl)
    build = os.getenv('LUAPREFIX', args.build)

    incl_macosx = os.getenv('LUAINC_macosx_base', incl)
    build_macosx = os.path.join('..', '..', os.getenv('LUAPREFIX_macosx', build))

    incl_linux = os.getenv('LUAINC_linux_base', incl)
    build_linux = os.path.join('..', '..', os.getenv('LUAPREFIX_linux', build))

    try:
        os.mkdir(os.path.join(
            'luasocket', 'src',
            build_macosx if args.plat == 'macosx' else build_linux
        ))
    except OSError:
        print('luasocket build directory already created.')

    cmd = ['make', 'install-both']
    env = dict(
        os.environ,
        PLAT=plat,
        DEBUG=debug or '',
        **({
            'LUAINC_macosx_base': incl_macosx,
            'LUAPREFIX_macosx': build_macosx
        } if args.plat == 'macosx' else {
            'LUAINC_linux_base': incl_linux,
            'LUAPREFIX_linux': build_linux
        })
    )

    with subprocess.Popen(cmd, cwd='luasocket', env=env) as io:
        if io.wait():
            print(repr(cmd), "returned:", io.returncode)
            exit(io.returncode)


def main():
    parser = argparse.ArgumentParser(description='Process some integers.')

    platform = 'macosx' if sys.platform.startswith('darwin') else 'linux'

    parser.add_argument('action', nargs='?', default='build')
    parser.add_argument('-p', '--plat', default=platform)
    parser.add_argument('-i', '--incl', default='/usr/local/include')
    parser.add_argument('-b', '--build', default='build')
    parser.add_argument('-j', type=int)

    args = parser.parse_args()

    {
        'test': test,
        'lint': lint,
        'clean': clean,
        'build': build
    }[args.action](args)

if __name__ == '__main__':
    main()
