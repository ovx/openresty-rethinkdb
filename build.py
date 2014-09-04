# Copyright 2014 Adam Grandquist, all rights reserved.

import argparse
import os
import shutil
import subprocess


def test(args):
    pass


def clean(args):
    shutil.rmtree('build', onerror=print)


def lint(args):
    pass


def build(args):
    debug = os.getenv('DEBUG')
    plat = os.getenv('PLAT', args.plat)

    incl = os.getenv('LUAINC', args.incl)
    build = os.getenv('LUAPREFIX', args.build)

    incl_macosx = os.getenv('LUAINC_macosx_base', incl)
    build_macosx = os.getenv('LUAPREFIX_macosx', build)

    incl_linux = os.getenv('LUAINC_linux_base', incl)
    build_linux = os.getenv('LUAPREFIX_linux', build)

    cmd = [
        'make',
        'install-both',
        'PLAT={}'.format(plat),
        'LUAINC_macosx_base={}'.format(incl_macosx),
        'LUAPREFIX_macosx={}'.format(build_macosx),
        'LUAINC_linux_base={}'.format(incl_linux),
        'LUAPREFIX_linux={}'.format(build_linux)
    ] + (['DEBUG={}'.format(debug)] if debug is not None else [])

    with subprocess.Popen(cmd, cwd='luasocket') as io:
        pass


def main():
    parser = argparse.ArgumentParser(description='Process some integers.')

    parser.add_argument('action', nargs='?', default='build')
    parser.add_argument('-p', '--plat', default='macosx')
    parser.add_argument('-i', '--incl', default='/usr/local/include')
    parser.add_argument('-b', '--build', default='../../build')
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
