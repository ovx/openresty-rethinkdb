# Copyright 2014 Adam Grandquist, all rights reserved.

import argparse


def test(args):
    pass


def clean(args):
    pass


def lint(args):
    pass


def build(args):
    pass


def main():
    parser = argparse.ArgumentParser(description='Process some integers.')

    parser.add_argument('action', nargs='?', default='build')
    parser.add_argument('-p', '--plat', default='macosx')
    parser.add_argument('-i', '--incl', default='/usr/local/include')
    parser.add_argument('-b', '--build', default='./../build')
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
