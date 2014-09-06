import subprocess
import unittest


class LuaTestCase(unittest.TestCase):
    pass


def run(stdin, cmd, cb, **kwargs):
    with subprocess.Popen(cmd, **kwargs) as io:
        if io.wait():
            cb(stdout, stderr, io.returncode)
        else:
            cb(stdout, stderr, 0)
