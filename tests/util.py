import json
import os
import re
import subprocess
import unittest

import rethinkdb as r


def lua_version():
    version = subprocess.check_output(['lua', '-v'])
    version = re.match(b'Lua (\d)\.(\d)\.(\d)', version)
    if not version: return 0
    version = version.group(1, 2, 3)
    return int(version[0]) + 0.1 * int(version[1]) + 0.01 * int(version[2])

class LuaTestCase(unittest.TestCase):
    tables = None

    def create_table(self, table, data=None):
        db, _, table = table.rpartition('.')
        db = db or 'test'
        with r.connect() as c:
            try:
                r.db_create(db).run(c)
            except r.errors.RqlRuntimeError:
                pass
            try:
                r.db(db).table_create(table).run(c)
            except r.errors.RqlRuntimeError:
                pass
            table = r.db(db).table(table)
            if data: table.insert(data).run(c)
        self.tables = self.tables or []
        self.tables.append(table)
        return table

    def deep_sort(self, res):
        if isinstance(res, dict):
            return {k: self.deep_sort(v) for k, v in res.items()}
        if isinstance(res, list):
            res = list(map(self.deep_sort, res))
            res.sort()
        return res

    def expect(self, lua, expected, **kwargs):
        try:
            res = subprocess.check_output(
                ['lua', lua + '.lua'],
                env=dict(
                    os.environ,
                    LUA_PATH='../src/?.lua;;'
                ), stderr=subprocess.STDOUT, timeout=10, cwd='tests',
                **kwargs
            )
        except subprocess.SubprocessError as e:
            print(e.output.decode())
            res = e.output
        res = res.decode()
        try:
            output = json.loads(res)
        except:
            output = res
        if isinstance(output, str):
            output = output.strip()
        self.assertEqual(self.deep_sort(output), expected)

    def tearDown(self):
        if self.tables:
            with r.connect() as c:
                for table in self.tables:
                    table.delete().run(c)
