import os
import subprocess
import unittest

import rethinkdb as r


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

    def run_lua(self, file, **kwargs):
        return self.run_cmd(
            ['lua', file + '.lua'],
            env=dict(
                os.environ,
                LUA_PATH='../src/?.lua;;'
            ), **kwargs
        )

    def run_cmd(self, cmd, **kwargs):
        try:
            return subprocess.check_output(
                cmd, stderr=subprocess.STDOUT, timeout=10, cwd='tests', **kwargs
            ).decode().strip()
        except subprocess.SubprocessError as e:
            print(e.output.decode())
            return e.output.decode().strip()

    def tearDown(self):
        if not self.tables:
            return
        with r.connect() as c:
            for table in self.tables:
                table.delete().run(c)
