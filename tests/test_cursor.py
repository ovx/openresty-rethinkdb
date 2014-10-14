import random
import util


class TestCursor(util.LuaTestCase):
    def setUp(self):
        self.num_rows = random.randint(1111, 2222)
        document = list(range(500))
        self.create_table('cursor.tests', [{'nums': document}] * self.num_rows)

    def test_type(self):
        self.expect('test_cursor_type', 'Cursor')

    def test_count(self):
        self.expect('test_cursor_count', self.num_rows)

    def test_close(self):
        self.expect('test_cursor_close', '')


if __name__ == '__main__':
    unittest.main()
