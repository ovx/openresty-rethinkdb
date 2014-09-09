import util

class TestChangeFeeds(util.LuaTestCase):
    def setUp(self):
        self.create_table('changefeeds.watched', [
            {'id': 1}, {'id': 2}, {'id': 3},
            {'id': 4}, {'id': 5}, {'id': 6}
        ])

    def test_all_changes(self):
        self.assertEqual(self.run_lua('test_changefeeds_all'), "7\n8\n9\n10\n")

    def test_even_changes(self):
        self.assertEqual(self.run_lua('test_changefeeds_even'), "8\n10\n")


if __name__ == '__main__':
    unittest.main()
