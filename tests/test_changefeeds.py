import util


class TestChangeFeeds(util.LuaTestCase):
    def setUp(self):
        self.create_table('changefeeds.watched', [
            {'id': 1}, {'id': 2}, {'id': 3},
            {'id': 4}, {'id': 5}, {'id': 6}
        ])

    def test_all(self):
        self.expect('test_changefeeds_all', [7, 8, 9, 10])

    def test_even(self):
        self.expect('test_changefeeds_even', [8, 10])


if __name__ == '__main__':
    unittest.main()
