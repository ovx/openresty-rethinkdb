import util


class TestArrayLimits(util.LuaTestCase):
    def setUp(self):
        self.create_table('array.limits')

    def test_create(self):
        self.expect('test_arraylimits_create', 'ReQLRuntimeError Array over size limit `4`. in:\n{1, 2, 3, 4, 5, 6, 7, 8}')

    def test_equal(self):
        self.expect('test_arraylimits_equal', [1, 2, 3, 4, 5, 6, 7, 8])

    def test_huge(self):
        self.expect('test_arraylimits_huge', 100001)

    def test_huge_read(self):
        self.expect('test_arraylimits_huge_read', None)

    def test_huge_table(self):
        self.expect('test_arraylimits_huge_table', {'deleted': 0, 'errors': 1, 'first_error': 'Array too large for disk writes (limit 100,000 elements)', 'inserted': 1, 'replaced': 0, 'skipped': 0, 'unchanged': 0})

    def test_lessthan(self):
        self.expect('test_arraylimits_lessthan', 'ReQLRuntimeError Array over size limit `4`. in:\nr({1, 2, 3, 4}):union({5, 6, 7, 8})')

    def test_lessthan_read(self):
        self.expect('test_arraylimits_lessthan_read', {'array': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 'id': 1})

    def test_negative(self):
        self.expect('test_arraylimits_negative', 'ReQLCompileError Illegal array size limit `-1`. in:\n{1, 2, 3, 4, 5, 6, 7, 8}')

    def test_zero(self):
        self.expect('test_arraylimits_zero', 'ReQLCompileError Illegal array size limit `0`. in:\n{1, 2, 3, 4, 5, 6, 7, 8}')


if __name__ == '__main__':
    unittest.main()
