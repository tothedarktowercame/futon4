import importlib.util
import unittest
from pathlib import Path


SCRIPT = Path(__file__).parents[1] / "dev/mojira-anykey-check.py"
SPEC = importlib.util.spec_from_file_location("mojira_anykey_check", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class AnyKeyCheckTest(unittest.TestCase):
    def by_code(self, codes):
        return {row["code"]: row for row in MODULE.analyse_codes(codes)}

    def test_ambiguous_lean_teeth(self):
        rows = self.by_code(["c1", "c13", "z"])
        self.assertEqual(rows["c1"]["costNoRet"], 2)
        self.assertEqual(rows["c1"]["costRet"], 3)
        self.assertEqual(rows["c1"]["extends_to"], "c13")

    def test_six_cost_fails_any_key_five(self):
        result = MODULE.report(MODULE.analyse_codes(["abcde", "abcdef", "z"]), "fixture")
        rows = {row["code"]: row for row in result["over_five"]}
        self.assertEqual(rows["abcde"]["costRet"], 6)
        self.assertEqual(result["anyKeyFive"], "fails (2 codes)")

    def test_prefix_free_teeth_hold(self):
        result = MODULE.report(MODULE.analyse_codes(["a", "bb", "cdefg"]), "fixture")
        self.assertEqual(result["needs_ret"], [])
        self.assertEqual(result["anyKeyFive"], "holds")

    def test_spaces_are_inserted_not_typed(self):
        row = self.by_code(["c 13"])["c 13"]
        self.assertEqual(row["typed"], "c13")
        self.assertEqual(row["costNoRet"], 3)


if __name__ == "__main__":
    unittest.main()
