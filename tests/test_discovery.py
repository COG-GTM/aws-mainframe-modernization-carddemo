"""Tests for docs/discovery/build_discovery.py and the files it generates.

Run with:  python3 -m pytest tests/test_discovery.py
"""
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DISCOVERY = ROOT / "docs" / "discovery"
SCRIPT = DISCOVERY / "build_discovery.py"

sys.path.insert(0, str(DISCOVERY))
import build_discovery as bd  # noqa: E402


def load_json():
    return json.loads((DISCOVERY / "inventory.json").read_text(encoding="utf-8"))


def md(name):
    return (DISCOVERY / name).read_text(encoding="utf-8")


def md_table_rows(text, heading):
    """Rows (list of cell lists) of the first pipe table under the given heading line."""
    lines = text.splitlines()
    start = next(i for i, l in enumerate(lines) if l.strip() == heading)
    rows, in_table = [], False
    for l in lines[start + 1:]:
        if l.startswith("|"):
            in_table = True
            cells = [c.strip() for c in l.strip().strip("|").split("|")]
            if not all(re.fullmatch(r":?-+:?", c) for c in cells):
                rows.append(cells)
        elif in_table:
            break
    return rows[1:]  # drop header row


def run_script(*args, cwd=ROOT):
    return subprocess.run([sys.executable, str(SCRIPT), *args], cwd=cwd,
                          capture_output=True, text=True)


class TestGeneratedFilesCurrent(unittest.TestCase):
    def test_check_passes_on_committed_files(self):
        r = run_script("--check")
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_generation_is_deterministic(self):
        first = bd.generate()
        second = bd.generate()
        self.assertEqual(first, second)
        for name, body in first.items():
            self.assertEqual(body, (DISCOVERY / name).read_text(encoding="utf-8"), name)


class TestInventoryCountsBothWays(unittest.TestCase):
    def setUp(self):
        self.data = load_json()
        self.rows = md_table_rows(md("01-inventory.md"), "## Counts by artifact type")
        self.md_counts = {r[1]: int(r[2]) for r in self.rows if r[1]}

    def test_every_json_type_is_in_markdown(self):
        json_counts = self.data["summary"]["artifact_counts"]
        for t, n in json_counts.items():
            self.assertIn(t, self.md_counts, f"type {t} missing from 01-inventory.md")
            self.assertEqual(self.md_counts[t], n, t)

    def test_every_markdown_type_is_in_json(self):
        json_counts = self.data["summary"]["artifact_counts"]
        for t, n in self.md_counts.items():
            self.assertIn(t, json_counts, f"type {t} in 01-inventory.md but not in inventory.json")
            self.assertEqual(json_counts[t], n, t)

    def test_summary_counts_match_artifact_list(self):
        arts = self.data["artifacts"]
        by_type = {}
        for a in arts:
            by_type[a["type"]] = by_type.get(a["type"], 0) + 1
        self.assertEqual(by_type, dict(self.data["summary"]["artifact_counts"]))
        self.assertEqual(len(arts), self.data["summary"]["artifact_total"])
        total_row = next(r for r in self.rows if r[0].strip("*") == "Total")
        self.assertEqual(int(total_row[2].strip("*")), len(arts))

    def test_full_inventory_table_has_one_row_per_artifact(self):
        rows = md_table_rows(md("01-inventory.md"), "## Full inventory")
        self.assertEqual(len(rows), len(self.data["artifacts"]))
        paths_md = {r[0].strip("`") for r in rows}
        self.assertEqual(paths_md, {a["path"] for a in self.data["artifacts"]})


class TestControlTotals(unittest.TestCase):
    def setUp(self):
        self.data = load_json()
        self.s = self.data["summary"]
        self.readme = md("README.md")

    def test_pinned_control_totals(self):
        """Pinned to the current source tree so a change in any total is deliberate."""
        self.assertEqual(self.s["artifact_total"], 237)
        self.assertEqual(self.s["headline"]["resolved_edges"], 547)
        self.assertEqual(self.s["headline"]["unresolved_edges"], 126)
        self.assertEqual(self.s["construct_total"], 2658)
        self.assertEqual(self.s["orphans"]["total"], 186)
        self.assertEqual({k: self.s["lineage"][k] for k in ("hops", "confirmed", "inferred")},
                         {"hops": 37, "confirmed": 30, "inferred": 7})
        self.assertEqual(self.s["government_decisions"], 17)

    def test_headline_edges_match_edge_list(self):
        edges = self.data["edges"]
        seen = {}
        for e in edges:
            if e["category"] in bd.HEADLINE_CATEGORIES:
                seen.setdefault((e["category"], e["from"], e["to"], e["status"]), 0)
                seen[(e["category"], e["from"], e["to"], e["status"])] += 1
        resolved = sum(1 for k in seen if k[3] == "resolved")
        unresolved = sum(1 for k in seen if k[3] == "unresolved")
        self.assertEqual(resolved, self.s["headline"]["resolved_edges"])
        self.assertEqual(unresolved, self.s["headline"]["unresolved_edges"])
        self.assertEqual(sum(v["resolved"] for v in self.s["headline"]["by_category"].values()), resolved)
        self.assertEqual(sum(v["unresolved"] for v in self.s["headline"]["by_category"].values()), unresolved)

    def test_headline_in_readme_first_paragraph(self):
        first_para = self.readme.split("\n\n")[1]
        self.assertIn(f"resolved {self.s['headline']['resolved_edges']} distinct dependency edges", first_para)
        self.assertIn(f"could not resolve {self.s['headline']['unresolved_edges']}", first_para)

    def test_readme_numbers_table_matches_json(self):
        rows = {r[0].strip(): r[1] for r in md_table_rows(self.readme, "## Numbers")}
        self.assertEqual(int(rows["Dependency edges resolved (headline)"]), self.s["headline"]["resolved_edges"])
        self.assertEqual(int(rows["Dependency edges unresolved (headline)"]), self.s["headline"]["unresolved_edges"])
        self.assertEqual(int(rows["Artifacts (total)"]), self.s["artifact_total"])
        self.assertEqual(int(rows["Construct-register occurrences"].replace(",", "")), self.s["construct_total"])
        self.assertEqual(int(rows["Government decisions"]), self.s["government_decisions"])
        self.assertTrue(rows["Orphans"].startswith(str(self.s["orphans"]["total"]) + " "))
        lin = self.s["lineage"]
        self.assertEqual(rows["Lineage hops"], f"{lin['hops']} ({lin['confirmed']} Confirmed / {lin['inferred']} Inferred)")
        for label, n in self.s["artifact_counts"].items():
            self.assertEqual(int(rows[bd.TYPE_LABELS[label]]), n, label)

    def test_construct_total_matches_register(self):
        self.assertEqual(len(self.data["constructs"]), self.s["construct_total"])
        reg = md("03-conversion-construct-register.md")
        rows = md_table_rows(reg, "## Counts per construct")
        per_construct = {r[0]: int(r[1].replace(",", "")) for r in rows if r[0].strip("*") != "Total"}
        self.assertEqual(per_construct, dict(self.s["construct_counts"]))
        self.assertEqual(sum(per_construct.values()), self.s["construct_total"])
        # every occurrence carries a path:line citation
        cited = re.findall(r"^\| `app/[^`]+:\d+` \|", reg, flags=re.M)
        self.assertEqual(len(cited), self.s["construct_total"])

    def test_lineage_and_decision_counts_come_from_authored_tables(self):
        per_section = bd.lineage_stats(DISCOVERY / bd.LINEAGE_FILE)
        lin = {"hops": sum(v["hops"] for v in per_section.values()),
               "confirmed": sum(v["Confirmed"] for v in per_section.values()),
               "inferred": sum(v["Inferred"] for v in per_section.values())}
        self.assertEqual(lin, {k: self.s["lineage"][k] for k in ("hops", "confirmed", "inferred")})
        self.assertEqual(lin["confirmed"] + lin["inferred"], lin["hops"])
        self.assertGreater(lin["hops"], 0)
        self.assertEqual(len(per_section), 2)
        self.assertEqual(bd.decision_count(DISCOVERY / bd.DECISIONS_FILE), self.s["government_decisions"])
        self.assertGreater(self.s["government_decisions"], 0)
        lineage_md = md(bd.LINEAGE_FILE)
        self.assertIn(f"| **Total** | **{lin['hops']}** | **{lin['confirmed']}** | **{lin['inferred']}** |", lineage_md)
        self.assertIn(f"**{self.s['government_decisions']} decisions**", md(bd.DECISIONS_FILE))

    def test_orphans_match_tables(self):
        dep = md("02-dependency-map.md")
        self.assertEqual(len(md_table_rows(dep, "### Programs no JCL, CSD, or resolved call references")), self.s["orphans"]["programs"])
        self.assertEqual(len(md_table_rows(dep, "### Copybooks nobody copies")), self.s["orphans"]["copybooks"])
        self.assertEqual(len(md_table_rows(dep, "### Data-bearing datasets no application program opens")), self.s["orphans"]["datasets"])


class TestRealDependencyEdges(unittest.TestCase):
    """One edge per headline category, checked against the actual source lines."""

    @classmethod
    def setUpClass(cls):
        cls.edges = load_json()["edges"]

    def find(self, category, frm, to, status="resolved"):
        hits = [e for e in self.edges if e["category"] == category and e["from"] == frm and e["to"] == to and e["status"] == status]
        self.assertTrue(hits, f"no {status} edge {category} {frm} -> {to}")
        return hits[0]

    def source_line(self, e):
        return (ROOT / e["path"]).read_text(encoding="utf-8", errors="replace").splitlines()[e["line"] - 1]

    def test_jcl_step_to_program_posttran_cbtrn02c(self):
        e = self.find("jclstep->program", "POSTTRAN/STEP15", "CBTRN02C")
        self.assertEqual(e["path"], "app/jcl/POSTTRAN.jcl")
        self.assertIn("PGM=CBTRN02C", self.source_line(e))

    def test_program_to_copybook_cbtrn02c_cvtra05y(self):
        e = self.find("program->copybook", "CBTRN02C", "CVTRA05Y")
        self.assertEqual(e["path"], "app/cbl/CBTRN02C.cbl")
        self.assertRegex(self.source_line(e), r"COPY\s+CVTRA05Y")

    def test_program_to_dataset_cbtrn02c_dalytran(self):
        e = self.find("program->dataset", "CBTRN02C", "AWS.M2.CARDDEMO.DALYTRAN.PS")
        self.assertIn("DALYTRAN", self.source_line(e))
        self.assertIn("read", e["modes"])
        jcl = (ROOT / "app/jcl/POSTTRAN.jcl").read_text(encoding="utf-8")
        self.assertRegex(jcl, r"//DALYTRAN\s+DD\s+DISP=SHR,\s*\n//\s+DSN=AWS\.M2\.CARDDEMO\.DALYTRAN\.PS")

    def test_transaction_to_program_cc00_cosgn00c(self):
        e = self.find("transaction->program", "CC00", "COSGN00C")
        self.assertRegex(self.source_line(e), r"TRANSID\(CC00\)")

    def test_program_to_program_cotrn02c_csutldtc(self):
        e = self.find("program->program", "COTRN02C", "CSUTLDTC")
        self.assertEqual(e["kind"], "static")
        self.assertRegex(self.source_line(e), r"CALL\s+'CSUTLDTC'")

    def targets_at(self, frm, line):
        return sorted(e["to"] for e in self.edges
                      if e["category"] == "program->program" and e["from"] == frm and e["line"] == line
                      and e["status"] == "resolved")

    def test_dynamic_xctl_uses_only_definitions_that_reach_the_statement(self):
        # COPAUS0C.cbl:316 MOVE WS-PGM-AUTH-DTL TO CDEMO-TO-PROGRAM dominates the XCTL at :322;
        # the MOVEs at :192 and :669 sit in other paragraphs and must not produce edges here.
        self.assertEqual(self.targets_at("COPAUS0C", 322), ["COPAUS1C"])
        # COTRN00C.cbl:188 MOVE 'COTRN01C' dominates the EXEC CICS XCTL at :192 ('COSGN00C' is set elsewhere).
        self.assertEqual(self.targets_at("COTRN00C", 192), ["COTRN01C"])
        # RETURN-TO-PREV-SCREEN (:665) sets 'COSGN00C' only conditionally, so the MOVEs before
        # its PERFORM at :237 (WS-PGM-MENU = 'COMEN01C') also reach the XCTL at :674.
        self.assertIn("COMEN01C", self.targets_at("COPAUS0C", 674))
        self.assertIn("COSGN00C", self.targets_at("COPAUS0C", 674))

    def test_move_inside_if_else_is_seen_by_the_parser(self):
        # COACTVWC.cbl:336 MOVE LIT-MENUPGM TO CDEMO-TO-PROGRAM follows an earlier MOVE ... ELSE;
        # the destination list of the earlier MOVE must not swallow it.
        self.assertEqual(self.targets_at("COACTVWC", 349), ["COMEN01C"])
        src = (ROOT / "app/cbl/COACTVWC.cbl").read_text(encoding="utf-8").splitlines()
        self.assertRegex(src[335], r"MOVE LIT-MENUPGM\s+TO CDEMO-TO-PROGRAM")
        self.assertRegex(src[167] + src[168], r"LIT-MENUPGM.*COMEN01C")

    def test_dynamic_edge_detail_names_the_variable(self):
        e = self.find("program->program", "COPAUS0C", "COPAUS1C")
        self.assertEqual(e["kind"], "dynamic")
        self.assertIn("CDEMO-TO-PROGRAM", e["detail"])

    def test_value_from_caller_commarea_is_reported_unresolved(self):
        # COTRTLIC.cbl:605 MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM in the ELSE branch: the
        # commarea field is never given a literal, so the XCTL at :620 is only partly resolved.
        e = self.find("program->program", "COTRTLIC",
                      "EXEC CICS XCTL via CDEMO-TO-PROGRAM (value from outside this program)", status="unresolved")
        self.assertEqual((e["path"], e["line"], e["kind"]), ("app/app-transaction-type-db2/cbl/COTRTLIC.cbl", 620, "dynamic"))
        self.assertIn("COADM01C", self.targets_at("COTRTLIC", 620))
        # COPAUS0C.cbl:322 is dominated by the MOVE at :316, so it gets no such edge.
        unres = [e["to"] for e in self.edges if e["from"] == "COPAUS0C" and e["line"] == 322 and e["status"] == "unresolved"]
        self.assertEqual(unres, [])


class TestInventoryArtifactFields(unittest.TestCase):
    def setUp(self):
        self.arts = {a["path"]: a for a in load_json()["artifacts"]}

    def test_cbtrn02c_record(self):
        a = self.arts["app/cbl/CBTRN02C.cbl"]
        self.assertEqual(a["type"], "cobol_program")
        self.assertEqual(a["name"], "CBTRN02C")
        self.assertEqual(a["subtype"], "batch")
        self.assertEqual(a["line_count"], len((ROOT / a["path"]).read_text(encoding="utf-8").splitlines()))
        self.assertIn("CVTRA05Y", [c["name"] for c in a["copy_targets"]])
        self.assertFalse(a["exec_sql"])
        dalytran = next(f for f in a["files"] if f["ddname"] == "DALYTRAN")
        self.assertEqual(dalytran["modes"], ["read"])
        self.assertEqual([d["dsn"] for d in dalytran["datasets"]], ["AWS.M2.CARDDEMO.DALYTRAN.PS"])

    def test_online_program_records_cics_verbs_and_static_call(self):
        a = self.arts["app/cbl/COTRN02C.cbl"]
        self.assertEqual(a["subtype"], "online")
        self.assertIn("SEND", a["cics_verbs"])
        self.assertIn("RECEIVE", a["cics_verbs"])
        calls = {(c["target"], c["kind"]) for c in a["call_targets"]}
        self.assertIn(("CSUTLDTC", "static"), calls)

    def test_jcl_job_records_driving_programs(self):
        a = self.arts["app/jcl/POSTTRAN.jcl"]
        self.assertEqual(a["type"], "jcl_job")
        steps = {(s["step"], s["driving_program"]) for s in a["jcl_steps"]}
        self.assertIn(("STEP15", "CBTRN02C"), steps)

    def test_call_targets_do_not_leak_internal_offsets(self):
        for a in self.arts.values():
            for c in a.get("call_targets", []):
                self.assertNotIn("offset", c)

    def test_no_hand_typed_count_drift_in_type_labels(self):
        # every type label the generator knows about is either used or absent from the counts table
        used = {a["type"] for a in self.arts.values()}
        for t in used:
            self.assertIn(t, bd.TYPE_LABELS)


class TestGeneratorPortability(unittest.TestCase):
    def test_no_possessive_quantifiers_in_regexes(self):
        """`\\s++`, `)*+`, `]?+` need Python 3.11; the generator must run on 3.10."""
        src = SCRIPT.read_text(encoding="utf-8")
        hit = re.search(r"(\\[sdwSDW.]|[)\]])(\+\+|\*\+|\?\+)", src)
        self.assertIsNone(hit, f"possessive quantifier in a regex: {hit and hit.group(0)}")


class TestStaleDetection(unittest.TestCase):
    def _copy_tree(self, tmp):
        for sub in ("app", "docs/discovery"):
            src = ROOT / sub
            dst = Path(tmp) / sub
            shutil.copytree(src, dst, ignore=shutil.ignore_patterns("__pycache__", ".pytest_cache"))

    def test_check_fails_on_stale_generated_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            self._copy_tree(tmp)
            script = Path(tmp) / "docs/discovery/build_discovery.py"
            target = Path(tmp) / "docs/discovery/01-inventory.md"
            target.write_text(target.read_text(encoding="utf-8") + "\nstale line\n", encoding="utf-8")
            r = subprocess.run([sys.executable, str(script), "--check"], cwd=tmp, capture_output=True, text=True)
            self.assertNotEqual(r.returncode, 0)
            self.assertIn("01-inventory.md", r.stdout + r.stderr)
            # regenerate, then --check is clean again
            r2 = subprocess.run([sys.executable, str(script)], cwd=tmp, capture_output=True, text=True)
            self.assertEqual(r2.returncode, 0, r2.stdout + r2.stderr)
            r3 = subprocess.run([sys.executable, str(script), "--check"], cwd=tmp, capture_output=True, text=True)
            self.assertEqual(r3.returncode, 0, r3.stdout + r3.stderr)

    def test_check_fails_on_stale_marked_block_in_authored_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            self._copy_tree(tmp)
            script = Path(tmp) / "docs/discovery/build_discovery.py"
            target = Path(tmp) / "docs/discovery" / bd.DECISIONS_FILE
            text = target.read_text(encoding="utf-8")
            # add a decision row without refreshing the generated count block
            text = text.rstrip("\n") + "\n| D999 | test row | `app/cbl/CBTRN02C.cbl:1` | none | test |\n"
            target.write_text(text, encoding="utf-8")
            r = subprocess.run([sys.executable, str(script), "--check"], cwd=tmp, capture_output=True, text=True)
            self.assertNotEqual(r.returncode, 0)
            self.assertIn(bd.DECISIONS_FILE, r.stdout + r.stderr)

    def test_check_fails_when_source_changes(self):
        with tempfile.TemporaryDirectory() as tmp:
            self._copy_tree(tmp)
            script = Path(tmp) / "docs/discovery/build_discovery.py"
            prog = Path(tmp) / "app/cbl/CBTRN02C.cbl"
            prog.write_text(prog.read_text(encoding="utf-8") + "       COPY UNUSED1Y.\n", encoding="utf-8")
            r = subprocess.run([sys.executable, str(script), "--check"], cwd=tmp, capture_output=True, text=True)
            self.assertNotEqual(r.returncode, 0)


class TestCitations(unittest.TestCase):
    def test_every_cited_line_exists(self):
        pat = re.compile(r"`(app/[^`:]+):(\d+)(?:-(\d+))?`")
        for name in ("01-inventory.md", "02-dependency-map.md", "03-conversion-construct-register.md",
                     bd.LINEAGE_FILE, bd.DECISIONS_FILE):
            text = md(name)
            for m in pat.finditer(text):
                path = ROOT / m.group(1)
                self.assertTrue(path.is_file(), f"{name}: {m.group(0)} path missing")
                n = len(path.read_text(encoding="utf-8", errors="replace").splitlines())
                last = int(m.group(3) or m.group(2))
                self.assertLessEqual(int(m.group(2)), n, f"{name}: {m.group(0)} beyond end of file ({n} lines)")
                self.assertLessEqual(last, n, f"{name}: {m.group(0)} beyond end of file ({n} lines)")


if __name__ == "__main__":
    unittest.main()
