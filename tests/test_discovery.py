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
        self.assertEqual(self.s["headline"]["resolved_edges"], 548)
        self.assertEqual(self.s["headline"]["unresolved_edges"], 125)
        self.assertEqual(self.s["construct_total"], 2658)
        self.assertEqual(self.s["orphans"]["total"], 185)
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

    def test_program_to_bmsmap_cosgn00c_cosgn00(self):
        # COSGN00C.cbl:151-153 EXEC CICS SEND / MAP('COSGN0A') / MAPSET('COSGN00') and :110 RECEIVE:
        # the option sits on the line after the verb and the map qualifier has no space before '('.
        hits = {e["line"]: e for e in self.edges if e["category"] == "program->bmsmap"
                and e["from"] == "COSGN00C" and e["to"] == "COSGN00" and e["status"] == "resolved"}
        self.assertEqual({151: "SEND MAP MAPSET COSGN00", 110: "RECEIVE MAP MAPSET COSGN00"},
                         {line: e["detail"] for line, e in hits.items()})
        self.assertRegex(self.source_line(hits[151]), r"EXEC CICS SEND\s*$")
        self.assertRegex(self.source_line(hits[110]), r"EXEC CICS RECEIVE\s*$")
        self.assertEqual(len({(e["from"], e["to"]) for e in self.edges
                              if e["category"] == "program->bmsmap" and e["status"] == "resolved"}), 21)

    def test_calls_inside_procedural_copybooks_belong_to_the_including_program(self):
        # CSUTLDPY.cpy:293 CALL 'CSUTLDTC' is procedure code COPYed into COACTUPC
        e = self.find("program->program", "COACTUPC", "CSUTLDTC")
        self.assertEqual((e["kind"], e["path"]), ("static", "app/cpy/CSUTLDPY.cpy"))
        self.assertRegex(self.source_line(e), r"CALL\s+'CSUTLDTC'")
        # CSDB2RPY.cpy:57 CALL LIT-DSNTIAC resolves through COTRTLIC's own VALUE 'DSNTIAC'
        e = self.find("program->program", "COTRTLIC", "DSNTIAC", status="unresolved")
        self.assertEqual((e["kind"], e["path"]), ("dynamic", "app/app-transaction-type-db2/cpy/CSDB2RPY.cpy"))
        self.assertRegex(self.source_line(e), r"CALL\s+LIT-DSNTIAC")
        arts = {a["path"]: a for a in load_json()["artifacts"]}
        carried = [c for c in arts["app/app-transaction-type-db2/cbl/COTRTLIC.cbl"]["call_targets"]
                   if c.get("included_from") == "CSDB2RPY"]
        self.assertEqual([(c["via"], c["path"], c["line"]) for c in carried],
                         [("LIT-DSNTIAC", "app/app-transaction-type-db2/cpy/CSDB2RPY.cpy", 57)])

    def test_csd_library_dsnames_join_the_dataset_inventory(self):
        # standalone CSD member and the DFHCSDUP input embedded in a JCL job (symbolic &HLQ)
        for path, line in (("app/csd/CARDDEMO.CSD", 494), ("app/jcl/CBADMCDJ.jcl", 44)):
            hits = [e for e in self.edges if e["category"] == "csdlibrary->dataset" and e["from"] == "COM2DOLL"
                    and e["to"] == "AWS.M2.CARDDEMO.LOADLIB" and e["path"] == path]
            self.assertEqual([e["line"] for e in hits], [line])
            self.assertRegex(self.source_line(hits[0]), r"DEFINE LIBRARY\(COM2DOLL\)")
        by_dsn = {d["dsn"]: d for d in load_json()["datasets"]}
        self.assertNotIn("&HLQ..LOADLIB", by_dsn)
        self.assertIn("CSD LIBRARY", by_dsn["AWS.M2.CARDDEMO.LOADLIB"]["reference_sources"])
        self.assertEqual(sorted({c["library"] for c in by_dsn["AWS.M2.CARDDEMO.LOADLIB"]["csd_libraries"]}),
                         ["CARDDLIB", "COM2DOLL"])

    def test_assembler_source_links_to_repository_macros(self):
        # COBDATFT.asm:66  COPY COCDATFT      MVSWAIT.asm:23  ASMWAIT BINLBL (macro instruction)
        hits = sorted(((e["from"], e["to"], e["path"], e["line"], e["status"], e["detail"])
                       for e in self.edges if e["category"] == "assembler->macro"))
        self.assertEqual(hits, [
            ("COBDATFT", "COCDATFT", "app/asm/COBDATFT.asm", 66, "resolved", "COPY"),
            ("MVSWAIT", "ASMWAIT", "app/asm/MVSWAIT.asm", 23, "resolved", "macro instruction"),
        ])
        for e in self.edges:
            if e["category"] == "assembler->macro":
                self.assertRegex(self.source_line(e), r"(COPY\s+COCDATFT|\bASMWAIT\b)")
        orphan_section = md("02-dependency-map.md").split("### Copybooks nobody copies", 1)[1].split("### ", 1)[0]
        self.assertNotRegex(orphan_section, r"ASMWAIT|COCDATFT")
        self.assertIn("UNUSED1Y", orphan_section)
        # the COBOL callers of the two Assembler modules stay resolved program->program edges
        self.assertEqual(self.find("program->program", "COBSWAIT", "MVSWAIT")["line"], 38)
        self.assertEqual(self.find("program->program", "CBACT01C", "COBDATFT")["line"], 231)

    def test_nested_jcl_symbols_resolve_to_a_fixed_point(self):
        # CREADB21.jcl:28-29  SET CODER=AWS / SET LBNM=&CODER..M2.CARDDEMO
        sets = {"CODER": "AWS", "LBNM": "&CODER..M2.CARDDEMO"}
        self.assertEqual(bd.resolve_symbolics("&LBNM..CNTL(DB2FREE)", sets), "AWS.M2.CARDDEMO.CNTL(DB2FREE)")
        self.assertEqual(bd.resolve_symbolics("&LOOP..X", {"LOOP": "&LOOP..Y"}).count("&LOOP"), 1)
        by_dsn = {d["dsn"] for d in load_json()["datasets"]}
        self.assertIn("AWS.M2.CARDDEMO.CNTL(DB2FREE)", by_dsn)
        self.assertFalse([d for d in by_dsn if d.startswith("&CODER")], "unexpanded nested symbol")

    def test_relative_gdg_generation_survives_dsn_normalisation(self):
        self.assertEqual(bd.split_gdg("AWS.M2.CARDDEMO.TRANTYPE.BKUP(+1)"), ("AWS.M2.CARDDEMO.TRANTYPE.BKUP", "+1"))
        self.assertEqual(bd.split_gdg("'aws.m2.carddemo.systran(0)'"), ("AWS.M2.CARDDEMO.SYSTRAN", "0"))
        self.assertEqual(bd.split_gdg("AWS.M2.CARDDEMO.CNTL(DB2FREE)"), ("AWS.M2.CARDDEMO.CNTL(DB2FREE)", None))
        by_dsn = {d["dsn"]: d for d in load_json()["datasets"]}
        # DEFGDGD.jcl:40 names TRANTYPE.BKUP(+1); the base is in no catalog listing, so the
        # relative generation is the only evidence that it is a GDG
        d = by_dsn["AWS.M2.CARDDEMO.TRANTYPE.BKUP"]
        self.assertEqual((d["kind"], d["catalog_types"], d["relative_generations"]), ("GDG / sequential", [], ["+1"]))
        self.assertNotIn("AWS.M2.CARDDEMO.TRANTYPE.BKUP(+1)", by_dsn)
        self.assertEqual(by_dsn["AWS.M2.CARDDEMO.SYSTRAN"]["relative_generations"], ["0", "+1"])
        # an absolute generation from the catalog listing is a GDG generation, not a plain file
        self.assertEqual(by_dsn["AWS.M2.CARDDEMO.SYSTRAN.G0018V00"]["kind"], "GDG / sequential")
        for d in by_dsn.values():
            if d["relative_generations"]:
                self.assertEqual(d["kind"], "GDG / sequential", d["dsn"])

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

    def test_table_driven_menu_xctl_resolves_to_the_initialised_table_entries(self):
        # COMEN01C.cbl:156 XCTL PROGRAM(CDEMO-MENU-OPT-PGMNAME(WS-OPTION)); the table is
        # COMEN02Y.cpy:94 CDEMO-MENU-OPT OCCURS 12 REDEFINES CDEMO-MENU-OPTIONS-DATA, whose
        # VALUE clauses initialise 11 entries and CDEMO-MENU-OPT-COUNT VALUE 11 bounds the lookup.
        menu = ["COACTUPC", "COACTVWC", "COBIL00C", "COCRDLIC", "COCRDSLC", "COCRDUPC",
                "COPAUS0C", "CORPT00C", "COTRN00C", "COTRN01C", "COTRN02C"]
        self.assertEqual(self.targets_at("COMEN01C", 156), menu)
        e = self.find("program->program", "COMEN01C", "COBIL00C")
        self.assertEqual(e["kind"], "dynamic")
        self.assertIn("CDEMO-MENU-OPT-PGMNAME", e["detail"])
        self.assertIn("app/cpy/COMEN02Y.cpy:94", e["detail"])
        self.assertIn("CDEMO-MENU-OPT-COUNT VALUE 11", e["detail"])
        # COADM01C.cbl:145 uses COADM02Y.cpy:56 CDEMO-ADMIN-OPT OCCURS 9 with 6 initialised entries.
        self.assertEqual(self.targets_at("COADM01C", 145),
                         ["COTRTLIC", "COTRTUPC", "COUSR00C", "COUSR01C", "COUSR02C", "COUSR03C"])
        # Every selectable entry is a VALUE literal, so neither statement is partly unresolved,
        # and no program-name literal from an unrelated copybook (e.g. COSGN00C) leaks in.
        for prog, line in (("COMEN01C", 156), ("COMEN01C", 184), ("COADM01C", 145)):
            self.assertEqual([e["to"] for e in self.edges
                              if e["from"] == prog and e["line"] == line and e["status"] == "unresolved"], [])
            self.assertNotIn("COSGN00C", self.targets_at(prog, line))


class TestInventoryArtifactFields(unittest.TestCase):
    def setUp(self):
        self.inv = load_json()
        self.arts = {a["path"]: a for a in self.inv["artifacts"]}

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
        # COTRN02C.cbl:522-523 EXEC CICS SEND / MAP('COTRN2A') and :541-542 RECEIVE / MAP('COTRN2A')
        self.assertEqual((a["cics_verbs"]["SEND MAP"], a["cics_verbs"]["RECEIVE MAP"]), (1, 1))
        self.assertNotIn("SEND", a["cics_verbs"])
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

    def test_exec_proc_invocation_overrides_bind_the_procedure_datasets(self):
        # TRANBKP/STEP05R overrides PRC001.FILEIN and PRC001.FILEOUT of REPROC; the procedure's own
        # SYSIN resolves through the invocation's CNTLLIB symbolic.
        step = self.arts["app/jcl/TRANBKP.jcl"]["jcl_steps"][0]
        self.assertEqual(("STEP05R", "PROC REPROC"), (step["step"], step["driving_program"]))
        self.assertEqual(
            [("PRC001", "FILEIN", "AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS", "app/jcl/TRANBKP.jcl"),
             ("PRC001", "FILEOUT", "AWS.M2.CARDDEMO.TRANSACT.BKUP", "app/jcl/TRANBKP.jcl"),
             ("PRC001", "SYSIN", "AWS.M2.CARDDEMO.CNTL(REPROCT)", "app/proc/REPROC.prc")],
            [(b["proc_step"], b["ddname"], b["dsn"], b["path"]) for b in step["proc_bindings"]],
        )
        by_dsn = {d["dsn"]: d for d in self.inv["datasets"]}
        refs = {(r["member"], r["step"], r["ddname"]) for r in by_dsn["AWS.M2.CARDDEMO.TRANSACT.BKUP"]["jcl_refs"]}
        self.assertIn(("TRANBKP", "STEP05R", "PRC001.FILEOUT"), refs)
        prt = self.arts["app/jcl/PRTCATBL.jcl"]["jcl_steps"]
        proc_step = next(s for s in prt if s["driving_program"] == "PROC REPROC")
        self.assertEqual({"AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS", "AWS.M2.CARDDEMO.TCATBALF.BKUP", "AWS.M2.CARDDEMO.CNTL(REPROCT)"},
                         {b["dsn"] for b in proc_step["proc_bindings"]})
        for s in prt:
            if s is not proc_step:
                self.assertEqual([], s["proc_bindings"])

    def test_dependency_map_rows_for_same_named_steps_are_distinct(self):
        rows = md_table_rows(md("02-dependency-map.md"), "## Batch: JCL job → step → program → copybooks → datasets")
        by_source = {r[2]: r for r in rows}
        self.assertIn("AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS (unknown; PRC001.FILEIN)", by_source["`app/jcl/TRANBKP.jcl:23`"][7])
        self.assertIn("AWS.M2.CARDDEMO.TCATBALF.BKUP (unknown; PRC001.FILEOUT)", by_source["`app/jcl/PRTCATBL.jcl:29`"][7])
        # TRANREPT names both its EXEC PROC step and its SORT step STEP05R
        proc_row, sort_row = by_source["`app/jcl/TRANREPT.jcl:23`"], by_source["`app/jcl/TRANREPT.jcl:37`"]
        self.assertEqual(("TRANREPT", "STEP05R", "PROC REPROC [procedure]"), (proc_row[0], proc_row[1], proc_row[3]))
        self.assertEqual(("TRANREPT", "STEP05R", "SORT [utility]"), (sort_row[0], sort_row[1], sort_row[3]))
        self.assertEqual({"AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS (unknown; PRC001.FILEIN)",
                          "AWS.M2.CARDDEMO.TRANSACT.BKUP (unknown; PRC001.FILEOUT)",
                          "AWS.M2.CARDDEMO.CNTL(REPROCT) (unknown; PRC001.SYSIN)"}, set(proc_row[7].split("<br>")))
        self.assertEqual({"AWS.M2.CARDDEMO.TRANSACT.BKUP (unknown)", "AWS.M2.CARDDEMO.TRANSACT.DALY (unknown)"},
                         set(sort_row[7].split("<br>")))
        # DEFCUST runs IDCAMS twice under the same step name; each row shows only its own datasets
        first, second = by_source["`app/jcl/DEFCUST.jcl:22`"], by_source["`app/jcl/DEFCUST.jcl:32`"]
        self.assertEqual(["AWS.CCDA.CUSTDATA.CLUSTER (unknown)"], first[7].split("<br>"))
        self.assertNotIn("AWS.CCDA.CUSTDATA.CLUSTER", second[7])
        self.assertIn("AWS.CUSTDATA.CLUSTER.INDEX (unknown)", second[7].split("<br>"))

    def test_sample_data_file_joins_the_dataset_its_jcl_names(self):
        # the .dat extension is not a dataset qualifier; .PS / .INIT are
        self.assertEqual(bd.sample_file_dsn("AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat"), "AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0")
        self.assertEqual(bd.sample_file_dsn("AWS.M2.CARDDEMO.DALYTRAN.PS.INIT"), "AWS.M2.CARDDEMO.DALYTRAN.PS.INIT")
        self.assertEqual(bd.sample_file_dsn("AWS.M2.CARDDEMO.ACCTDATA.PS"), "AWS.M2.CARDDEMO.ACCTDATA.PS")
        by_dsn = {d["dsn"]: d for d in self.inv["datasets"]}
        self.assertNotIn("AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.DAT", by_dsn)
        ims = by_dsn["AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0"]
        self.assertEqual(ims["sample_files"], ["app/app-authorization-ims-db2-mq/data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat"])
        self.assertTrue(ims["jcl_refs"])

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

    def test_check_passes_under_python_3_10_interpreter(self):
        """Run the generator's --check under a real 3.10 interpreter when one is installed."""
        exe = shutil.which("python3.10")
        if not exe:
            self.skipTest("no python3.10 interpreter on PATH")
        r = subprocess.run([exe, str(SCRIPT), "--check"], cwd=ROOT, capture_output=True, text=True)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)


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

    DATA_ITEM_CONSTRUCTS = (
        "COMP-3 / packed decimal", "COMP / binary", "Signed zoned decimal (PIC S9)",
        "Edited numeric picture", "OCCURS ... DEPENDING ON", "Hard-coded date",
        "Hard-coded amount / numeric literal",
    )

    def test_data_item_constructs_cite_the_line_that_declares_the_item(self):
        # A data item usually starts a line or more after the period that ends the previous one; the
        # cited line and snippet must belong to the item named in the detail, not to its predecessor.
        sources = {}
        checked = 0
        for c in load_json()["constructs"]:
            if c["construct"] not in self.DATA_ITEM_CONSTRUCTS or not c["path"].endswith((".cbl", ".cpy", ".CBL", ".CPY")):
                continue
            name = c["detail"].split()[0]
            if c["path"] not in sources:
                sources[c["path"]] = (ROOT / c["path"]).read_text(encoding="latin-1").splitlines()
            cited = sources[c["path"]][c["line"] - 1].upper()
            word = re.compile(r"(?<![A-Z0-9-])" + re.escape(name) + r"(?![A-Z0-9-])")
            self.assertRegex(cited, word, f"{c['path']}:{c['line']} does not declare {name}: {cited.strip()!r}")
            self.assertRegex(c["snippet"].upper(), word, f"{c['path']}:{c['line']} snippet is not {name}: {c['snippet']!r}")
            checked += 1
        self.assertGreater(checked, 900)


class TestWording(unittest.TestCase):
    """The dossier may reproduce source identifiers verbatim (dataset qualifiers, field prefixes,
    the repository name) but must never use the word itself as prose."""

    WORD = "de" + "mo"  # assembled so this file passes its own check
    STANDALONE = re.compile(r"(?<![A-Za-z0-9_-])" + WORD + r"(?![A-Za-z0-9_-])", re.IGNORECASE)

    def test_no_standalone_word_in_dossier_or_tooling(self):
        files = sorted((ROOT / "docs/discovery").glob("*")) + [Path(__file__)]
        for f in files:
            if not f.is_file():
                continue
            for i, line in enumerate(f.read_text(encoding="utf-8").splitlines(), 1):
                self.assertIsNone(self.STANDALONE.search(line), f"{f.relative_to(ROOT)}:{i}: {line.strip()[:120]}")


def cobol(*lines):
    """Fixed-format COBOL text: 6-column sequence area, indicator column, then the code."""
    return "".join(f"      {l}\n" for l in lines)


class TestParserFixtures(unittest.TestCase):
    """Exact facts each parser extracts from small synthetic sources. Pinned totals
    over the real estate catch broad drift; these catch a parser that starts
    accepting one syntax variant while silently dropping another."""

    def parse_cobol(self, *lines):
        return bd.CobolProgram(bd.CobolSource(bd.ROOT / "app" / "cbl" / "FIXTURE.cbl", cobol(*lines)))

    def test_cobol_copy_call_select_and_file_verbs(self):
        p = self.parse_cobol(
            " IDENTIFICATION DIVISION.",
            " PROGRAM-ID. FIXTURE.",
            " ENVIRONMENT DIVISION.",
            " INPUT-OUTPUT SECTION.",
            " FILE-CONTROL.",
            "     SELECT TRAN-FILE ASSIGN TO UT-S-TRANFILE.",
            "     SELECT OPTIONAL ACCT-FILE ASSIGN TO ACCTFILE.",
            " DATA DIVISION.",
            " FILE SECTION.",
            " FD  TRAN-FILE.",
            " 01  TRAN-REC                  PIC X(350).",
            " FD  ACCT-FILE.",
            " 01  ACCT-REC                  PIC X(300).",
            " WORKING-STORAGE SECTION.",
            "     COPY CVTRA05Y.",
            "     COPY 'CVACT01Y'.",
            "     COPY CSUTLDPY REPLACING ==:TAG:== BY ==WS==.",
            "*    COPY NOTREAL.",
            " 01  WS-PGM                    PIC X(8) VALUE 'CBSTM03B'.",
            " 01  WS-MSG                    PIC X(20) VALUE 'CALL NOTAPGM HERE'.",
            " PROCEDURE DIVISION.",
            "     OPEN INPUT TRAN-FILE OUTPUT ACCT-FILE.",
            "     READ TRAN-FILE.",
            "     WRITE ACCT-REC.",
            "     REWRITE ACCT-REC.",
            "     DELETE ACCT-FILE.",
            "     CALL 'CSUTLDTC' USING WS-MSG.",
            "     CALL WS-PGM.",
            "     CLOSE TRAN-FILE ACCT-FILE.",
        )
        self.assertEqual(p.program_id, "FIXTURE")
        self.assertEqual(
            [(c["name"], c["line"], c["replacing"]) for c in p.copies],
            [("CVTRA05Y", 15, False), ("CVACT01Y", 16, False), ("CSUTLDPY", 17, True)],
        )
        self.assertEqual(
            [(s["file"], s["ddname"], s["line"]) for s in p.selects],
            [("TRAN-FILE", "TRANFILE", 6), ("ACCT-FILE", "ACCTFILE", 7)],
        )
        self.assertEqual(p.fd_records, {"TRAN-REC": "TRAN-FILE", "ACCT-REC": "ACCT-FILE"})
        self.assertEqual(
            [(c["target"], c["kind"], c["via"], c["line"]) for c in p.calls],
            [("CSUTLDTC", "static", None, 27), (None, "dynamic", "WS-PGM", 28)],
        )
        self.assertEqual(p.values["WS-PGM"], [("CBSTM03B", 19)])
        self.assertEqual(dict(p.file_modes), {
            "TRAN-FILE": {"open input", "read"},
            "ACCT-FILE": {"open output", "write", "rewrite", "delete"},
        })

    def test_cobol_cics_and_sql_statements(self):
        p = self.parse_cobol(
            " IDENTIFICATION DIVISION.",
            " PROGRAM-ID. FIXTURE2.",
            " DATA DIVISION.",
            " WORKING-STORAGE SECTION.",
            " 01  WS-MAP                    PIC X(7) VALUE 'COSGN0A'.",
            " PROCEDURE DIVISION.",
            "     EXEC CICS SEND MAP(WS-MAP) MAPSET('COSGN00')",
            "          ERASE END-EXEC.",
            "     EXEC CICS READ DATASET('ACCTDAT') INTO(WS-REC)",
            "          RIDFLD(WS-KEY) END-EXEC.",
            "     EXEC CICS XCTL PROGRAM('COMEN01C') END-EXEC.",
            "     EXEC SQL",
            "          SELECT TR_TYPE INTO :WS-TYPE FROM TRANSACTION_TYPE",
            "     END-EXEC.",
        )
        self.assertTrue(p.has_cics)
        self.assertTrue(p.has_sql)
        self.assertEqual([(c["verb"], c["line"]) for c in p.cics],
                         [("SEND MAP", 7), ("READ", 9), ("XCTL", 11)])
        send, read, xctl = p.cics
        self.assertEqual(send["options"]["MAP"], {"literal": None, "var": "WS-MAP"})
        self.assertEqual(send["options"]["MAPSET"], {"literal": "COSGN00", "var": None})
        self.assertEqual(read["options"]["DATASET"], {"literal": "ACCTDAT", "var": None})
        self.assertEqual(xctl["options"]["PROGRAM"], {"literal": "COMEN01C", "var": None})
        self.assertEqual([(s["statement"], s["line"]) for s in p.sql], [("SELECT", 12)])

    def test_cics_two_word_verbs_with_and_without_space_before_paren(self):
        p = self.parse_cobol(
            " PROCEDURE DIVISION.",
            "     EXEC CICS SEND MAP ('COSGN0A') MAPSET ('COSGN00') END-EXEC.",
            "     EXEC CICS SEND MAP('COSGN0A') MAPSET('COSGN00') END-EXEC.",
            "     EXEC CICS RECEIVE MAP('COSGN0A') INTO(WS-MAP) END-EXEC.",
            "     EXEC CICS SEND TEXT FROM(WS-MSG) ERASE END-EXEC.",
            "     EXEC CICS SEND CONTROL ERASE FREEKB END-EXEC.",
            "     EXEC CICS SEND FROM(WS-MSG) LENGTH(10) END-EXEC.",
            "     EXEC CICS HANDLE ABEND LABEL(ABEND-PARA) END-EXEC.",
            "     EXEC CICS HANDLE CONDITION NOTFND(NF-PARA) END-EXEC.",
        )
        self.assertEqual([c["verb"] for c in p.cics],
                         ["SEND MAP", "SEND MAP", "RECEIVE MAP", "SEND TEXT", "SEND CONTROL", "SEND",
                          "HANDLE ABEND", "HANDLE CONDITION"])
        self.assertEqual([c["options"].get("MAP", {}).get("literal") for c in p.cics[:3]], ["COSGN0A"] * 3)
        self.assertEqual(p.cics[5]["options"]["FROM"], {"literal": None, "var": "WS-MSG"})

    def test_picture_bytes_and_group_layout(self):
        self.assertEqual([bd.picture_bytes(pic, None) for pic in ("X(08)", "9(02)", "S9(7)V99", "X", "ZZ,ZZ9.99-", "S9(5)V99CR")],
                         [8, 2, 9, 1, 10, 9])
        self.assertEqual(bd.picture_bytes("S9(9)V99", "COMP-3"), 6)
        self.assertEqual([bd.picture_bytes("S9(4)", "COMP"), bd.picture_bytes("S9(9)", "COMP"), bd.picture_bytes("S9(18)", "BINARY")],
                         [2, 4, 8])
        p = self.parse_cobol(
            " DATA DIVISION.",
            " WORKING-STORAGE SECTION.",
            " 01  WS-REC.",
            "     05  WS-A                  PIC X(03).",
            "     05  WS-B                  PIC 9(02) OCCURS 4 TIMES.",
            "     05  WS-C REDEFINES WS-A.",
            "         10 WS-C1              PIC X(01).",
            "         10 WS-C2              PIC X(02).",
            "     05  WS-D.",
            "         10 WS-D1              PIC S9(3) COMP-3.",
            "         88 WS-D1-ZERO         VALUE 0.",
            "         10 WS-D2              PIC X(05).",
            " 01  WS-OTHER                  PIC X(01).",
        )
        fields, size = bd.data_layout(p.data_items, 0)
        self.assertEqual([(f["item"]["name"], f["offset"], f["size"]) for f in fields],
                         [("WS-A", 0, 3), ("WS-B", 3, 2), ("WS-D1", 11, 2), ("WS-D2", 13, 5)])
        self.assertEqual(size, 18)

    def table_program(self, *value_lines, count_line=None, occurs=3):
        return self.parse_cobol(
            " IDENTIFICATION DIVISION.",
            " PROGRAM-ID. MENUFIX.",
            " DATA DIVISION.",
            " WORKING-STORAGE SECTION.",
            " 01  WS-OPTION                 PIC 9(02).",
            " 01  WS-MENU-DATA.",
            *([f"     05  MENU-OPT-COUNT        PIC 9(02) VALUE {count_line}."] if count_line else []),
            "     05  MENU-OPTIONS-DATA.",
            *value_lines,
            f"     05  MENU-OPTIONS REDEFINES MENU-OPTIONS-DATA.",
            f"         10 MENU-OPT OCCURS {occurs} TIMES.",
            "             15 MENU-OPT-NUM       PIC 9(02).",
            "             15 MENU-OPT-NAME      PIC X(10).",
            "             15 MENU-OPT-PGMNAME   PIC X(08).",
            " 01  WS-UNRELATED              PIC X(08) VALUE 'COSGN00C'.",
            " PROCEDURE DIVISION.",
            "     EXEC CICS XCTL PROGRAM(MENU-OPT-PGMNAME(WS-OPTION)) END-EXEC.",
        )

    def table_values(self, prog):
        est = bd.Estate.__new__(bd.Estate)
        est.cobol = {prog.src.rel: prog}
        est.copybooks = {}
        return est.table_values({"path": prog.src.rel, "copy_targets": []}, "MENU-OPT-PGMNAME(WS-OPTION)")

    def test_table_values_come_from_the_redefined_value_clauses_only(self):
        entry = lambda n, name, pgm: [  # noqa: E731
            f"         10 FILLER                PIC 9(02) VALUE {n}.",
            f"         10 FILLER                PIC X(10) VALUE '{name}'.",
            f"         10 FILLER                PIC X(08) VALUE '{pgm}'.",
        ]
        full = self.table_values(self.table_program(*entry(1, "ONE", "COACTVWC"), *entry(2, "TWO", "COACTUPC"),
                                                    *entry(3, "THREE", "COCRDLIC")))
        self.assertEqual([lit for lit, _ in full["literals"]], ["COACTVWC", "COACTUPC", "COCRDLIC"])
        self.assertEqual((full["table"], full["occurs"], full["initialised"], full["count_field"], full["complete"]),
                         ("MENU-OPT", 3, 3, None, True))
        # Two of three entries initialised and no count field: the third entry can hold anything.
        partial = self.table_values(self.table_program(*entry(1, "ONE", "COACTVWC"), *entry(2, "TWO", "COACTUPC")))
        self.assertEqual(([lit for lit, _ in partial["literals"]], partial["complete"]), (["COACTVWC", "COACTUPC"], False))
        # The same two entries with MENU-OPT-COUNT VALUE 2 bounding the lookup are complete.
        counted = self.table_values(self.table_program(*entry(1, "ONE", "COACTVWC"), *entry(2, "TWO", "COACTUPC"), count_line=2))
        self.assertEqual((counted["count_field"], counted["complete"]), ("MENU-OPT-COUNT", True))
        # Name literals at the wrong offset (the PIC X(10) slot) never masquerade as programs.
        self.assertNotIn("ONE", [lit for lit, _ in full["literals"]])
        # A reference to a field that is not inside an OCCURS/REDEFINES table yields nothing.
        est = bd.Estate.__new__(bd.Estate)
        prog = self.table_program(*entry(1, "ONE", "COACTVWC"))
        est.cobol, est.copybooks = {prog.src.rel: prog}, {}
        self.assertIsNone(est.table_values({"path": prog.src.rel, "copy_targets": []}, "WS-UNRELATED(1)"))

    def test_jcl_steps_dds_continuations_and_symbols(self):
        text = "\n".join([
            "//FIXJOB   JOB (ACCT),'FIXTURE',CLASS=A",
            "//         SET HLQ=AWS.M2.CARDDEMO",
            "//* comment line",
            "//STEP010  EXEC PGM=CBTRN02C,",
            "//         PARM='2022-01-01'",
            "//DALYTRAN DD  DSN=&HLQ..DALYTRAN.PS,",
            "//             DISP=SHR",
            "//TRANFILE DD  DSN=&HLQ..TRANSACT.BKUP(+1),DISP=(NEW,CATLG)",
            "//SYSIN    DD  *",
            "  DELETE AWS.M2.CARDDEMO.TEMP.PS",
            "/*",
            "//STEP020  EXEC PROC=SORTPROC,OUTFILE=SORTED",
            "//SORTIN   DD  DUMMY",
            "",
        ])
        m = bd.JclMember(bd.ROOT / "app" / "jcl" / "FIXJOB.jcl", text)
        self.assertEqual((m.job_name, m.job_line, m.sets), ("FIXJOB", 1, {"HLQ": "AWS.M2.CARDDEMO"}))
        self.assertEqual(
            [(s["step"], s["pgm"], s["proc"], s["parm"], s["line"]) for s in m.steps],
            [("STEP010", "CBTRN02C", None, "'2022-01-01'", 4), ("STEP020", None, "SORTPROC", None, 12)],
        )
        self.assertEqual(m.steps[1]["symbols"], {"OUTFILE": "SORTED"})
        dds = m.steps[0]["dds"]
        self.assertEqual(
            [(d["ddname"], d["dsn"], d["disp"], d["line"]) for d in dds],
            [("DALYTRAN", "&HLQ..DALYTRAN.PS", "SHR", 6),
             ("TRANFILE", "&HLQ..TRANSACT.BKUP(+1)", "(NEW,CATLG)", 8),
             ("SYSIN", None, None, 9)],
        )
        self.assertEqual(m.steps[0]["instream"], [(10, "  DELETE AWS.M2.CARDDEMO.TEMP.PS")])
        self.assertEqual(bd.resolve_symbolics(dds[0]["dsn"], m.sets), "AWS.M2.CARDDEMO.DALYTRAN.PS")
        self.assertEqual(bd.split_gdg(bd.resolve_symbolics(dds[1]["dsn"], m.sets)),
                         ("AWS.M2.CARDDEMO.TRANSACT.BKUP", "+1"))
        self.assertTrue(m.steps[1]["dds"][0]["dummy"])

    def test_csd_defines_with_continuations_and_sequence_numbers(self):
        lines = list(enumerate([
            "* comment",
            "DEFINE TRANSACTION(CC00) GROUP(CARDDEMO)",
            "       PROGRAM(COSGN00C) TASKDATALOC(ANY)",
            "DEFINE PROGRAM(COSGN00C) GROUP(CARDDEMO) LANGUAGE(COBOL)",
            "DEFINE FILE(ACCTDAT) GROUP(CARDDEMO)" + " " * 36 + "00000010",
            "       DSNAME(AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS)",
            "DEFINE LIBRARY(CARDDLIB) GROUP(CARDDEMO) DSNAME01(AWS.M2.CARDDEMO.LOADLIB)",
            "DELETE PROGRAM(OLDPGM) GROUP(CARDDEMO)",
        ], 1))
        entries = bd.parse_csd(lines)
        self.assertEqual(
            [(e["kind"], e["name"], e["line"]) for e in entries],
            [("TRANSACTION", "CC00", 2), ("PROGRAM", "COSGN00C", 4), ("FILE", "ACCTDAT", 5), ("LIBRARY", "CARDDLIB", 7)],
        )
        self.assertEqual(entries[0]["attrs"]["PROGRAM"], "COSGN00C")
        self.assertEqual(entries[2]["attrs"]["DSNAME"], "AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS")
        self.assertEqual(entries[3]["attrs"]["DSNAME01"], "AWS.M2.CARDDEMO.LOADLIB")

    def test_assembler_csect_copy_and_macro_instructions(self):
        asm = bd.parse_asm("\n".join([
            "* comment line",
            "FIXTURE  CSECT",
            "         STM   R14,R12,12(R13)      * STANDARD ENTRY",
            "         COPY  COCDATFT",
            "LBL      ASMWAIT BINLBL              START INTERVAL CONTROL TIMER",
            "         MYMAC (R2),X'01'",
            "         END   FIXTURE",
        ]))
        self.assertEqual(asm["csects"], [{"name": "FIXTURE", "line": 2}])
        self.assertEqual(asm["copies"], [{"name": "COCDATFT", "line": 4}])
        self.assertEqual([(o["op"], o["line"]) for o in asm["ops"]], [("STM", 3), ("ASMWAIT", 5), ("MYMAC", 6)])


if __name__ == "__main__":
    unittest.main()
