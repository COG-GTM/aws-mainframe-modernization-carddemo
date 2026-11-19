#!/usr/bin/env python3
"""JCL / source cross-reference lint.

  * every EXEC PGM= in app/jcl and app/proc names a program in app/cbl or
    app/asm, or a known z/OS system utility;
  * every EXEC PROC= (or EXEC <name>) resolves to app/proc or an in-stream
    PROC in the same member;
  * every COPY in app/cbl and the copybooks resolves to app/cpy or
    app/cpy-bms (IBM-supplied CICS copybooks excepted);
  * every static CALL 'literal' in app/cbl resolves to a program in
    app/cbl / app/asm or a Language Environment service.
Run: python3 tests/static/test_references.py -v
"""
import re
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
APP = REPO / "app"
JCL_DIRS = [APP / "jcl", APP / "proc"]
SRC_DIRS = [APP / "cbl", APP / "asm"]
CPY_DIRS = [APP / "cpy", APP / "cpy-bms"]

SYSTEM_PROGRAMS = {
    "IDCAMS", "SORT", "IEFBR14", "IEBGENER", "IEBCOPY", "SDSF", "IKJEFT01",
    "IKJEFT1B", "FTP", "DFHCSDUP", "DFSRRC00", "DSNTIAUL", "DSNUTILB",
    "ICETOOL", "IRXJCL",
}
IBM_COPYBOOKS = {"DFHAID", "DFHBMSCA", "DFHEIBLK", "SQLCA"}
LE_SERVICES = {"CEE3ABD", "CEEDAYS", "CEELOCT", "CEEGMT", "CEEDATE"}


def members(dirs, exts=None):
    out = {}
    for d in dirs:
        for p in d.iterdir():
            if p.is_file() and (exts is None or p.suffix.lower() in exts):
                out.setdefault(p.stem.upper(), p)
    return out


def jcl_statements(path):
    """Yield (lineno, text) for non-comment JCL statements."""
    for no, line in enumerate(path.read_text(errors="replace").splitlines(), 1):
        if line.startswith("//*") or not line.startswith("//"):
            continue
        yield no, line[:72]


EXEC_RE = re.compile(r"^//\S*\s+EXEC\s+(?:(PGM|PROC)=)?([A-Z0-9@#$]+)", re.I)
PROC_DEF_RE = re.compile(r"^//([A-Z0-9@#$]+)\s+PROC\b", re.I)
COPY_RE = re.compile(r"^.{6}[ ]+.*?\bCOPY\s+['\"]?([A-Z0-9-]+)", re.I)
CALL_RE = re.compile(r"\bCALL\s+'([A-Z0-9-]+)'", re.I)


def cobol_code_lines(path):
    for no, line in enumerate(path.read_text(errors="replace").splitlines(), 1):
        if len(line) > 6 and line[6] in "*/":
            continue
        yield no, line[:72]


class TestJclReferences(unittest.TestCase):
    def test_exec_references_resolve(self):
        programs = members(SRC_DIRS)
        procs = members([APP / "proc"])
        problems, checked = [], 0
        for d in JCL_DIRS:
            for path in sorted(d.iterdir()):
                stmts = list(jcl_statements(path))
                instream = {m.group(1).upper() for _, t in stmts if (m := PROC_DEF_RE.match(t))}
                for no, text in stmts:
                    m = EXEC_RE.match(text)
                    if not m:
                        continue
                    kind, name = (m.group(1) or "PROC").upper(), m.group(2).upper()
                    checked += 1
                    if kind == "PGM":
                        ok = name in programs or name in SYSTEM_PROGRAMS
                    else:
                        ok = name in procs or name in instream
                    if not ok:
                        problems.append(f"{path.relative_to(REPO)}:{no}: EXEC {kind}={name} not found")
        self.assertGreater(checked, 50)
        self.assertEqual(problems, [], "\n" + "\n".join(problems))

    def test_batch_programs_are_driven_by_jcl(self):
        """Every CB* batch program in app/cbl is executed by some JCL.
        Known gap: CBTRN01C (transaction validation) has no JCL member."""
        known = {"CBTRN01C"}
        jcl_text = "\n".join(t for d in JCL_DIRS for p in d.iterdir()
                             for _, t in jcl_statements(p))
        called = set()
        for p in (APP / "cbl").iterdir():
            called.update(m.upper() for m in CALL_RE.findall(p.read_text(errors="replace")))
        missing = []
        for p in sorted((APP / "cbl").iterdir()):
            name = p.stem.upper()
            if name.startswith("CB") and name not in called and name not in known and \
                    not re.search(rf"PGM={name}\b", jcl_text, re.I):
                missing.append(name)
        self.assertEqual(missing, [], "batch programs with no JCL: %s" % missing)


class TestSourceReferences(unittest.TestCase):
    def test_copy_statements_resolve(self):
        copybooks = members(CPY_DIRS)
        problems, checked = [], 0
        for d in [APP / "cbl", *CPY_DIRS]:
            for path in sorted(d.iterdir()):
                for no, line in cobol_code_lines(path):
                    m = COPY_RE.match(line)
                    if not m:
                        continue
                    name = m.group(1).upper()
                    checked += 1
                    if name not in copybooks and name not in IBM_COPYBOOKS:
                        problems.append(f"{path.relative_to(REPO)}:{no}: COPY {name} not found")
        self.assertGreater(checked, 100)
        self.assertEqual(problems, [], "\n" + "\n".join(problems))

    def test_static_calls_resolve(self):
        programs = members(SRC_DIRS)
        problems = []
        for path in sorted((APP / "cbl").iterdir()):
            for no, line in cobol_code_lines(path):
                for name in CALL_RE.findall(line):
                    name = name.upper()
                    if name not in programs and name not in LE_SERVICES:
                        problems.append(f"{path.relative_to(REPO)}:{no}: CALL '{name}' not found")
        self.assertEqual(problems, [], "\n" + "\n".join(problems))

    def test_no_tabs_in_cobol_source(self):
        """TABs are not valid in z/OS fixed-format source; report them.
        (Known: CUSTREC.cpy, CSLKPCDY.cpy - the harness compiles with
        -ftab-width=4.)  New files with TABs fail this test."""
        known = {"CUSTREC.cpy", "CSLKPCDY.cpy"}
        offenders = sorted(p.name for d in [APP / "cbl", *CPY_DIRS] for p in d.iterdir()
                           if "\t" in p.read_text(errors="replace"))
        self.assertEqual([o for o in offenders if o not in known], [])


if __name__ == "__main__":
    unittest.main()
