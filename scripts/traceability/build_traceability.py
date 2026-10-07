#!/usr/bin/env python3
"""Build the CardDemo migration traceability matrix (COBOL/CICS/JCL/VSAM -> Java 21).

Reads the legacy sources under app/, the inventory (docs/modernization/inventory.json), the Java sources of
modernization/carddemo-app, the React route table and the modernization docs, and writes

    docs/modernization/07-traceability.md
    docs/modernization/traceability.json

Usage:
    python3 scripts/traceability/build_traceability.py              # regenerate both files
    python3 scripts/traceability/build_traceability.py --check      # regenerate into a temp dir; exit 1 on any
                                                                    # diff against the committed files or any GAP
    python3 scripts/traceability/build_traceability.py --out-dir D  # write into D instead

Paragraph -> method links come from Javadoc tags `{@code NNNN-PARA-NAME}` (ADR-0002). A tag counts for a program
when the member's Javadoc names the program, else when its class Javadoc names it, else when the file lives in one
of the program's packages (PROGRAM_PATHS). Paragraphs without a method get exactly one reason category
(retired-cics, retired-jcl, folded, GAP) from the rules and the hand-maintained tables below; any GAP fails.
Standard library only.
"""

from __future__ import annotations

import argparse
import csv
import difflib
import json
import re
import sys
import tempfile
from collections import OrderedDict, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
APP = ROOT / "app"
DOCS = ROOT / "docs" / "modernization"
JAVA = ROOT / "modernization" / "carddemo-app" / "src" / "main" / "java"
JAVA_PKG = JAVA / "com" / "carddemo"
RESOURCES = ROOT / "modernization" / "carddemo-app" / "src" / "main" / "resources"
COLUMN_MAP = RESOURCES / "db" / "copybook-column-map.csv"
UI_PROGRAMS = ROOT / "modernization" / "carddemo-ui" / "src" / "programs.ts"
UI_PAGES = ROOT / "modernization" / "carddemo-ui" / "src" / "pages"
OUT_DIR = DOCS
MD_NAME = "07-traceability.md"
JSON_NAME = "traceability.json"

EXPECTED = OrderedDict([
    ("programs", 31), ("copybooks", 47), ("jcl", 38), ("transactions", 17), ("scheduler", 45),
])
CATEGORIES = ("retired-cics", "retired-jcl", "folded", "GAP")
PARAGRAPH_RE = re.compile(r"^ {7}([0-9A-Z][0-9A-Z-]*) *\.$")

# ---------------------------------------------------------------------------
# Hand-maintained tables
# ---------------------------------------------------------------------------

ONLINE_COMMON = ["web/ScreenHeader.java", "web/ScreenHeaders.java", "web/NavigationContext.java",
                 "web/ProgramContext.java", "common/online/", "common/web/"]
BATCH_COMMON = ["batch/harness/", "common/date/", "common/codec/", "common/file/"]

# Packages (relative to com/carddemo) that implement each program; used when a tag names no program.
PROGRAM_PATHS = {
    "COSGN00C": ["web/signon/", "user/signon/"],
    "COMEN01C": ["web/menu/", "user/menu/"],
    "COADM01C": ["web/menu/", "user/menu/"],
    "COACTVWC": ["web/account/", "account/online/"],
    "COACTUPC": ["web/account/", "account/online/", "common/date/CsutldpyDateEdit.java"],
    "COCRDLIC": ["web/card/", "card/online/"],
    "COCRDSLC": ["web/card/", "card/online/"],
    "COCRDUPC": ["web/card/", "card/online/"],
    "COTRN00C": ["web/transaction/", "transaction/online/"],
    "COTRN01C": ["web/transaction/", "transaction/online/"],
    "COTRN02C": ["web/transaction/", "transaction/online/"],
    "COBIL00C": ["web/transaction/", "transaction/online/"],
    "CORPT00C": ["web/report/", "batch/report/"],
    "COUSR00C": ["web/user/", "user/admin/"],
    "COUSR01C": ["web/user/", "user/admin/"],
    "COUSR02C": ["web/user/", "user/admin/"],
    "COUSR03C": ["web/user/", "user/admin/"],
    "CBACT01C": ["batch/print/"],
    "CBACT02C": ["batch/print/"],
    "CBACT03C": ["batch/print/"],
    "CBCUS01C": ["batch/print/"],
    "CBACT04C": ["batch/intcalc/"],
    "CBTRN01C": ["batch/posttran/"],
    "CBTRN02C": ["batch/posttran/"],
    "CBTRN03C": ["batch/tranrept/"],
    "CBSTM03A": ["batch/creastmt/"],
    "CBSTM03B": ["batch/creastmt/"],
    "CBEXPORT": ["batch/exchange/"],
    "CBIMPORT": ["batch/exchange/"],
    "CSUTLDTC": ["common/date/"],
    "COBSWAIT": [],
}

# Programs whose Java home is not (only) found through the Javadoc scan.
PROGRAM_OVERRIDES = {
    "COBSWAIT": {"status": "retired",
                 "note": "WAITSTEP's 36-second MVSWAIT sleep between CLOSEFIL/OPENFIL and the batch jobs; the "
                         "nightly-cycle flow job sequences members by completion (06-scheduling.md §4)."},
}

# Paragraphs with no tagged method that the generic rules do not classify: (category, destination, note).
# Destinations "Class#method" (class = simple name) are checked to exist in the Java sources.
PARAGRAPHS: dict[str, dict[str, tuple[str, str, str]]] = {
    "CBACT04C": {
        "1400-COMPUTE-FEES": ("folded", "Cbact04c#run",
                              "empty stub in the source ('* To be implemented', EXIT): performed per category "
                              "balance and does nothing, so the Java loop has no fee step"),
        "Z-GET-DB2-FORMAT-TIMESTAMP": ("folded", "Cbtrn02c#PROC_TS",
                                       "same paragraph as CBTRN02C; both programs format with that formatter"),
    },
    "CBEXPORT": {
        "1100-OPEN-FILES": ("retired-jcl", "", "OPEN of the five input KSDS and EXPFILE: DD job parameters / repositories"),
        **{p: ("folded", "ExchangeJobConfiguration#cbexportJob",
               "sequential READ of one source dataset: the job's reader step for that record type")
           for p in ("2100-READ-CUSTOMER-RECORD", "3100-READ-ACCOUNT-RECORD", "4100-READ-XREF-RECORD",
                     "5100-READ-TRANSACTION-RECORD", "5600-READ-CARD-RECORD")},
    },
    "CBIMPORT": {
        "1100-OPEN-FILES": ("retired-jcl", "", "OPEN of EXPFILE and the six output files: DD job parameters / repositories"),
        "2100-READ-EXPORT-RECORD": ("folded", "ExchangeJobConfiguration#cbimportJob",
                                    "sequential READ of EXPFILE: the job's reader"),
    },
    "CBSTM03A": {
        "9999-GOBACK": ("folded", "Cbstm03a#run", "end of the main line (GOBACK = return of run())"),
        "8599-EXIT": ("folded", "Cbstm03a#loadTransactions",
                      "end of the TRNX load loop (table counter, GO TO 0000-START)"),
    },
    "CBSTM03B": {
        "9999-GOBACK": ("folded", "Cbstm03b#call", "GOBACK to CBSTM03A = return of call()"),
        "1900-EXIT": ("folded", "Cbstm03b#sequential", "MOVE TRNXFILE-STATUS TO LK-M03B-RC: the Response return code"),
        "1999-EXIT": ("folded", "Cbstm03b#sequential", "EXIT of 1000-TRNXFILE-PROC"),
        "2900-EXIT": ("folded", "Cbstm03b#sequential", "MOVE XREFFILE-STATUS TO LK-M03B-RC: the Response return code"),
        "2999-EXIT": ("folded", "Cbstm03b#sequential", "EXIT of 2000-XREFFILE-PROC"),
        "3900-EXIT": ("folded", "Cbstm03b#keyed", "MOVE CUSTFILE-STATUS TO LK-M03B-RC: the Response return code"),
        "3999-EXIT": ("folded", "Cbstm03b#keyed", "EXIT of 3000-CUSTFILE-PROC"),
        "4900-EXIT": ("folded", "Cbstm03b#keyed", "MOVE ACCTFILE-STATUS TO LK-M03B-RC: the Response return code"),
        "4999-EXIT": ("folded", "Cbstm03b#keyed", "EXIT of 4000-ACCTFILE-PROC"),
    },
    "COACTUPC": {
        "1000-PROCESS-INPUTS": ("folded", "AccountController#update, AccountUpdateService#update",
                                "RECEIVE MAP + edits: the JSON body is the input, the service runs the edits"),
        "1230-EDIT-ALPHANUM-REQD": ("folded", "AccountUpdateEdits#edit",
                                    "never PERFORMed in COACTUPC (dead code); no field uses it, so the edit list "
                                    "has no alphanumeric-required step"),
        "1240-EDIT-ALPHANUM-OPT": ("folded", "AccountUpdateEdits#edit",
                                   "never PERFORMed in COACTUPC (dead code); no field uses it, so the edit list "
                                   "has no alphanumeric-optional step"),
        "3201-SHOW-INITIAL-VALUES": ("retired-cics", "", "MOVEs blanks to the map fields: the UI form starts empty"),
        "3203-SHOW-UPDATED-VALUES": ("retired-cics", "", "MOVEs the typed values back to the map: AccountViewScreen / "
                                                         "AccountUpdateResponse carry them"),
        "3310-PROTECT-ALL-ATTRS": ("retired-cics", "", "BMS field attributes (DFHBMPRF): the React page decides "
                                                       "which inputs are editable from the response state"),
        "3320-UNPROTECT-FEW-ATTRS": ("retired-cics", "", "BMS field attributes (DFHBMFSE): same as 3310"),
        "3390-SETUP-INFOMSG-ATTRS": ("retired-cics", "", "BMS attributes of the info line: message + state in the response"),
    },
    "COACTVWC": {
        "2000-PROCESS-INPUTS": ("folded", "AccountController#view, AccountLookup#accountId",
                                "RECEIVE MAP + 2200 edit of the account id: path variable parsed by accountId()"),
    },
    "COCRDLIC": {
        "2100-RECEIVE-SCREEN": ("retired-cics", "", "RECEIVE MAP: query parameters / JSON body"),
        "9100-READ-BACKWARDS-EXIT": ("folded", "CardBrowse#previous",
                                     "ENDBR of the backward browse: a keyset query holds no browse cursor"),
        "2200-EDIT-INPUTS": ("folded", "CardController#list, CardKeys#listFilters",
                             "2210/2220 account and card filter edits + 2250 selection edit (CardSelection)"),
    },
    "COCRDSLC": {
        "2000-PROCESS-INPUTS": ("folded", "CardController#viewByAccount, CardKeys#searchKeys",
                                "RECEIVE MAP + 2200 account/card key edits"),
    },
    "COCRDUPC": {
        "1000-PROCESS-INPUTS": ("folded", "CardController#update, CardKeys#searchKeys, CardUpdateEdits#edit",
                                "RECEIVE MAP + 1200 key edits + 1230..1260 field edits"),
        "9300-CHECK-CHANGE-IN-REC": ("folded", "CardUpdateService#update",
                                     "re-read for update and compare with the fetched image: the version check "
                                     "(CardRepository.lockVersion, 409 CHANGED, ADR-0020)"),
    },
    "COTRN00C": {
        "PROCESS-ENTER-KEY": ("folded", "TransactionController#list, TransactionController#select",
                              "start key edit + selection dispatch: list (startTranId) and selection endpoints"),
        **{p: ("folded", "TransactionBrowse#browse",
               "CICS browse of TRANSACT: one keyset query per page (KeysetPage, ADR-0011)")
           for p in ("STARTBR-TRANSACT-FILE", "READNEXT-TRANSACT-FILE", "READPREV-TRANSACT-FILE",
                     "ENDBR-TRANSACT-FILE")},
    },
    "COTRN02C": {
        "READ-CXACAIX-FILE": ("folded", "TransactionAddEdits#validateInputKeyFields",
                              "account → card through CXACAIX: CardXrefRepository lookup inside the key edits"),
        "READ-CCXREF-FILE": ("folded", "TransactionAddEdits#validateInputKeyFields",
                             "card → account through CCXREF: CardXrefRepository lookup inside the key edits"),
    },
    "COUSR00C": {
        "PROCESS-ENTER-KEY": ("folded", "UserAdminController#select",
                              "selection U/D dispatch: the selection endpoint returns the NavigationContext"),
        "POPULATE-USER-DATA": ("folded", "UserListRow", "MOVEs one USRSEC record to a screen row: the row DTO"),
        **{p: ("folded", "UserListBrowse#browse",
               "CICS browse of USRSEC: one keyset query per page (KeysetPage, ADR-0011)")
           for p in ("STARTBR-USER-SEC-FILE", "READNEXT-USER-SEC-FILE", "READPREV-USER-SEC-FILE",
                     "ENDBR-USER-SEC-FILE")},
    },
}

# Copybooks with no record class / table / tagged Java type: what replaced them.
COPYBOOK_NOTES: dict[str, tuple[str, list[str]]] = {
    "CSDAT01Y": ("WS-DATE-TIME work area: current date/time shown in every online screen header", ["ScreenHeaders"]),
    "CSMSG02Y": ("ABEND-DATA of the ABEND-ROUTINE paragraphs (retired-cics): errors leave as the uniform ApiError "
                 "body (ADR-0019)", ["ApiError"]),
    "CSSETATY": ("retired (BMS attribute macro of COACTUPC): field highlighting comes from ApiError.field / "
                 "invalidFields in the React page", ["ApiError"]),
    "CSSTRPFY": ("retired (EIBAID → CCARD-AID-* mapping): PF keys are explicit endpoints / request fields "
                 "(08-ui-map.md, PF column)", []),
    "CVCRD01Y": ("CC-WORK-AREAS (AID, next program/mapset, account/card id input of the account and card programs): "
                 "NavigationContext + the request DTOs", ["NavigationContext"]),
    "CVTRA07Y": ("report header/detail/total line layouts of CBTRN03C", ["Cbtrn03c"]),
    "UNUSED1Y": ("unused: no program in app/cbl COPYs it (01-inventory.md); nothing to port", []),
}

# JCL member → (kind, Java, reason). Kinds: job | initial-load | on-demand | retired | out of scope.
_LOAD = "IDCAMS DELETE/DEFINE/REPRO of the sample into a KSDS; Flyway owns the table, initial-load fills it"
_GDG = "defines GDG bases; outputs are dated generations (DatedOutputFiles, ADR-0012) kept by carddemo.batch.retain"
JCL: dict[str, tuple[str, str, str]] = {
    "ACCTFILE": ("initial-load", "--job=initial-load (ACCTDATA → account)", _LOAD),
    "CARDFILE": ("initial-load", "--job=initial-load (CARDDATA → card)",
                 _LOAD + "; the CLCIFIL/OPCIFIL CEMT steps are retired (06-scheduling.md §4)"),
    "CBADMCDJ": ("retired", "common.online.InstalledPrograms",
                 "DFHCSDUP CSD definitions: no CICS region; transactions are REST controllers and the CSD is read "
                 "as data by InstalledPrograms (ADR-0017)"),
    "CBEXPORT": ("on-demand", "--job=cbexport", "STEP01 IDCAMS DEFINE of EXPFILE is not needed (file sink)"),
    "CBIMPORT": ("on-demand", "--job=cbimport", ""),
    "CLOSEFIL": ("retired", "", "CEMT SET FIL CLO: no CICS file ownership to hand over (06-scheduling.md §4)"),
    "COMBTRAN": ("job", "--job=combtran (nightly-cycle member COMBTRAN)", "STEP05R sort + STEP10 REPRO"),
    "CREASTMT": ("job", "--job=creastmt (nightly-cycle member CREASTMT)",
                 "STEP010 creastmt-sort, STEP020 trxfl-repro, STEP040 cbstm03a; DELDEF01/STEP030 not needed "
                 "(dated generations, 06-scheduling.md §3)"),
    "CUSTFILE": ("initial-load", "--job=initial-load (CUSTDATA → customer)",
                 _LOAD + "; CLCIFIL/OPCIFIL retired"),
    "DALYREJS": ("retired", "", _GDG),
    "DEFCUST": ("retired", "", "IDCAMS DEFINE of an alternative customer cluster (AWS.CUSTDATA.CLUSTER), not used by "
                               "the programs; the customer table comes from Flyway and CUSTFILE's data"),
    "DEFGDGB": ("retired", "", _GDG),
    "DEFGDGD": ("retired", "", "GDG bases + IEBGENER backups of TRANTYPE/TRANCATG/DISCGRP: reference data lives in "
                               "PostgreSQL (initial-load / repro); " + _GDG),
    "DISCGRP": ("initial-load", "--job=initial-load (DISCGRP → disclosure_group); --job=repro --DATASET=DISCGRP",
                _LOAD + "; weekly refresh on demand (06-scheduling.md §1c)"),
    "DUSRSECJ": ("initial-load", "--job=initial-load (USRSEC → user_security)", _LOAD),
    "ESDSRRDS": ("retired", "", "demo of ESDS/RRDS copies of USRSEC; no program reads them, user_security is the "
                                "one store"),
    "FTPJCL": ("retired", "", "z/OS FTP sample (PUT of a test dataset): file transfer is outside the application"),
    "INTCALC": ("job", "--job=intcalc (nightly-cycle member INTCALC)", "STEP15 cbact04c"),
    "INTRDRJ1": ("retired", "", "internal-reader demo (REPRO of the FTP test file, submit INTRDRJ2): no CardDemo data"),
    "INTRDRJ2": ("retired", "", "internal-reader demo, second job: no CardDemo data"),
    "OPENFIL": ("retired", "", "CEMT SET FIL OPE: see CLOSEFIL (06-scheduling.md §4)"),
    "POSTTRAN": ("job", "--job=posttran (nightly-cycle member POSTTRAN)", "STEP15 cbtrn02c (CBTRN01C as first step)"),
    "PRTCATBL": ("job", "--job=prtcatbl (nightly-cycle member PRTCATBL)",
                 "STEP05R reproc, STEP10R prtcatbl-sort; DELDEF not needed (dated generations)"),
    "READACCT": ("job", "--job=readacct (nightly-cycle member READACCT)", "STEP05 CBACT01C; PREDEL not needed"),
    "READCARD": ("job", "--job=readcard (nightly-cycle member READCARD)", "STEP05 CBACT02C"),
    "READCUST": ("job", "--job=readcust (nightly-cycle member READCUST)", "STEP05 CBCUS01C"),
    "READXREF": ("job", "--job=readxref (nightly-cycle member READXREF)", "STEP05 CBACT03C"),
    "REPTFILE": ("retired", "", _GDG),
    "TCATBALF": ("initial-load", "--job=initial-load (TCATBALF → tran_cat_balance); --job=repro --DATASET=TCATBALF",
                 _LOAD),
    "TRANBKP": ("job", "--job=tranbkp (nightly-cycle member TRANBKP)", "STEP05R reproc + IDCAMS DELETE/DEFINE (emulated)"),
    "TRANCATG": ("initial-load", "--job=initial-load (TRANCATG → transaction_category); --job=repro --DATASET=TRANCATG",
                 _LOAD),
    "TRANFILE": ("initial-load", "--job=initial-load (TRANSACT → transaction)",
                 _LOAD + "; the AIX is index transaction_proc_ts_ix (V2); CLCIFIL/OPCIFIL retired"),
    "TRANIDX": ("retired", "", "IDCAMS DEFINE AIX/PATH/BLDINDEX on TRANSACT: index transaction_proc_ts_ix in Flyway V2"),
    "TRANREPT": ("job", "--job=tranrept (nightly-cycle member TRANREPT)", "STEP05 reproc, STEP10 tranrept-sort, STEP15 cbtrn03c"),
    "TRANTYPE": ("initial-load", "--job=initial-load (TRANTYPE → transaction_type); --job=repro --DATASET=TRANTYPE",
                 _LOAD),
    "TXT2PDF1": ("retired", "", "TSO REXX TXT2PDF of STATEMNT.PS: no COBOL; HTML statements already produced "
                                "(06-scheduling.md §4)"),
    "WAITSTEP": ("retired", "", "COBSWAIT 36 s sleep: the flow job sequences by completion (06-scheduling.md §4)"),
    "XREFFILE": ("initial-load", "--job=initial-load (CARDXREF → card_xref)",
                 _LOAD + "; the CXACAIX AIX is index card_xref_acct_id_ix (V2)"),
}

# Scheduler job → (kind, Java, reason), from 06-scheduling.md §1c / §4.
_RET = "CICS file protocol / sleep: retired (06-scheduling.md §4)"
SCHEDULER: dict[str, tuple[str, str, str]] = {
    "CLOSEFIL": ("retired", "", _RET), "CLOSEFIL1": ("retired", "", _RET), "CLOSEFIL2": ("retired", "", _RET),
    "OPENFIL": ("retired", "", _RET), "WAITSTEP": ("retired", "", _RET),
    "TXT2PDF1": ("retired", "", "REXX PDF conversion, no COBOL (06-scheduling.md §4)"),
    "CBPAUP0J": ("out of scope", "", "IMS/DB2/MQ authorization extension (d-scope); POSTTRAN loses this predecessor"),
    "MNTTRDB2": ("out of scope", "", "Db2 transaction-type extension (d-scope)"),
    "TRANEXTR": ("out of scope", "", "Db2 transaction-type extension (d-scope)"),
    **{j: ("nightly-cycle", f"member {j} → --job={j.lower()}", "") for j in (
        "READACCT", "READCARD", "READCUST", "READXREF", "POSTTRAN", "INTCALC", "TRANBKP", "COMBTRAN",
        "CREASTMT", "PRTCATBL")},
    **{j: ("on-demand", f"--job=initial-load / --job=repro --DATASET={j}",
           "reference-data reload, not nightly in Java (06-scheduling.md §1c)")
       for j in ("TRANTYPE", "TRANCATG", "TCATBALF", "DISCGRP")},
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def rel(path: Path) -> str:
    return path.relative_to(ROOT).as_posix()


def md_escape(text) -> str:
    return str(text).replace("|", "\\|").replace("\n", " ")


def md_table(headers, rows) -> str:
    out = ["| " + " | ".join(headers) + " |", "| " + " | ".join("---" for _ in headers) + " |"]
    for r in rows:
        out.append("| " + " | ".join(md_escape(c) for c in r) + " |")
    return "\n".join(out)


def word(w: str) -> re.Pattern:
    return re.compile(r"(?<![A-Z0-9-])" + re.escape(w) + r"(?![A-Z0-9-])")


class Failure(Exception):
    pass


# ---------------------------------------------------------------------------
# Java scan
# ---------------------------------------------------------------------------

MEMBER_RE = re.compile(r"/\*\*(.*?)\*/\s*((?:@[\w.]+(?:\((?:[^()]|\([^()]*\))*\))?\s*)*)([^;{=]*?)([;{=(])", re.S)


class JavaIndex:
    def __init__(self, programs: list[str]):
        self.programs = programs
        self.files: dict[str, str] = {}
        for f in sorted(JAVA_PKG.rglob("*.java")):
            self.files[f.relative_to(JAVA_PKG).as_posix()] = read_text(f)
        self.members = []  # dict(file, cls, name, kind, javadoc)
        self.type_doc: dict[str, str] = {}
        self.class_file: dict[str, str] = {}
        for path, text in self.files.items():
            cls = Path(path).stem
            self.class_file.setdefault(cls, path)
            for m in MEMBER_RE.finditer(text):
                jd, decl, end = m.group(1), m.group(3), m.group(4)
                full = decl + end
                t = re.search(r"\b(class|record|enum|interface)\s+(\w+)", full)
                if t:
                    kind, name = "type", t.group(2)
                elif re.search(r"\bpackage\s+[\w.]+\s*;$", full.strip()):
                    kind, name = "package", cls
                elif end == "(":
                    n = re.search(r"(\w+)\s*\($", full)
                    kind, name = "method", (n.group(1) if n else "?")
                else:
                    n = re.search(r"(\w+)\s*[;=]$", full.strip())
                    kind, name = "field", (n.group(1) if n else "?")
                jd = re.sub(r"^\s*\*", "", jd, flags=re.M)
                jd = " ".join(jd.split())
                self.members.append({"file": path, "cls": cls, "name": name, "kind": kind, "javadoc": jd})
                if kind in ("type", "package") and name == cls and cls not in self.type_doc:
                    self.type_doc[path] = jd
        self.methods: dict[str, set[str]] = defaultdict(set)
        decl = re.compile(r"^\s+(?:(?:public|private|protected|static|final|abstract|synchronized|default)\s+)*"
                          r"(?:<[^>]+>\s+)?([\w.<>\[\], ?]+?)\s+(\w+)\s*\(", re.M)
        for path, text in self.files.items():
            cls = Path(path).stem
            for m in decl.finditer(text):
                rt, n = m.group(1).split(), m.group(2)
                if not rt or rt[-1] in ("new", "return", "else", "throw") or n in (
                        "if", "for", "while", "switch", "catch", "synchronized", "return"):
                    continue
                self.methods[cls].add(n)
            for m in re.finditer(r"\b(?:record|class|enum|interface)\s+(\w+)", text):
                self.methods[cls].add(m.group(1))
            for m in re.finditer(r"\bstatic\s+final\s+[\w.<>\[\]]+\s+([A-Z][A-Z0-9_]*)\s*=", text):
                self.methods[cls].add(m.group(1))
        # Program context of each member: programs named in its Javadoc or its class Javadoc, else by package.
        for mem in self.members:
            own = self.named(mem["javadoc"])
            cls_doc = self.named(self.type_doc.get(mem["file"], ""))
            mem["own"] = own
            mem["programs"] = (own | cls_doc) or {p for p in programs if self.in_paths(mem["file"], p)}

    def tag_programs(self, mem: dict, name: str) -> set[str]:
        """Programs a {@code NAME} tag in this member applies to: "PGM {@code NAME}" names one program
        explicitly; an unqualified tag applies to the programs the member's own Javadoc names, else to its
        class/package program context."""
        tag, jd, out = "{@code " + name + "}", mem["javadoc"], set()
        start = jd.find(tag)
        while start >= 0:
            q = re.search(r"(?<![A-Z0-9-])([A-Z][A-Z0-9]{3,7})(?:'s)?\s+$", jd[:start])
            out |= {q.group(1)} if q and q.group(1) in self.programs else (mem["own"] or mem["programs"])
            start = jd.find(tag, start + 1)
        return out

    def named(self, text: str) -> set[str]:
        return {p for p in self.programs if word(p).search(text)}

    @staticmethod
    def in_paths(path: str, pgm: str) -> bool:
        paths = list(PROGRAM_PATHS.get(pgm, []))
        if PROGRAM_PATHS.get(pgm):
            paths += ONLINE_COMMON if pgm.startswith("CO") else BATCH_COMMON
        return any(path == p or (p.endswith("/") and path.startswith(p)) for p in paths)

    def qualified(self, path: str) -> str:
        return path[:-5].replace("/", ".")

    def exists(self, ref: str) -> bool:
        cls, _, meth = ref.partition("#")
        if cls not in self.class_file:
            return False
        return not meth or meth in self.methods[cls]

    def qref(self, ref: str) -> str:
        cls, _, meth = ref.partition("#")
        q = self.qualified(self.class_file[cls])
        return q + ("#" + meth if meth else "")


# ---------------------------------------------------------------------------
# COBOL
# ---------------------------------------------------------------------------


def program_file(pgm: str) -> Path:
    for f in (APP / "cbl").iterdir():
        if f.stem.upper() == pgm:
            return f
    raise Failure(f"no source for {pgm}")


def paragraphs(pgm: str) -> list[dict]:
    lines = read_text(program_file(pgm)).split("\n")
    out, proc, cur = [], False, None
    for i, raw in enumerate(lines, 1):
        line = raw.rstrip("\r")[:72]
        if len(line) > 6 and line[6] in "*/":
            continue
        if not proc:
            if re.search(r"\bPROCEDURE\s+DIVISION\b", line[7:]):
                proc = True
            continue
        m = PARAGRAPH_RE.match(line.rstrip())
        if m and not line.rstrip().endswith("SECTION."):
            cur = {"name": m.group(1), "line": i, "body": []}
            out.append(cur)
        elif cur is not None and line[7:].strip():
            cur["body"].append(line[7:].strip())
    return out


ONLINE_RETIRED = re.compile(
    r"^(SEND-|RECEIVE-|RETURN-TO-|COMMON-RETURN$|INITIALIZE-|CLEAR-CURRENT-SCREEN$|ABEND-ROUTINE$|YYYY-STORE-PFKEY$|"
    r"MAIN-PARA$|0000-MAIN$|SEND-PLAIN-TEXT$|SEND-LONG-TEXT$)"
    r"|-(SEND-MAP|SEND-SCREEN|SEND-LONG-TEXT|SEND-PLAIN-TEXT|RECEIVE-MAP|SCREEN-INIT|SCREEN-ARRAY-INIT|"
    r"SETUP-SCREEN-VARS|SETUP-SCREEN-ATTRS|SETUP-ARRAY-ATTRIBS|SETUP-INFOMSG|SETUP-MESSAGE|SETUP-HEADER)$")
BATCH_RETIRED = re.compile(r"(-OPEN|-CLOSE)$")


def classify_generic(pgm: str, ptype: str, para: dict, prev: dict | None,
                     owner: str | None = None) -> tuple[str, str, str] | None:
    name = para["name"]
    body = " ".join(re.sub(r"COPY\s+'?[A-Z0-9]+'?", "", " ".join(para["body"])).replace(".", " ").split())
    if name.endswith("-EXIT") and body in ("EXIT", "") and (owner or prev) is not None:
        o = owner or prev["name"]
        return ("folded", "@" + o, "PERFORM ... THRU exit point of " + o)
    if ptype == "online" and ONLINE_RETIRED.search(name):
        return ("retired-cics", "", "BMS SEND/RECEIVE, COMMAREA set-up, RETURN/XCTL plumbing or EIBCALEN/EIBAID "
                                    "dispatch: replaced by the REST controller and NavigationContext (ADR-0007/0008)")
    if ptype != "online" and BATCH_RETIRED.search(name):
        return ("retired-jcl", "", "OPEN/CLOSE of a DD: replaced by DD job parameters (file path or table), "
                                   "repositories and KeyedDataset/sinks (ADR-0015)")
    return None


# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------


def load_inventory() -> dict:
    return json.loads(read_text(DOCS / "inventory.json"))["modules"]["core"]


def check_inventory_current(inv: dict) -> None:
    """The matrix enumerates programs/JCL/copybooks from inventory.json: refuse a stale inventory."""
    on_disk = {
        "programs": (APP / "cbl", (".cbl",)),
        "jcl_jobs": (APP / "jcl", (".jcl",)),
        "copybooks": (APP / "cpy", (".cpy",)),
        "bms_copybooks": (APP / "cpy-bms", (".cpy",)),
    }
    for key, (folder, exts) in on_disk.items():
        files = {f.stem.upper() for f in folder.iterdir() if f.suffix.lower() in exts}
        listed = {Path(n).stem.upper() for n in inv[key]}
        if files != listed:
            raise Failure(f"inventory.json {key} out of date vs {folder.relative_to(ROOT)}: "
                          f"missing {sorted(files - listed)}, extra {sorted(listed - files)} "
                          "(regenerate with docs/modernization/build_inventory.py)")


def build() -> OrderedDict:
    inv = load_inventory()
    check_inventory_current(inv)
    programs = sorted(inv["programs"])
    jx = JavaIndex(programs)
    problems: list[str] = []

    def check_ref(ref: str, where: str) -> str:
        if not jx.exists(ref):
            problems.append(f"{where}: unknown Java reference {ref}")
            return ref
        return jx.qref(ref)

    rules = {p.stem for p in (DOCS / "rules").glob("*.md")}
    ui = ui_pages()

    # 1. Programs ---------------------------------------------------------
    prog_rows = []
    for pgm in programs:
        meta = inv["programs"][pgm]
        classes = sorted({jx.qualified(path) for path, doc in jx.type_doc.items()
                          if word(pgm).search(doc) and not path.endswith("package-info.java")})
        packages = sorted({jx.qualified(path).rsplit(".", 1)[0] for path, doc in jx.type_doc.items()
                           if path.endswith("package-info.java") and word(pgm).search(doc)})
        controllers = sorted({jx.qualified(path) for path, text in jx.files.items()
                              if path.endswith("Controller.java") and word(pgm).search(text)})
        ov = PROGRAM_OVERRIDES.get(pgm, {})
        status = ov.get("status") or ("ported" if classes or controllers else "GAP")
        prog_rows.append(OrderedDict([
            ("program", pgm), ("type", meta["type"]), ("transaction", meta.get("transaction_id")),
            ("status", status), ("controllers", controllers), ("classes", classes), ("packages", packages),
            ("rules_doc", f"docs/modernization/rules/{pgm}.md" if pgm in rules else None),
            ("ui_route", ui.get(pgm, {}).get("route")), ("note", ov.get("note", "")),
        ]))

    # 2. Paragraphs --------------------------------------------------------
    para_rows = []
    for pgm in programs:
        ptype = inv["programs"][pgm]["type"]
        plist = paragraphs(pgm)
        resolved: dict[str, dict] = {}
        for idx, para in enumerate(plist):
            name = para["name"]
            tag = "{@code " + name + "}"
            hits = sorted({f"{jx.qualified(m['file'])}#{m['name']}" if m["kind"] != "type" or m["name"] != m["cls"]
                           else jx.qualified(m["file"])
                           for m in jx.members
                           if m["kind"] in ("method", "type", "package", "field") and tag in m["javadoc"]
                           and pgm in jx.tag_programs(m, name)})
            entry = OrderedDict([("program", pgm), ("paragraph", name), ("line", para["line"])])
            if hits:
                entry.update(status="mapped", methods=hits, category=None, destination=None, note="")
            else:
                explicit = PARAGRAPHS.get(pgm, {}).get(name)
                names = {p["name"] for p in plist}
                generic = classify_generic(pgm, ptype, para, plist[idx - 1] if idx else None,
                                           name[:-5] if name[:-5] in names else None)
                cat, dest, note = explicit or generic or ("GAP", "", "no tagged method, no rule")
                entry.update(status="not-mapped", methods=[], category=cat, destination=dest, note=note)
            resolved[name] = entry
            para_rows.append(entry)
        # resolve "@PARA" (folded into another paragraph) destinations
        for entry in [r for r in para_rows if r["program"] == pgm]:
            dest = entry["destination"]
            if dest and dest.startswith("@"):
                owner = resolved.get(dest[1:])
                if owner is None:
                    problems.append(f"{pgm} {entry['paragraph']}: unknown owner paragraph {dest}")
                    continue
                if owner["status"] == "mapped":
                    entry["destination"] = ", ".join(owner["methods"])
                elif owner["category"] == "folded":
                    entry["destination"] = owner["destination"]
                else:
                    entry["category"] = owner["category"]
                    entry["destination"] = ""
                    entry["note"] = f"exit point of {owner['paragraph']} ({owner['category']})"
            elif dest and entry["category"] == "folded":
                entry["destination"] = ", ".join(check_ref(d.strip(), f"{pgm} {entry['paragraph']}")
                                                 for d in dest.split(","))
            if entry["category"] == "folded" and not entry["destination"]:
                problems.append(f"{pgm} {entry['paragraph']}: folded without a destination")
            if entry["category"] not in (None,) + CATEGORIES:
                problems.append(f"{pgm} {entry['paragraph']}: bad category {entry['category']}")

    # 3. Copybooks ---------------------------------------------------------
    colmap = defaultdict(lambda: {"datasets": set(), "tables": set()})
    with COLUMN_MAP.open(encoding="utf-8") as fh:
        for row in csv.DictReader(line for line in fh if not line.startswith("#")):
            colmap[row["copybook"]]["datasets"].add(row["dataset"])
            if row["table"]:
                colmap[row["copybook"]]["tables"].add(row["table"])
    record_classes = defaultdict(set)
    for path, text in jx.files.items():
        for m in re.finditer(r'CopybookRecordMapper\.of\(\s*\w+\.class\s*,\s*"([A-Z0-9]+)"', text):
            record_classes[m.group(1)].add(jx.qualified(path))
        for m in re.finditer(r'Copybook\.layout\(\s*"([A-Z0-9]+)"\s*\)', text):
            record_classes[m.group(1)].add(jx.qualified(path))
    entities = {}
    for path, text in jx.files.items():
        m = re.search(r'@Table\(\s*name\s*=\s*"([a-z_]+)"', text)
        if m and "@Entity" in text:
            entities[m.group(1)] = jx.qualified(path)
    ui_map = ui_map_rows()
    copy_rows = []
    cpy_files = sorted((APP / "cpy").iterdir()) + sorted((APP / "cpy-bms").iterdir())
    for f in cpy_files:
        if f.suffix.lower() != ".cpy":
            continue
        name, bms = f.stem.upper(), f.parent.name == "cpy-bms"
        tagged = sorted({jx.qualified(m["file"]) for m in jx.members
                         if m["kind"] in ("type", "package", "field") and "{@code " + name + "}" in m["javadoc"]
                         and not m["file"].endswith("package-info.java")})
        tables = sorted(colmap[name]["tables"]) if name in colmap else []
        entry = OrderedDict([
            ("copybook", name), ("source", rel(f)), ("kind", "bms" if bms else "copybook"),
            ("record_classes", sorted(record_classes.get(name, ()))),
            ("datasets", sorted(colmap[name]["datasets"]) if name in colmap else []),
            ("tables", tables), ("entities", [entities[t] for t in tables if t in entities]),
            ("java", tagged), ("ui", None), ("note", ""),
        ])
        if bms:
            row = ui_map.get(name)
            if row:
                entry["ui"] = OrderedDict([("route", row["route"]), ("program", row["program"]),
                                           ("page", ui.get(row["program"], {}).get("page")),
                                           ("doc", "docs/modernization/08-ui-map.md")])
        if name in COPYBOOK_NOTES:
            note, refs = COPYBOOK_NOTES[name]
            entry["note"] = note
            entry["java"] = sorted(set(entry["java"]) | {check_ref(r, name) for r in refs})
        entry["status"] = "mapped" if (entry["record_classes"] or entry["tables"] or entry["java"] or entry["ui"]
                                       or entry["note"]) else "GAP"
        copy_rows.append(entry)

    # 4. JCL --------------------------------------------------------------
    job_names = java_job_names(jx)
    jcl_rows = []
    for name in sorted(inv["jcl_jobs"]):
        kind, java, note = JCL.get(name, ("GAP", "", "not in the JCL table"))
        for j in re.findall(r"--job=([a-z0-9-]+)", java):
            if j not in job_names:
                problems.append(f"JCL {name}: --job={j} is not a Java job name")
        steps = [s["step"] + ": " + (s.get("program") or ("PROC=" + (s.get("proc") or "?")))
                 for s in inv["jcl_jobs"][name]["steps"]]
        jcl_rows.append(OrderedDict([("jcl", name), ("steps", steps), ("kind", kind), ("java", java),
                                     ("note", note)]))

    # 5. CICS transactions ---------------------------------------------
    csd = inv["csd"]["CARDDEMO.CSD"]["transactions"]
    endpoints = controller_endpoints(jx)
    tran_rows, csd_extra = [], []
    menus = menu_targets()
    for tran in sorted(csd):
        pgm = csd[tran]
        if pgm not in inv["programs"]:
            csd_extra.append(OrderedDict([("transaction", tran), ("program", pgm)]))
            continue
        row = next((r for r in ui_map.values() if r["program"] == pgm), None)
        calls = row["api"] if row else []
        for call in calls:
            if not endpoint_known(call, endpoints):
                problems.append(f"{tran}: API call {call} (08-ui-map.md) has no controller mapping")
        prog = next(p for p in prog_rows if p["program"] == pgm)
        tran_rows.append(OrderedDict([
            ("transaction", tran), ("program", pgm), ("mapset", row["mapset"] if row else None),
            ("endpoints", calls), ("controllers", prog["controllers"]),
            ("route", ui.get(pgm, {}).get("route")), ("menu_options", menus.get(pgm, [])),
        ]))
    menu_rows = []
    for menu, opts in menu_options():
        for num, label, target in opts:
            tran = next((t for t, p in csd.items() if p == target), None)
            menu_rows.append(OrderedDict([("menu", menu), ("option", num), ("label", label), ("program", target),
                                          ("transaction", tran), ("route", ui.get(target, {}).get("route")),
                                          ("in_scope", target in inv["programs"])]))

    # 6. Scheduler --------------------------------------------------------
    sched_rows = []
    for sched, folder, job in scheduler_definitions():
        kind, java, note = SCHEDULER.get(job, ("GAP", "", "not in the scheduler table"))
        sched_rows.append(OrderedDict([("scheduler", sched), ("folder_or_chain", folder), ("job", job),
                                       ("kind", kind), ("java", java), ("note", note)]))

    # 7. ADRs and deviations ----------------------------------------------
    adr_rows = []
    for f in sorted((DOCS / "adr").glob("ADR-*.md")):
        title = next((l[2:].strip() for l in read_text(f).splitlines() if l.startswith("# ")), f.stem)
        adr_rows.append(OrderedDict([("adr", f.stem[:8]), ("title", title), ("file", rel(f))]))
    dev_rows = deviations()

    # Counts / checks -------------------------------------------------------
    counts = OrderedDict([
        ("programs", len(prog_rows)), ("copybooks", len(copy_rows)), ("jcl", len(jcl_rows)),
        ("transactions", len(tran_rows)), ("scheduler", len(sched_rows)),
    ])
    for k, v in EXPECTED.items():
        if counts[k] != v:
            problems.append(f"count {k} = {counts[k]}, expected {v}")
    inv_counts = json.loads(read_text(DOCS / "inventory.json"))["counts"]["core"]
    if counts["copybooks"] != inv_counts["copybooks"] + inv_counts["bms_copybooks"]:
        problems.append("copybook count differs from inventory.json")
    gaps = ([f"program {r['program']}" for r in prog_rows if r["status"] == "GAP"]
            + [f"paragraph {r['program']} {r['paragraph']}" for r in para_rows if r["category"] == "GAP"]
            + [f"copybook {r['copybook']}" for r in copy_rows if r["status"] == "GAP"]
            + [f"jcl {r['jcl']}" for r in jcl_rows if r["kind"] == "GAP"]
            + [f"transaction {r['transaction']}" for r in tran_rows if not r["endpoints"] or not r["route"]]
            + [f"scheduler {r['scheduler']} {r['job']}" for r in sched_rows if r["kind"] == "GAP"])
    by_cat = OrderedDict((c, sum(1 for r in para_rows if r["category"] == c)) for c in CATEGORIES)
    para_counts = OrderedDict([("total", len(para_rows)),
                               ("mapped", sum(1 for r in para_rows if r["status"] == "mapped"))])
    para_counts.update(by_cat)

    return OrderedDict([
        ("generated_by", "scripts/traceability/build_traceability.py"),
        ("counts", counts), ("expected", EXPECTED), ("paragraphs", para_counts),
        ("gaps", gaps), ("problems", problems),
        ("programs", prog_rows), ("paragraph_map", para_rows), ("copybooks", copy_rows), ("jcl", jcl_rows),
        ("transactions", tran_rows), ("csd_without_source", csd_extra), ("menu_targets", menu_rows),
        ("scheduler", sched_rows), ("adrs", adr_rows), ("deviations", dev_rows),
    ])


# ---------------------------------------------------------------------------
# Source readers
# ---------------------------------------------------------------------------


def ui_pages() -> dict[str, dict]:
    text = read_text(UI_PROGRAMS)
    out = {}
    for m in re.finditer(r"\{\s*program:\s*'(\w+)',\s*tranId:\s*'(\w+)',\s*mapset:\s*'(\w+)',\s*map:\s*'(\w+)',"
                         r"\s*title:\s*'([^']*)',\s*route:\s*'([^']*)'", text):
        pgm = m.group(1)
        page = None
        for p in sorted(UI_PAGES.glob("*Page.tsx")):
            if re.search(r"\b" + pgm + r"\b", read_text(p)):
                page = rel(p)
                break
        out[pgm] = {"tranId": m.group(2), "mapset": m.group(3), "map": m.group(4), "title": m.group(5),
                    "route": m.group(6), "page": page}
    return out


def ui_map_rows() -> dict[str, dict]:
    rows = {}
    for line in read_text(DOCS / "08-ui-map.md").splitlines():
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if len(cells) < 7 or not re.fullmatch(r"CO[A-Z0-9]+", cells[0]):
            continue
        m = re.match(r"(\w{4}) / (\w+)", cells[1])
        api = [a.strip() for a in re.split(r"<br>", cells[5]) if re.match(r"(GET|POST|PUT|DELETE) /", a.strip())]
        rows[cells[0]] = {"mapset": cells[0], "tran": m.group(1), "program": m.group(2),
                          "route": re.search(r"`([^`]+)`", cells[2]).group(1), "api": api}
    return rows


def controller_endpoints(jx: JavaIndex) -> list[tuple[str, str]]:
    out = []
    for path, text in jx.files.items():
        if not path.startswith("web/") or not path.endswith("Controller.java"):
            continue
        consts = dict(re.findall(r'static final String (\w+)\s*=\s*"([^"]*)"', text))

        def value(args: str) -> str:
            m = re.search(r'(?:(?:path|value)\s*=\s*)?(?:"([^"]*)"|([A-Za-z_][\w.]*))', args)
            if not m or (m.group(2) or "").split(".")[-1] in ("consumes", "produces"):
                return ""
            if re.match(r"\s*(consumes|produces)\s*=", args) and not re.search(r"(path|value)\s*=", args):
                return ""
            return m.group(1) if m.group(1) is not None else consts.get(m.group(2).split(".")[-1], "")

        head = text[:re.search(r"\bclass\s+\w+", text).start()]
        m = re.search(r"@RequestMapping\(([^)]*)\)", head)
        base = value(m.group(1)) if m else ""
        for m in re.finditer(r"@(Get|Post|Put|Delete|Patch)Mapping(?:\(([^)]*)\))?", text):
            sub = value(m.group(2) or "")
            out.append((m.group(1).upper(), sub if sub.startswith("/api") else base + sub))
    return out


def endpoint_known(call: str, endpoints: list[tuple[str, str]]) -> bool:
    verb, path = call.split(" ", 1)
    pattern = re.sub(r"\\\{[^}]*\\\}", r"[^/]+", re.escape("/api/v1" + path))
    norm = [(v, re.sub(r"\{[^}]*\}", "{x}", p)) for v, p in endpoints]
    return any(v == verb and re.fullmatch(pattern, p) for v, p in norm)


def java_job_names(jx: JavaIndex) -> set[str]:
    names = set()
    for path, text in jx.files.items():
        if path.startswith("batch/"):
            names |= set(re.findall(r'static final String \w+\s*=\s*"([a-z0-9-]+)"', text))
            names |= set(re.findall(r'new Member\("\w+",\s*"([a-z0-9-]+)"', text))
    return names


def menu_options() -> list[tuple[str, list[tuple[int, str, str]]]]:
    out = []
    for menu, cpy in (("COMEN01C", "COMEN02Y"), ("COADM01C", "COADM02Y")):
        lines = [l[6:72] for l in read_text(APP / "cpy" / f"{cpy}.cpy").splitlines() if len(l) > 6 and l[6] != "*"]
        src = " ".join(lines)
        opts = []
        for m in re.finditer(r"PIC 9\(02\) VALUE (\d+)\.\s+10 FILLER\s+PIC X\(35\) VALUE\s+'([^']*)'\.\s+"
                             r"10 FILLER\s+PIC X\(08\) VALUE '([^']*)'", src):
            opts.append((int(m.group(1)), m.group(2).strip(), m.group(3).strip()))
        out.append((menu, opts))
    return out


def menu_targets() -> dict[str, list[str]]:
    out = defaultdict(list)
    for menu, opts in menu_options():
        for num, _label, target in opts:
            out[target].append(f"{menu} option {num}")
    return out


def scheduler_definitions() -> list[tuple[str, str, str]]:
    defs = []
    ctm = read_text(APP / "scheduler" / "CardDemo.controlm")
    for fm in re.finditer(r"<(SMART_FOLDER|FOLDER)\b([^>]*)>(.*?)</\1>", ctm, re.S):
        folder = re.search(r'FOLDER_NAME="([^"]+)"', fm.group(2)).group(1)
        for jm in re.finditer(r"<JOB\b[^>]*\bJOBNAME=\"([^\"]+)\"", fm.group(3)):
            defs.append(("Control-M", folder, jm.group(1)))
    ca7 = read_text(APP / "scheduler" / "CardDemo.ca7")
    for i, m in enumerate(re.finditer(r"LJOB,JOB=([A-Z0-9]+)", ca7), 1):
        defs.append(("CA-7", f"LJOB #{i:02d}", m.group(1)))
    return defs


DEVIATION_RE = re.compile(r"\b(deviations?|deliberate|addition|added check|not in the cobol)\b", re.I)


def deviations() -> list[dict]:
    rows = []
    files = sorted((DOCS / "rules").glob("*.md")) + sorted((DOCS / "adr").glob("ADR-*.md"))
    for f in files:
        lines = read_text(f).splitlines()
        seen = set()
        for i, line in enumerate(lines):
            if not DEVIATION_RE.search(line):
                continue
            # the enclosing bullet / table row / paragraph
            start = i
            while start > 0 and lines[start].strip() and not re.match(r"\s*([-*]|\d+\.|\|)\s", lines[start]) \
                    and not lines[start - 1].strip() == "":
                start -= 1
            if start in seen:
                continue
            seen.add(start)
            end = start + 1
            while end < len(lines) and lines[end].strip() and not re.match(r"\s*([-*]|\d+\.|\|)\s", lines[end]):
                end += 1
            text = " ".join(l.strip() for l in lines[start:end])
            text = re.sub(r"^([-*]|\d+\.)\s+", "", text)
            if text.startswith("|"):
                cells = [c.strip() for c in text.strip("|").split("|")]
                text = " — ".join(c for c in cells if c)
            if len(text) > 260:
                text = text[:257].rstrip() + "..."
            rows.append(OrderedDict([("file", rel(f)), ("line", start + 1), ("text", text)]))
    return rows


# ---------------------------------------------------------------------------
# Markdown
# ---------------------------------------------------------------------------


def short(q: str) -> str:
    return "`" + q.replace("com.carddemo.", "") + "`" if q else ""


def render(data: OrderedDict) -> str:
    L = []
    c, e, pc = data["counts"], data["expected"], data["paragraphs"]
    L.append("# 07 — Traceability matrix: CardDemo COBOL/CICS/JCL/VSAM → Java 21")
    L.append("")
    L.append("Generated by `scripts/traceability/build_traceability.py` (`make traceability`); do not edit by hand. "
             "`make traceability-check` regenerates into a temp dir and fails on any diff or `GAP`. Machine-readable "
             "copy: [`traceability.json`](traceability.json). Java names are relative to `com.carddemo`.")
    L.append("")
    L.append("Scope (`d-scope`): the core app under `app/`. The extension apps (`app/app-authorization-ims-db2-mq`, "
             "`app/app-transaction-type-db2`, `app/app-vsam-mq`) and their jobs (`CBPAUP0J`, `MNTTRDB2`, `TRANEXTR`) "
             "appear below only where the core schedules reference them, as **out of scope**.")
    L.append("")
    L.append("## Coverage")
    L.append("")
    rows = [[k, f"{c[k]}/{e[k]}"] for k in e]
    rows.append(["paragraphs", f"{pc['total']} ({pc['mapped']} mapped to methods; not mapped: "
                               f"{pc['retired-cics']} retired-cics, {pc['retired-jcl']} retired-jcl, "
                               f"{pc['folded']} folded, {pc['GAP']} GAP)"])
    rows.append(["GAP entries", str(len(data["gaps"]))])
    L.append(md_table(["Item", "Covered"], rows))
    L.append("")
    L.append("Reason categories for paragraphs without a method of their own: `retired-cics` (BMS SEND/RECEIVE, "
             "COMMAREA, RETURN/XCTL plumbing — the REST controllers and `NavigationContext` replace them), "
             "`retired-jcl` (OPEN/CLOSE of a DD — replaced by DD job parameters, repositories and datasets), "
             "`folded` (merged into the named method), `GAP` (unmapped; fails the check).")
    L.append("")

    L.append("## 1. Program → Java classes")
    L.append("")
    L.append("Classes are those whose class Javadoc names the program (ADR-0002); controllers are the "
             "`web.*Controller` classes that name it.")
    L.append("")
    rows = []
    for r in data["programs"]:
        rows.append([f"`{r['program']}`", r["type"], r["transaction"] or "", r["status"],
                     "<br>".join(short(x) for x in r["controllers"]),
                     "<br>".join(short(x) for x in r["classes"]) + (("<br>" if r["classes"] else "") + r["note"]
                                                                   if r["note"] else ""),
                     f"[{r['program']}.md](rules/{r['program']}.md)" if r["rules_doc"] else "",
                     f"`{r['ui_route']}`" if r["ui_route"] else ""])
    L.append(md_table(["Program", "Type", "Tran", "Status", "Controller", "Classes", "Rules", "UI route"], rows))
    L.append("")

    L.append("## 2. Paragraph → method")
    L.append("")
    L.append("Every paragraph of the PROCEDURE DIVISION (`^ {7}[0-9A-Z][0-9A-Z-]* *\\.$`).")
    L.append("")
    by_pgm = defaultdict(list)
    for r in data["paragraph_map"]:
        by_pgm[r["program"]].append(r)
    for pgm in sorted(by_pgm):
        rs = by_pgm[pgm]
        mapped = [r for r in rs if r["status"] == "mapped"]
        L.append(f"### {pgm} — {len(rs)} paragraphs, {len(mapped)} mapped, {len(rs) - len(mapped)} not mapped")
        L.append("")
        if not rs:
            L.append("No paragraphs (the program is a single unnamed paragraph).")
            L.append("")
            continue
        if mapped:
            L.append(md_table(["Paragraph", "Line", "Method(s)"],
                              [[f"`{r['paragraph']}`", r["line"], "<br>".join(short(m) for m in r["methods"])]
                               for r in mapped]))
            L.append("")
        nm = [r for r in rs if r["status"] != "mapped"]
        if nm:
            L.append("Not mapped:")
            L.append("")
            L.append(md_table(["Paragraph", "Line", "Category", "Folded into / reason"],
                              [[f"`{r['paragraph']}`", r["line"], f"`{r['category']}`",
                                ", ".join(short(d.strip()) for d in r["destination"].split(",")) +
                                (" — " if r["destination"] and r["note"] else "") + r["note"]
                                if r["destination"] else r["note"]]
                               for r in nm]))
            L.append("")

    L.append("## 3. Copybook → record / entity / table / UI")
    L.append("")
    rows = []
    for r in data["copybooks"]:
        ui = r["ui"]
        rows.append([f"`{r['copybook']}`", r["kind"], "<br>".join(short(x) for x in r["record_classes"]),
                     "<br>".join(f"`{t}`" for t in r["tables"]) +
                     ("<br>" + "<br>".join(short(x) for x in r["entities"]) if r["entities"] else ""),
                     "<br>".join(short(x) for x in r["java"]),
                     (f"`{ui['route']}` ({ui['program']}, `{ui['page'].rsplit('/', 1)[-1]}`)" if ui and ui["page"]
                      else (f"`{ui['route']}`" if ui else "")),
                     r["note"]])
    L.append(md_table(["Copybook", "Kind", "Record class", "Table / entity", "Java", "UI page", "Note"], rows))
    L.append("")
    L.append("Tables: `modernization/carddemo-app/src/main/resources/db/copybook-column-map.csv` (field → column, "
             "`docs/modernization/03-data-model.md`). UI: `docs/modernization/08-ui-map.md`.")
    L.append("")

    L.append("## 4. JCL job → Spring Batch job / stream")
    L.append("")
    rows = [[f"`{r['jcl']}`", "<br>".join(r["steps"]), r["kind"], r["java"], r["note"]] for r in data["jcl"]]
    L.append(md_table(["JCL", "Steps", "Kind", "Java", "Note"], rows))
    L.append("")

    L.append("## 5. CICS transaction / BMS map → endpoints → UI route")
    L.append("")
    rows = [[f"`{r['transaction']}`", f"`{r['program']}`", f"`{r['mapset']}`", "<br>".join(r["endpoints"]),
             "<br>".join(short(x) for x in r["controllers"]), f"`{r['route']}`",
             "<br>".join(r["menu_options"])] for r in data["transactions"]]
    L.append(md_table(["Tran", "Program", "Mapset", "Endpoints (`/api/v1`)", "Controller", "Route",
                       "Menu XCTL"], rows))
    L.append("")
    for x in data["csd_without_source"]:
        L.append(f"The CSD also defines `{x['transaction']}` → `{x['program']}`, which has no source in `app/cbl` "
                 f"(01-inventory.md); it is listed in `common.online.OnlineProgram` as installed but has no endpoint.")
        L.append("")
    L.append("Menu option tables (`COMEN02Y`, `COADM02Y`) → XCTL target → transaction → route:")
    L.append("")
    rows = [[r["menu"], r["option"], r["label"], f"`{r['program']}`", r["transaction"] or "—",
             f"`{r['route']}`" if r["route"] else ("—" if r["in_scope"] else "out of scope (extension app)")]
            for r in data["menu_targets"]]
    L.append(md_table(["Menu", "Option", "Label", "Program", "Tran", "Route"], rows))
    L.append("")

    L.append("## 6. Scheduler definition → nightly-cycle member / retired / out of scope")
    L.append("")
    L.append("All job definitions of `app/scheduler/CardDemo.controlm` (15) and `app/scheduler/CardDemo.ca7` "
             "(30 `LJOB` sections); details and conditions in `06-scheduling.md`, operations in "
             "`10-runbook-nightly-cycle.md`.")
    L.append("")
    rows = [[r["scheduler"], r["folder_or_chain"], f"`{r['job']}`", r["kind"], r["java"], r["note"]]
            for r in data["scheduler"]]
    L.append(md_table(["Scheduler", "Folder / section", "Job", "Kind", "Java", "Note"], rows))
    L.append("")

    L.append("## 7. ADR index and deviation index")
    L.append("")
    L.append(md_table(["ADR", "Title"], [[f"[{r['adr']}](adr/{Path(r['file']).name})", r["title"]]
                                          for r in data["adrs"]]))
    L.append("")
    L.append("Deviations and additions recorded in `rules/*.md` and the ADRs (every paragraph that says "
             "deviation, deliberate, addition or not in the COBOL):")
    L.append("")
    L.append(md_table(["Documented in", "Deviation / addition"],
                      [[f"[{Path(r['file']).name}:{r['line']}]({Path(r['file']).relative_to('docs/modernization').as_posix()})",
                        r["text"]] for r in data["deviations"]]))
    L.append("")
    return "\n".join(L) + "\n"


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true", help="regenerate into a temp dir; fail on diff or GAP")
    ap.add_argument("--out-dir", type=Path, default=None, help="write the outputs here instead")
    args = ap.parse_args()
    data = build()
    json_text = json.dumps(data, indent=2, ensure_ascii=False) + "\n"
    md_text = render(data)
    status = 0
    for p in data["problems"]:
        print(f"PROBLEM: {p}", file=sys.stderr)
        status = 1
    for g in data["gaps"]:
        print(f"GAP: {g}", file=sys.stderr)
        status = 1
    c, pc = data["counts"], data["paragraphs"]
    summary = (", ".join(f"{k} {c[k]}/{data['expected'][k]}" for k in c)
               + f"; paragraphs {pc['total']} (mapped {pc['mapped']}, retired-cics {pc['retired-cics']}, "
                 f"retired-jcl {pc['retired-jcl']}, folded {pc['folded']}, GAP {pc['GAP']})")
    if args.check:
        with tempfile.TemporaryDirectory() as tmp:
            out = Path(tmp)
            (out / MD_NAME).write_text(md_text, encoding="utf-8")
            (out / JSON_NAME).write_text(json_text, encoding="utf-8")
            for name in (MD_NAME, JSON_NAME):
                committed = OUT_DIR / name
                fresh = (out / name).read_text(encoding="utf-8")
                old = committed.read_text(encoding="utf-8") if committed.exists() else ""
                if old != fresh:
                    status = 1
                    print(f"STALE: {rel(committed)} differs from the generated file (run `make traceability`)",
                          file=sys.stderr)
                    diff = difflib.unified_diff(old.splitlines(), fresh.splitlines(), rel(committed), "generated",
                                                lineterm="", n=1)
                    for i, line in enumerate(diff):
                        if i >= 40:
                            print("...", file=sys.stderr)
                            break
                        print(line, file=sys.stderr)
        print(f"traceability: {summary}; {'OK' if status == 0 else 'FAILED'}")
        return status
    out = args.out_dir or OUT_DIR
    out.mkdir(parents=True, exist_ok=True)
    (out / MD_NAME).write_text(md_text, encoding="utf-8")
    (out / JSON_NAME).write_text(json_text, encoding="utf-8")
    print(f"wrote {out / MD_NAME} and {out / JSON_NAME}")
    print(f"traceability: {summary}")
    return status


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Failure as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        sys.exit(2)
