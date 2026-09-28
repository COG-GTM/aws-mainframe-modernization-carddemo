"""POSTTRAN (CBTRN02C) and INTCALC (CBACT04C): Java batch jobs vs the original COBOL compiled with GnuCOBOL.

Both sides run on app/data/ASCII: the COBOL via aws/batch/golden/generate-golden.sh (file assignment through
DD_* environment variables, app/ untouched), the Java jobs via carddemo-batch.jar against a dedicated database
created from aws/db/schema.sql. CREASTMT and COMBTRAN are smoke-tested on the chained result.
"""

from __future__ import annotations

import collections
import csv
import gzip
import io
import json
import os
import re
import shutil
import subprocess
from dataclasses import dataclass, field
from decimal import Decimal
from pathlib import Path

import psycopg
import pytest
from psycopg import conninfo

from conftest import DB_DSN, REPO_ROOT

from . import records

pytestmark = pytest.mark.batch

BATCH_DIR = REPO_ROOT / "aws" / "batch"
JAR = BATCH_DIR / "target" / "carddemo-batch.jar"
WORK = Path(os.environ.get("VALIDATION_WORK_DIR", REPO_ROOT / "aws" / "validation" / "target"))
BATCH_DB = os.environ.get("VALIDATION_BATCH_DB", "carddemo_batch")  # suffixed _sample / _synthetic
BUSINESS_DATE = "2022-07-18"
PARM_DATE = "2022071800"
SEED_ORDER = ["customer", "account", "card", "card_xref", "transaction_type", "transaction_category",
              "disclosure_group", "tran_cat_balance"]


@dataclass
class BatchRun:
    bucket: Path
    cobol: Path
    db: psycopg.Connection
    db_name: str
    work: Path
    results: dict[str, dict] = field(default_factory=dict)
    exit_codes: dict[str, int] = field(default_factory=dict)
    accounts_after_post: list[tuple] = field(default_factory=list)

    def run(self, job: str, run_id: str, *extra: str) -> dict:
        dsn = conninfo.conninfo_to_dict(DB_DSN)
        env = {**os.environ, "DB_HOST": str(dsn.get("host", "localhost")), "DB_PORT": str(dsn.get("port", 5432)),
               "DB_NAME": self.db_name, "DB_USER": str(dsn.get("user")), "DB_PASSWORD": str(dsn.get("password", "")),
               "DB_SCHEMA": "carddemo"}
        java = Path(os.environ["JAVA_HOME"], "bin", "java") if os.environ.get("JAVA_HOME") else "java"
        log = self.work / "logs" / f"{run_id}-{job}.log"
        log.parent.mkdir(parents=True, exist_ok=True)
        with log.open("w") as out:
            proc = subprocess.run(
                [str(java), "-jar", str(JAR), "--spring.profiles.active=local",
                 f"--carddemo.storage.local-dir={self.bucket}", f"--job={job}", f"--runId={run_id}",
                 f"--businessDate={BUSINESS_DATE}", *extra],
                env=env, stdout=out, stderr=subprocess.STDOUT, timeout=600, check=False)
        self.exit_codes[f"{run_id}/{job}"] = proc.returncode
        result = json.loads((self.bucket / "runs" / run_id / f"{job}.json").read_text())
        self.results[f"{run_id}/{job}"] = result
        return result

    def read(self, key: str) -> str:
        return (self.bucket / key).read_text()

    def golden(self, name: str) -> list[str]:
        return records.lines((self.cobol / name).read_text())


def _prepare(name: str, dalytran: Path) -> BatchRun:
    """GnuCOBOL ground truth + a seeded Java database/bucket for one DALYTRAN input."""
    cobol = WORK / name / "golden-cobol"
    shutil.rmtree(WORK / name, ignore_errors=True)
    subprocess.run([str(BATCH_DIR / "golden" / "generate-golden.sh")], check=True, timeout=600,
                   env={**os.environ, "GOLDEN_OUT": str(cobol), "WORK_DIR": str(WORK / name / "golden-work"),
                        "DALYTRAN_IN": str(dalytran)},
                   stdout=subprocess.DEVNULL)

    bucket = WORK / name / "batch-bucket"
    shutil.copytree(REPO_ROOT / "app" / "data" / "ASCII", bucket / "seed" / "ascii")
    (bucket / "input" / "dalytran" / BUSINESS_DATE).mkdir(parents=True)
    shutil.copy(dalytran, bucket / "input" / "dalytran" / BUSINESS_DATE / "dalytran.txt")

    db_name = f"{BATCH_DB}_{name}"
    admin = conninfo.make_conninfo(DB_DSN, dbname="postgres")
    with psycopg.connect(admin, autocommit=True) as conn:
        conn.execute(f'DROP DATABASE IF EXISTS "{db_name}" WITH (FORCE)')
        conn.execute(f'CREATE DATABASE "{db_name}"')
    conn = psycopg.connect(conninfo.make_conninfo(DB_DSN, dbname=db_name), autocommit=True)
    conn.execute((REPO_ROOT / "aws" / "db" / "schema.sql").read_text())
    conn.execute("SET search_path TO carddemo")
    run = BatchRun(bucket=bucket, cobol=cobol, db=conn, db_name=db_name, work=WORK / name)
    for table in SEED_ORDER:
        assert run.run("load-reference-data", f"seed-{table}", f"--table={table}")["returnCode"] == 0
    return run


@pytest.fixture(scope="module")
def batch() -> BatchRun:
    assert JAR.is_file(), f"{JAR} missing: build aws/batch first (run.sh does this)"
    assert shutil.which("cobc"), "GnuCOBOL (cobc) is required for the batch ground truth"
    run = _prepare("sample", REPO_ROOT / "app" / "data" / "ASCII" / "dailytran.txt")
    run.run("post-daily-transactions", "val-post")
    run.accounts_after_post = run.db.execute(records.ACCOUNT_SQL).fetchall()
    run.run("backup-transactions", "val-post")
    run.run("calculate-interest", "val-int", f"--parmDate={PARM_DATE}")
    run.run("combine-transactions", "val-comb", f"--backupKey=backup/transaction/{BUSINESS_DATE}/val-post.csv.gz")
    run.run("create-statements", "val-stmt")
    yield run
    _write_summary(run)
    run.db.close()


def _synthetic_dalytran(base: str) -> str:
    """Sample DALYTRAN + records that hit the reject reasons absent from the sample (100, 103, 102+103)."""
    sample = [line.rstrip("\r").ljust(350) for line in base.splitlines() if line.strip()]
    posted = sample[0]
    extra = [
        "9999999999999901" + posted[16:262] + "9999999999999999" + posted[278:],       # unknown card -> 100
        "9999999999999902" + posted[16:278] + "2099-01-01 00:00:00.000000" + posted[304:],  # after expiry -> 103
        # overlimit AND expired: CBTRN02C moves 102 then 103, so the later edit wins
        "9999999999999903" + posted[16:132] + "9999999999{" + posted[143:278] + "2099-01-01 00:00:00.000000"
        + posted[304:],
    ]
    return "\n".join(sample + extra) + "\n"


@pytest.fixture(scope="module")
def synthetic() -> BatchRun:
    path = WORK / "dalytran-synthetic.txt"
    WORK.mkdir(parents=True, exist_ok=True)
    path.write_text(_synthetic_dalytran((REPO_ROOT / "app" / "data" / "ASCII" / "dailytran.txt").read_text()))
    run = _prepare("synthetic", path)
    run.run("post-daily-transactions", "val-post")
    yield run
    run.db.close()


def _write_summary(run: BatchRun) -> None:
    summary = {"results": run.results, "exitCodes": run.exit_codes,
               "rejectReasons": dict(collections.Counter(
                   f"{r[1]} {r[2]}" for r in map(records.reject, run.golden("posttran/dalyrejs.txt"))))}
    (WORK / "batch-summary.json").write_text(json.dumps(summary, indent=2, default=str))


def _diff(expected: list[tuple], actual: list[tuple], names: list[str] | None = None) -> list[str]:
    out = []
    for e, a in zip(expected, actual):
        if e != a:
            labels = names or [str(i) for i in range(len(e))]
            fields = [f"{labels[i]}: cobol={e[i]!r} java={a[i]!r}" for i in range(len(e)) if e[i] != a[i]]
            out.append(f"key {e[0]}: " + "; ".join(fields))
    if len(expected) != len(actual):
        out.append(f"row count cobol={len(expected)} java={len(actual)}")
    return out


# ---------------------------------------------------------------- ground truth provenance

def test_fresh_gnucobol_run_matches_committed_golden(batch):
    committed = BATCH_DIR / "src" / "test" / "resources" / "golden"
    for f in sorted(p.relative_to(committed) for p in committed.rglob("*.txt")):
        assert (batch.cobol / f).read_text() == (committed / f).read_text(), f


# ---------------------------------------------------------------- POSTTRAN / CBTRN02C

def test_posttran_return_code_and_counts(batch):
    result = batch.results["val-post/post-daily-transactions"]
    assert result["returnCode"] == int(batch.golden("posttran/returncode.txt")[0]) == 4
    assert batch.exit_codes["val-post/post-daily-transactions"] == 0  # RC 4 is a warning: the cycle continues
    processed, rejected = (int(line.split(":")[1]) for line in batch.golden("posttran/counts.txt"))
    assert (result["counts"]["processed"], result["counts"]["rejected"]) == (processed, rejected) == (300, 38)
    assert result["counts"]["posted"] == processed - rejected


def test_posttran_rejects_and_reason_codes(batch):
    expected = batch.golden("posttran/dalyrejs.txt")
    actual = records.lines(batch.read(f"output/dalyrejs/{BUSINESS_DATE}/val-post.txt"))
    assert [records.reject(x) for x in actual] == [records.reject(x) for x in expected]
    assert actual == expected  # byte-identical 430-byte records
    # the sample DALYTRAN only exercises reason 102; 100 / 103 are covered by test_posttran_synthetic_rejects
    assert collections.Counter((r[1], r[2]) for r in map(records.reject, expected)) == {
        (102, "OVERLIMIT TRANSACTION"): 38}


def test_posttran_account_balances(batch):
    expected = [records.account(x) for x in batch.golden("posttran/acctdata.txt")]
    assert _diff(expected, batch.accounts_after_post, records.ACCOUNT_FIELDS) == []


def test_posttran_tran_cat_balance(batch):
    # INTCALC does not touch TCATBAL, so the final table must still equal the CBTRN02C output
    expected = [records.tcatbal(x) for x in batch.golden("posttran/tcatbal.txt")]
    assert _diff(expected, batch.db.execute(records.TCATBAL_SQL).fetchall()) == []


def test_posttran_posted_transactions(batch):
    expected = [records.transaction(x) for x in batch.golden("posttran/transact.txt")]
    actual = batch.db.execute(records.TRANSACTION_SQL.format(where="WHERE source <> 'System'")).fetchall()
    assert len(expected) == 262
    assert _diff(expected, actual) == []


def test_posttran_synthetic_rejects(synthetic):
    result = synthetic.results["val-post/post-daily-transactions"]
    processed, rejected = (int(line.split(":")[1]) for line in synthetic.golden("posttran/counts.txt"))
    assert (result["counts"]["processed"], result["counts"]["rejected"]) == (processed, rejected) == (303, 41)
    assert result["returnCode"] == int(synthetic.golden("posttran/returncode.txt")[0]) == 4
    expected = synthetic.golden("posttran/dalyrejs.txt")
    actual = records.lines(synthetic.read(f"output/dalyrejs/{BUSINESS_DATE}/val-post.txt"))
    assert actual == expected
    assert [records.reject(x)[:2] for x in expected[-3:]] == [
        ("9999999999999901", 100), ("9999999999999902", 103), ("9999999999999903", 103)]
    cobol_accounts = [records.account(x) for x in synthetic.golden("posttran/acctdata.txt")]
    assert _diff(cobol_accounts, synthetic.db.execute(records.ACCOUNT_SQL).fetchall(), records.ACCOUNT_FIELDS) == []


# ---------------------------------------------------------------- INTCALC / CBACT04C

def test_intcalc_return_code(batch):
    result = batch.results["val-int/calculate-interest"]
    assert result["returnCode"] == int(batch.golden("intcalc/returncode.txt")[0]) == 0


def _mask(line: str) -> str:
    return line[:278] + "ORIG-TS-MASKED            PROC-TS-MASKED            " + line[330:]


def test_intcalc_interest_transactions(batch):
    expected = batch.golden("intcalc/systran.txt")
    actual = [_mask(x) for x in records.lines(batch.read(f"output/systran/{BUSINESS_DATE}/val-int.txt"))]
    assert len(expected) == 50
    assert [records.transaction(x) for x in actual] == [records.transaction(x) for x in expected]
    assert actual == expected
    # and the same 50 rows are in the transaction table after COMBTRAN
    rows = batch.db.execute(records.TRANSACTION_SQL.format(where="WHERE source = 'System'")).fetchall()
    assert [r[:11] for r in rows] == [records.transaction(x)[:11] for x in expected]


def test_intcalc_account_balances(batch):
    """CBACT04C never runs 1050-UPDATE-ACCOUNT for the last TCATBAL account (the EOF branch is unreachable after
    the final READ); the Java job does. All other accounts must match byte for byte; the last one must equal
    balance-after-POSTTRAN + its interest with cycle totals reset (derived from the COBOL formula)."""
    expected = [records.account(x) for x in batch.golden("intcalc/acctdata.txt")]
    actual = batch.db.execute(records.ACCOUNT_SQL).fetchall()
    last = batch.db.execute("SELECT max(acct_id) FROM tran_cat_balance").fetchone()[0]
    others = [(e, a) for e, a in zip(expected, actual) if e[0] != last]
    assert _diff([e for e, _ in others], [a for _, a in others], records.ACCOUNT_FIELDS) == []

    before = next(a for a in batch.accounts_after_post if a[0] == last)
    cobol_last = next(e for e in expected if e[0] == last)
    java_last = next(a for a in actual if a[0] == last)
    interest = batch.db.execute("SELECT coalesce(sum(amt), 0) FROM transaction WHERE description = %s",
                                (f"Int. for a/c {int(last):011d}",)).fetchone()[0]
    assert cobol_last == before  # the legacy defect: untouched
    assert java_last[2] == before[2] + interest and java_last[8] == 0 and java_last[9] == 0


def test_intcalc_interest_formula(batch):
    """Hand check of 1300-COMPUTE-INTEREST: (TRAN-CAT-BAL * DIS-INT-RATE) / 1200, per TCATBAL row, DEFAULT group
    fallback when the account group has no disclosure row."""
    rows = batch.db.execute("""
        SELECT b.acct_id, b.balance, coalesce(d.int_rate, dd.int_rate) FROM tran_cat_balance b
        JOIN account a ON a.acct_id = b.acct_id
        LEFT JOIN disclosure_group d ON d.acct_group_id = a.group_id AND d.type_cd = b.type_cd
                                    AND d.cat_cd = b.cat_cd
        LEFT JOIN disclosure_group dd ON dd.acct_group_id = 'DEFAULT' AND dd.type_cd = b.type_cd
                                     AND dd.cat_cd = b.cat_cd
        ORDER BY b.acct_id""").fetchall()
    per_acct: dict[int, Decimal] = collections.defaultdict(Decimal)
    for acct, bal, rate in rows:
        if rate:
            per_acct[acct] += (bal * rate / 1200).quantize(Decimal("0.01"), rounding="ROUND_DOWN")
    systran = {int(t[4][-11:]): t[5] for t in map(records.transaction, batch.golden("intcalc/systran.txt"))}
    assert {a: v for a, v in per_acct.items() if v} == {a: v for a, v in systran.items() if v}


# ---------------------------------------------------------------- COMBTRAN / CREASTMT smoke

def test_combtran_smoke(batch):
    """COMBTRAN: SORT TRANSACT.BKUP + SYSTRAN by TRAN-ID, REPRO into TRANSACT -> 262 posted + 50 interest rows."""
    result = batch.results["val-comb/combine-transactions"]
    assert result["returnCode"] == 0
    assert (result["counts"]["backupRows"], result["counts"]["systemTransactions"],
            result["counts"]["transactionRows"]) == (262, 50, 312)
    backup = list(csv.reader(io.StringIO(gzip.decompress(
        (batch.bucket / "backup" / "transaction" / BUSINESS_DATE / "val-post.csv.gz").read_bytes()).decode())))
    expected_ids = sorted({r[0] for r in backup[1:]} | {x[:16] for x in batch.golden("intcalc/systran.txt")})
    assert [r[0] for r in batch.db.execute("SELECT tran_id FROM transaction ORDER BY tran_id")] == expected_ids


def test_creastmt_smoke(batch):
    """CREASTMT (CBSTM03A/B): one statement per CARDXREF record, 80-byte text lines, Total EXP = sum of the card's
    TRANSACT amounts (hand-derived from the combined transaction table)."""
    result = batch.results["val-stmt/create-statements"]
    xrefs = batch.db.execute("SELECT count(*) FROM card_xref").fetchone()[0]
    assert result["returnCode"] == 0 and result["counts"]["statements"] == xrefs == 50
    assert result["counts"]["transactions"] == 312
    text = batch.read(f"statements/{BUSINESS_DATE}/val-stmt/statement.txt")
    assert all(len(line) == 80 for line in text.splitlines())
    accounts = re.findall(r"^Account ID\s*:(\d{11})", text, re.M)
    totals = [Decimal(sign + amount.replace(",", "")) for amount, sign in
              re.findall(r"^Total EXP:\s*\$\s*([\d.,]+)(-?)\s*$", text, re.M)]
    assert len(accounts) == len(totals) == 50
    expected = dict(batch.db.execute(
        "SELECT x.acct_id, coalesce(sum(t.amt), 0) FROM card_xref x LEFT JOIN transaction t "
        "ON t.card_num = x.card_num GROUP BY x.acct_id").fetchall())
    assert {int(a): t for a, t in zip(accounts, totals)} == {int(a): v for a, v in expected.items()}
    html = batch.read(f"statements/{BUSINESS_DATE}/val-stmt/statement.html")
    assert html.count("<h3>Statement for Account Number:") == 50
