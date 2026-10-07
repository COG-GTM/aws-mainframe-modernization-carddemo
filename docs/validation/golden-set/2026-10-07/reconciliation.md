# Golden-set reconciliation: online scenario + nightly cycle, Java vs GnuCOBOL

**Verdict: PASS** — zero unexplained differences; every explained difference is an allow-list entry under `scripts/golden-set/expected-diffs/` that matched exactly once.

Regenerate (Docker, GnuCOBOL 3.1.2, Java 21; about two minutes on the sample data):

```
make golden-set    # = scripts/golden-set/run_golden_set.sh
```

| comparison | result |
|---|---|
| Online changes: independent expected files vs Java export (6 datasets) | PASS |
| Online report download vs COBOL TRANREPT for the same window | byte-identical |
| Nightly cycle, every job (SYSOUT, outputs, after-images, RC) | PASS |
| Final datasets after the cycle (7 datasets, field by field) | PASS |

Companion documents: [what this does not prove](what-this-does-not-prove.md) · [online-change comparison](online-changes.md) · [final datasets](final-datasets.md) · [nightly-cycle job matrix and per-group compare reports](nightly-cycle.md) · [online report download](online-TRANREPT.txt).

## 1. Starting data and run parameters

| side | starting data | how the online scenario is applied | batch cycle |
|---|---|---|---|
| Java | fresh `postgres:16-alpine` container, `--job=initial-load --mode=REPLACE` from `app/data/EBCDIC` (rows 10/7/18/51/50/50/50/50/50/0/300 in user_security / transaction_type / transaction_category / disclosure_group / customer / account / card / card_xref / tran_cat_balance / transaction / daily_transaction) | `carddemo-app` web under the `golden` profile, REST calls of `online_scenario.sh`; then `--job=unload` of ACCTDATA CUSTDATA CARDDATA CARDXREF TRANSACT USRSEC | `--job=nightly-cycle --run-date=2022-07-06` (table mode, one launch, through `scripts/batch/run_nightly_cycle.sh table` with `NIGHTLY_CYCLE_SKIP_LOAD=1`) |
| COBOL | `app/data/ASCII` (the baseline's input; USRSEC from the `app/data/EBCDIC` sample decoded as IBM-037, there is no ASCII twin), TRANSACT empty | `apply_online_scenario.py`: the scenario applied to the fixed-width records from the rules in `docs/modernization/rules/` with the copybook offsets — no Java code, no Java API | GnuCOBOL baseline machinery (`cobol_cycle.py` → `scripts/baseline/baseline.py`): IDCAMS loads of the after-online files, then the same 11 jobs (+ CBTRN01C, which Java runs as POSTTRAN STEP10) |

Both sides: business clock 2022-07-06 (ADR-0014: `COB_CURRENT_DATE=2022-07-06`, `golden` profile `carddemo.clock.fixed=2022-07-06T00:00:00`), INTCALC PARM `2022071800`, TRANREPT DATEPARM 2022-01-01..2022-07-06. The `golden` profile turns asynchronous reports off (`carddemo.reports.async.enabled=false`), so the Custom report request runs the `tranrept` stream synchronously inside `POST /api/v1/reports/transactions` (ADR-0021) and returns 202 with an execution that is already `COMPLETED`; the run keeps that default. The app runs with `carddemo.reports.encoding=ASCII` so the download is comparable with the GnuCOBOL (ASCII) TRANREPT.

Toolchain: `openjdk version "21.0.12.1" 2026-08-18` · `cobc (GnuCOBOL) 3.1.2.0`.

## 2. Online scenario

Inputs: [`scripts/golden-set/scenario.json`](../../../../scripts/golden-set/scenario.json). Every step asserts its HTTP status; the full request/response transcript is [Appendix A](#appendix-a-requestresponse-transcript).

| step | request | expected | actual |
|---|---|---|---|
| S01 | `POST /auth/login` | 200 | 200 |
| S02 | `POST /auth/login` | 200 | 200 |
| S03 | `GET /menu/main` | 200 | 200 |
| S04 | `GET /menu/admin` | 200 | 200 |
| S05 | `GET /accounts/00000000001` | 200 | 200 |
| S06 | `GET /accounts/00000000010` | 200 | 200 |
| S07 | `PUT /accounts/00000000010` | 200 | 200 |
| S08 | `PUT /accounts/00000000010` | 200 | 200 |
| S09 | `GET /cards?accountId=00000000010` | 200 | 200 |
| S10 | `GET /cards/3260763612337560?accountId=00000000010` | 200 | 200 |
| S11 | `PUT /cards/3260763612337560` | 200 | 200 |
| S12 | `PUT /cards/3260763612337560` | 200 | 200 |
| S13 | `GET /transactions` | 200 | 200 |
| S14 | `POST /transactions` | 201 | 201 |
| S15 | `POST /transactions` | 201 | 201 |
| S16 | `GET /transactions` | 200 | 200 |
| S17 | `GET /transactions/0000000000000001` | 200 | 200 |
| S18 | `POST /accounts/00000000002/bill-payment` | 200 | 200 |
| S19 | `POST /accounts/00000000002/bill-payment` | 200 | 200 |
| S20 | `GET /accounts/00000000002` | 200 | 200 |
| S21 | `GET /transactions` | 200 | 200 |
| S22 | `POST /users` | 201 | 201 |
| S23 | `GET /users/GOLDEN01?fromProgram=COUSR00C` | 200 | 200 |
| S24 | `PUT /users/GOLDEN01` | 200 | 200 |
| S25 | `DELETE /users/USER0005?fromProgram=COUSR00C` | 200 | 200 |
| S26 | `DELETE /users/USER0005?confirm=Y&version=0` | 200 | 200 |
| S27 | `GET /users` | 200 | 200 |
| S28 | `POST /reports/transactions` | 202 | 202 |
| S29 | `GET /reports/transactions/1` | 200 | 200 |
| S30 | `GET /reports/transactions/1/report` | 200 | 200 |

Records the independent COBOL-side transformer wrote (`apply_online_scenario.py`, rule ids from `docs/modernization/rules/<PGM>.md`):

| # | verb | dataset | key | rule |
|---|---|---|---|---|
| 1 | REWRITE | ACCTDATA | 00000000010 | COACTUPC R-39 (ACCT-UPDATE-RECORD) |
| 2 | REWRITE | CUSTDATA | 000000010 | COACTUPC R-39 (CUST-UPDATE-RECORD) |
| 3 | REWRITE | CARDDATA | 3260763612337560 | COCRDUPC R-30 |
| 4 | WRITE | TRANSACT | 0000000000000001 | COTRN02C R-28/R-29 |
| 5 | WRITE | TRANSACT | 0000000000000002 | COTRN02C R-28/R-29 |
| 6 | WRITE | TRANSACT | 0000000000000003 | COBIL00C R-12 (payment transaction) |
| 7 | REWRITE | ACCTDATA | 00000000002 | COBIL00C R-12 (balance 0) |
| 8 | WRITE | USRSEC | GOLDEN01 | COUSR01C R-13 |
| 9 | REWRITE | USRSEC | GOLDEN01 | COUSR02C R-17 |
| 10 | DELETE | USRSEC | USER0005 | COUSR03C DELETE |

## 3. Online-change comparison (the online equivalence proof)

The six datasets the online programs maintain, expected (COBOL rules, pristine samples) vs exported (Java, after the REST scenario), matched by key, every field of every record compared (`compare_datasets.py`, layouts parsed from `app/cpy/*.cpy`).

### Online changes: independent expected files vs Java export

COBOL side: `build/golden-set/cobol/after-online` · Java side: `build/golden-set/java/after-online` · allow-list: `scripts/golden-set/expected-diffs/online.txt` (2 entries)

| dataset | copybook | fields/record | COBOL records | Java records | records compared | differences | explained | unexplained |
|---|---|---|---|---|---|---|---|---|
| ACCTDATA | CVACT01Y | 13 | 50 | 50 | 50 | 2 | 2 | 0 |
| CUSTDATA | CVCUS01Y | 19 | 50 | 50 | 50 | 0 | 0 | 0 |
| CARDDATA | CVACT02Y | 7 | 50 | 50 | 50 | 0 | 0 | 0 |
| CARDXREF | CVACT03Y | 4 | 50 | 50 | 50 | 0 | 0 | 0 |
| TRANSACT | CVTRA05Y | 14 | 3 | 3 | 3 | 0 | 0 | 0 |
| USRSEC | CSUSR01Y | 6 | 10 | 10 | 10 | 0 | 0 | 0 |

| dataset | key | field | COBOL value | Java value | explained by |
|---|---|---|---|---|---|
| ACCTDATA | 00000000010 | ACCT-ADDR-ZIP | `''` | `A000000000` | docs/modernization/rules/COACTUPC.md "Deviation (R-39, ACCT-ADDR-ZIP)": COBOL ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the update writes the (blank) group id over the ZIP; Java keeps the stored ZIP |
| ACCTDATA | 00000000049 | ACCT-ADDR-ZIP | `A000000000` | `ZEROAPR` | Sample data, not behaviour: ACCT-ADDR-ZIP is A000000000 in app/data/ASCII (COBOL input) and ZEROAPR in app/data/EBCDIC (initial-load input); modernization/carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt |

Result: **PASS** — 2 differences, 2 explained, 0 unexplained, 0 allow-list entries not matched exactly once.

## 4. Online report download vs batch TRANREPT

The Custom report for 2022-01-01..2022-07-06 downloaded from `GET /api/v1/reports/transactions/{id}/report` after the scenario, against the TRANREPT generation GnuCOBOL wrote (`ONLINE-TRANREPT`: the TRANREPT job, IDCAMS REPRO + SORT + CBTRN03C, on the expected after-online TRANSACT, before the cycle).

```
online 65218b40b16ac761b192ae317045485a57f0dee84f1a373146f639ecce4c1d10 1862 bytes
cobol  65218b40b16ac761b192ae317045485a57f0dee84f1a373146f639ecce4c1d10 1862 bytes
IDENTICAL
```

## 5. Nightly cycle: per-job comparison

Each job compared with the same compare script as `make batch-equivalence`, against the golden GnuCOBOL run (`CARDDEMO_BASELINE_DIR`) instead of the committed baseline: SYSOUT, every output generation, the KSDS after-images and the RC. Records compared = records of the GnuCOBOL outputs of the job (each matched against the Java output) plus its SYSOUT lines.

Cycle RC (JCL max): Java 4 · GnuCOBOL 4.

| job | COBOL RC | Java RC | records compared (COBOL outputs) | SYSOUT lines | compare | differences | explained by | result |
|---|---|---|---|---|---|---|---|---|
| READACCT | 0 | 0 | 200 (ARRYFILE 50, OUTFILE 50, VBRCFILE 100) | 757 | `compare_print_jobs.py` | 2 | READACCT/sysout: expected diff on line `00000000010Y...` `00000000000{00000000000{` -> `00000000000{00000000000{A000000000`: applied<br>READACCT/sysout: expected diff on line `00000000049...` `A000000000` -> `ZEROAPR   `: applied | PASS |
| READCARD | 0 | 0 | 0 (-) | 54 | `compare_print_jobs.py` | 0 | - | PASS |
| READCUST | 0 | 0 | 0 (-) | 104 | `compare_print_jobs.py` | 0 | - | PASS |
| READXREF | 0 | 0 | 0 (-) | 104 | `compare_print_jobs.py` | 0 | - | PASS |
| CBTRN01C | 0 | POSTTRAN STEP10 | 0 (-) | 1809 | `compare_posttran.py` | 0 | - | PASS |
| POSTTRAN | 4 | 4 | 450 (ACCTDATA.ksds 50, DALYREJS 38, TCATBALF.ksds 100, TRANSACT.ksds 262) | 65 | `compare_posttran.py` | 3 | ACCTDATA record 10 ACCT-ADDR-ZIP: baseline `` / Java `A000000000` (documented input-data difference)<br>ACCTDATA record 49 ACCT-ADDR-ZIP: baseline `A000000000` / Java `ZEROAPR` (documented input-data difference)<br>TCATBALF: FILLER differs in 100 record(s); FILLER is not persisted in table mode (ADR-0011), so the unload writes spaces | PASS |
| INTCALC | 0 | 0 | 100 (ACCTDATA.ksds 50, TRANSACT 50) | 308 | `compare_intcalc.py` | 5 | SYSOUT: 100 DISPLAYed TCATBALF images end in spaces instead of the 22-zero FILLER of the sample (not persisted in table mode, ADR-0011)<br>ACCTDATA record 10 ACCT-ADDR-ZIP: baseline `` / Java `A000000000` (documented input-data difference)<br>ACCTDATA record 49 ACCT-ADDR-ZIP: baseline `A000000000` / Java `ZEROAPR` (documented input-data difference)<br>TCATBALF: FILLER differs in 100 record(s); FILLER is not persisted in table mode (ADR-0011), so the unload writes spaces<br>DISCGRP record 34 (DEFAULT/07/0001) has DIS-INT-RATE 15.00 in the EBCDIC sample loaded by initial-load vs 0.00 in the ASCII sample of the baseline; 0 TCATBALF row(s) look it up (no TCATBALF row has type 07), so it cannot change SYSTRAN or ACCTDATA | PASS |
| TRANBKP | 0 | 0 | 262 (TRANSACT.BKUP 262, TRANSACT.ksds 0) | 9 | `compare_tranrept.py` | 0 | - | PASS |
| COMBTRAN | 0 | 0 | 624 (TRANSACT.COMBINED 312, TRANSACT.ksds 312) | 7 | `compare_tranrept.py` | 0 | - | PASS |
| TRANREPT | 0 | 0 | 1143 (TRANREPT 519, TRANSACT.BKUP 312, TRANSACT.DALY 312) | 323 | `compare_tranrept.py` | 0 | - | PASS |
| CREASTMT | 0 | 0 | 8206 (HTMLFILE 6632, STMTFILE 1262, TRXFL.SEQ 312) | 7 | `compare_creastmt.py` | 1 | CBSTM03A SYSOUT: baseline `Running JCL : CREASTMT  Step STEP040 (TIOT walk bypassed under GnuCOBOL)` (build-time TIOT patch) / Java `Running JCL : CREASTMT Step STEP040` (CBSTM03A.md R-2) | PASS |
| PRTCATBL | 0 | 0 | 200 (TCATBALF.BKUP 100, TCATBALF.REPT 100) | 4 | `compare_tranrept.py` | 1 | TCATBALF: FILLER differs in 100 record(s); FILLER is not persisted in table mode (ADR-0011), so the unload writes spaces | PASS |

POSTTRAN ends RC 4 on both sides because CBTRN02C rejects 38 of the 300 DALYTRAN records (DALYREJS; the committed baseline rejects 38): the online scenario writes TRANSACT, not DALYTRAN, so the daily input and its reject reasons (overlimit / expired card) are the sample's. INTCALC and the later jobs run because the cycle's COND only bypasses on RC > 4 (ADR-0016).

Not run (same list as `make nightly-cycle`): CLOSEFIL/OPENFIL/WAITSTEP (retired, 06-scheduling.md), CBPAUP0J (IMS/DB2 extension), TXT2PDF1 (retired), TRANTYPE/TRANCATG/TCATBALF/DISCGRP refresh loads (initial-load / repro, not nightly), TRANEXTR/MNTTRDB2 (Db2 extension).

## 6. Final datasets after the cycle

GnuCOBOL KSDS unloaded after PRTCATBL vs `--job=unload` of the Java tables after the cycle.

### Final datasets after the nightly cycle: GnuCOBOL vs Java

COBOL side: `build/golden-set/cobol/cycle/FINAL` · Java side: `build/golden-set/java/final` · allow-list: `scripts/golden-set/expected-diffs/final.txt` (3 entries)

| dataset | copybook | fields/record | COBOL records | Java records | records compared | differences | explained | unexplained |
|---|---|---|---|---|---|---|---|---|
| ACCTDATA | CVACT01Y | 13 | 50 | 50 | 50 | 2 | 2 | 0 |
| CUSTDATA | CVCUS01Y | 19 | 50 | 50 | 50 | 0 | 0 | 0 |
| CARDDATA | CVACT02Y | 7 | 50 | 50 | 50 | 0 | 0 | 0 |
| CARDXREF | CVACT03Y | 4 | 50 | 50 | 50 | 0 | 0 | 0 |
| TRANSACT | CVTRA05Y | 14 | 312 | 312 | 312 | 0 | 0 | 0 |
| TCATBALF | CVTRA01Y | 5 | 100 | 100 | 100 | 100 | 100 | 0 |
| USRSEC | CSUSR01Y | 6 | 10 | 10 | 10 | 0 | 0 | 0 |

| dataset | key | field | COBOL value | Java value | explained by |
|---|---|---|---|---|---|
| ACCTDATA | 00000000010 | ACCT-ADDR-ZIP | `''` | `A000000000` | docs/modernization/rules/COACTUPC.md "Deviation (R-39, ACCT-ADDR-ZIP)": COBOL ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the update writes the (blank) group id over the ZIP; Java keeps the stored ZIP |
| ACCTDATA | 00000000049 | ACCT-ADDR-ZIP | `A000000000` | `ZEROAPR` | Sample data, not behaviour: ACCT-ADDR-ZIP is A000000000 in app/data/ASCII (COBOL input) and ZEROAPR in app/data/EBCDIC (initial-load input); modernization/carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt |
| TCATBALF | every record (100) | FILLER | `0000000000000000000000` | `''` | docs/modernization/adr/ADR-0011-vsam-to-relational.md: FILLER is not persisted in table mode |

Result: **PASS** — 102 differences, 102 explained, 0 unexplained, 0 allow-list entries not matched exactly once.

## 7. Allow-list (every explained difference)

| file | entry | matched | justification |
|---|---|---|---|
| `expected-diffs/online.txt` | `ACCTDATA\|00000000010\|ACCT-ADDR-ZIP\|''\|A000000000` | 1 of 1 | docs/modernization/rules/COACTUPC.md "Deviation (R-39, ACCT-ADDR-ZIP)": COBOL ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the update writes the (blank) group id over the ZIP; Java keeps the stored ZIP |
| `expected-diffs/online.txt` | `ACCTDATA\|00000000049\|ACCT-ADDR-ZIP\|A000000000\|ZEROAPR` | 1 of 1 | Sample data, not behaviour: ACCT-ADDR-ZIP is A000000000 in app/data/ASCII (COBOL input) and ZEROAPR in app/data/EBCDIC (initial-load input); modernization/carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt |
| `expected-diffs/final.txt` | `ACCTDATA\|00000000010\|ACCT-ADDR-ZIP\|''\|A000000000` | 1 of 1 | docs/modernization/rules/COACTUPC.md "Deviation (R-39, ACCT-ADDR-ZIP)": COBOL ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the update writes the (blank) group id over the ZIP; Java keeps the stored ZIP |
| `expected-diffs/final.txt` | `ACCTDATA\|00000000049\|ACCT-ADDR-ZIP\|A000000000\|ZEROAPR` | 1 of 1 | Sample data, not behaviour: ACCT-ADDR-ZIP is A000000000 in app/data/ASCII (COBOL input) and ZEROAPR in app/data/EBCDIC (initial-load input); modernization/carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt |
| `expected-diffs/final.txt` | `TCATBALF\|*\|FILLER\|0000000000000000000000\|''` | 100 of 100 | docs/modernization/adr/ADR-0011-vsam-to-relational.md: FILLER is not persisted in table mode |
| `expected-diffs/cycle/print.txt` | `READACCT\|sysout\|00000000010Y\|00000000000{00000000000{\|00000000000{00000000000{A000000000` | 1 (group PASS) | Account 00000000010 (updated online): docs/modernization/rules/COACTUPC.md "Deviation (R-39, ACCT-ADDR-ZIP)": COBOL ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the update writes the (blank) group id over the ZIP; Java keeps the stored ZIP. |
| `expected-diffs/cycle/print.txt` | `READACCT\|sysout\|00000000049\|A000000000\|ZEROAPR` | 1 (group PASS) | Account 00000000049: Sample data, not behaviour: ACCT-ADDR-ZIP is A000000000 in app/data/ASCII (COBOL input) and ZEROAPR in app/data/EBCDIC (initial-load input); modernization/carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt. |
| `expected-diffs/cycle/posttran.txt` | `ACCTDATA\|10\|ACCT-ADDR-ZIP\|\|A000000000` | 1 (group PASS) | ACCTDATA record 10 (account 00000000010, updated online): docs/modernization/rules/COACTUPC.md "Deviation (R-39, ACCT-ADDR-ZIP)": COBOL ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the update writes the (blank) group id over the ZIP; Java keeps the stored ZIP. |
| `expected-diffs/cycle/posttran.txt` | `ACCTDATA\|49\|ACCT-ADDR-ZIP\|A000000000\|ZEROAPR` | 1 (group PASS) | ACCTDATA record 49: Sample data, not behaviour: ACCT-ADDR-ZIP is A000000000 in app/data/ASCII (COBOL input) and ZEROAPR in app/data/EBCDIC (initial-load input); modernization/carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt. |
| `expected-diffs/cycle/intcalc.txt` | `ACCTDATA\|10\|ACCT-ADDR-ZIP\|\|A000000000` | 1 (group PASS) | ACCTDATA record 10 (account 00000000010, updated online): docs/modernization/rules/COACTUPC.md "Deviation (R-39, ACCT-ADDR-ZIP)": COBOL ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the update writes the (blank) group id over the ZIP; Java keeps the stored ZIP. |
| `expected-diffs/cycle/intcalc.txt` | `ACCTDATA\|49\|ACCT-ADDR-ZIP\|A000000000\|ZEROAPR` | 1 (group PASS) | ACCTDATA record 49: Sample data, not behaviour: ACCT-ADDR-ZIP is A000000000 in app/data/ASCII (COBOL input) and ZEROAPR in app/data/EBCDIC (initial-load input); modernization/carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt. |

Differences the compare scripts document without an entry (same behaviour as `make batch-equivalence`): TCATBALF FILLER (not persisted, ADR-0011) in after-images and the INTCALC SYSOUT images, DISCGRP record 34 (never read; the script counts the lookups), the CBSTM03A TIOT banner (build-time patch of the baseline, CBSTM03A.md R-2). They are listed per job in section 5.

## Appendix A. Request/response transcript

Scenario inputs: `scripts/golden-set/scenario.json`. Bearer tokens are redacted, wall-clock values (JWT expiry, report submission, batch_run step times) are shown as `<wall clock>`, the encrypted `cardRef` (random nonce, ADR-0020) as `<opaque, per run>`; sample passwords are the
plaintext values of the USRSEC sample file. Each step asserts its HTTP status (`steps.tsv`).

#### S01 sign on as USER0001 (COSGN00C)

```
POST /api/v1/auth/login
{
  "password": "PASSWORD",
  "userId": "USER0001"
}

HTTP 200
{
  "expiresAt": "<wall clock>",
  "navigation": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COSGN00C",
    "fromTranId": "CC00",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "role": "USER",
  "targetMenu": "COMEN01C",
  "targetMenuUrl": "/api/v1/menu/main",
  "token": "<redacted>",
  "tokenType": "Bearer",
  "userId": "USER0001",
  "userType": "U"
}
```

#### S02 sign on as ADMIN001 (COSGN00C)

```
POST /api/v1/auth/login
{
  "password": "PASSWORD",
  "userId": "ADMIN001"
}

HTTP 200
{
  "expiresAt": "<wall clock>",
  "navigation": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COSGN00C",
    "fromTranId": "CC00",
    "pgmContext": "ENTER",
    "toProgram": "COADM01C",
    "toTranId": "CA00"
  },
  "role": "ADMIN",
  "targetMenu": "COADM01C",
  "targetMenuUrl": "/api/v1/menu/admin",
  "token": "<redacted>",
  "tokenType": "Bearer",
  "userId": "ADMIN001",
  "userType": "A"
}
```

#### S03 USER0001 main menu (COMEN01C)

```
GET /api/v1/menu/main

HTTP 200
{
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COMEN01C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CM00"
  },
  "map": "COMEN1A",
  "mapset": "COMEN01",
  "menu": "main",
  "message": "",
  "optionLines": [
    "01. Account View",
    "02. Account Update",
    "03. Credit Card List",
    "04. Credit Card View",
    "05. Credit Card Update",
    "06. Transaction List",
    "07. Transaction View",
    "08. Transaction Add",
    "09. Transaction Reports",
    "10. Bill Payment",
    "11. Pending Authorization View",
    ""
  ],
  "options": [
    {
      "adminOnly": false,
      "label": "01. Account View",
      "name": "Account View",
      "number": 1,
      "programId": "COACTVWC"
    },
    {
      "adminOnly": false,
      "label": "02. Account Update",
      "name": "Account Update",
      "number": 2,
      "programId": "COACTUPC"
    },
    {
      "adminOnly": false,
      "label": "03. Credit Card List",
      "name": "Credit Card List",
      "number": 3,
      "programId": "COCRDLIC"
    },
    {
      "adminOnly": false,
      "label": "04. Credit Card View",
      "name": "Credit Card View",
      "number": 4,
      "programId": "COCRDSLC"
    },
    {
      "adminOnly": false,
      "label": "05. Credit Card Update",
      "name": "Credit Card Update",
      "number": 5,
      "programId": "COCRDUPC"
    },
    {
      "adminOnly": false,
      "label": "06. Transaction List",
      "name": "Transaction List",
      "number": 6,
      "programId": "COTRN00C"
    },
    {
      "adminOnly": false,
      "label": "07. Transaction View",
      "name": "Transaction View",
      "number": 7,
      "programId": "COTRN01C"
    },
    {
      "adminOnly": false,
      "label": "08. Transaction Add",
      "name": "Transaction Add",
      "number": 8,
      "programId": "COTRN02C"
    },
    {
      "adminOnly": false,
      "label": "09. Transaction Reports",
      "name": "Transaction Reports",
      "number": 9,
      "programId": "CORPT00C"
    },
    {
      "adminOnly": false,
      "label": "10. Bill Payment",
      "name": "Bill Payment",
      "number": 10,
      "programId": "COBIL00C"
    },
    {
      "adminOnly": false,
      "label": "11. Pending Authorization View",
      "name": "Pending Authorization View",
      "number": 11,
      "programId": "COPAUS0C"
    }
  ],
  "programId": "COMEN01C",
  "tranId": "CM00"
}
```

#### S04 ADMIN001 admin menu (COADM01C)

```
GET /api/v1/menu/admin

HTTP 200
{
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COADM01C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CA00"
  },
  "map": "COADM1A",
  "mapset": "COADM01",
  "menu": "admin",
  "message": "",
  "optionLines": [
    "01. User List (Security)",
    "02. User Add (Security)",
    "03. User Update (Security)",
    "04. User Delete (Security)",
    "05. Transaction Type List/Update (Db2)",
    "06. Transaction Type Maintenance (Db2)",
    "",
    "",
    "",
    "",
    "",
    ""
  ],
  "options": [
    {
      "adminOnly": false,
      "label": "01. User List (Security)",
      "name": "User List (Security)",
      "number": 1,
      "programId": "COUSR00C"
    },
    {
      "adminOnly": false,
      "label": "02. User Add (Security)",
      "name": "User Add (Security)",
      "number": 2,
      "programId": "COUSR01C"
    },
    {
      "adminOnly": false,
      "label": "03. User Update (Security)",
      "name": "User Update (Security)",
      "number": 3,
      "programId": "COUSR02C"
    },
    {
      "adminOnly": false,
      "label": "04. User Delete (Security)",
      "name": "User Delete (Security)",
      "number": 4,
      "programId": "COUSR03C"
    },
    {
      "adminOnly": false,
      "label": "05. Transaction Type List/Update (Db2)",
      "name": "Transaction Type List/Update (Db2)",
      "number": 5,
      "programId": "COTRTLIC"
    },
    {
      "adminOnly": false,
      "label": "06. Transaction Type Maintenance (Db2)",
      "name": "Transaction Type Maintenance (Db2)",
      "number": 6,
      "programId": "COTRTUPC"
    }
  ],
  "programId": "COADM01C",
  "tranId": "CA00"
}
```

#### S05 view account 00000000001 (COACTVWC)

```
GET /api/v1/accounts/00000000001

HTTP 200
{
  "accountVersion": 0,
  "acctId": "00000000001",
  "activeStatus": "Y",
  "addressLine1": "618 Deshaun Route",
  "addressLine2": "Apt. 802",
  "cardNum": "9680294154603697",
  "cardNumbers": [
    "9680294154603697"
  ],
  "cashCreditLimit": 1020,
  "city": "Altenwerthshire",
  "country": "USA",
  "creditLimit": 2020,
  "currentBalance": 194,
  "currentCycleCredit": 0,
  "currentCycleDebit": 0,
  "custId": "000000001",
  "customerVersion": 0,
  "dateOfBirth": "1961-06-08",
  "eftAccountId": "0053581756",
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COACTVWC",
    "fromTranId": "CAVW",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "expirationDate": "2025-05-20",
  "ficoScore": "274",
  "firstName": "Immanuel",
  "governmentId": "00000000000049368437",
  "groupId": "",
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COACTVWC",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CAVW"
  },
  "infoMessage": "Enter or update id of account to display",
  "lastName": "Kessler",
  "message": "",
  "middleName": "Madeline",
  "openDate": "2014-11-20",
  "phone1": "(908)119-8310",
  "phone2": "(373)693-8684",
  "primaryCardHolder": "Y",
  "reissueDate": "2025-05-20",
  "ssn": "020-97-3888",
  "state": "NC",
  "updateForm": {
    "accountVersion": 0,
    "activeStatus": "Y",
    "addressLine1": "618 Deshaun Route",
    "addressLine2": "Apt. 802",
    "cashCreditLimit": "1020.00",
    "city": "Altenwerthshire",
    "confirm": false,
    "creditLimit": "2020.00",
    "currentBalance": "194.00",
    "currentCycleCredit": "0.00",
    "currentCycleDebit": "0.00",
    "customerVersion": 0,
    "dateOfBirth": {
      "day": "08",
      "month": "06",
      "year": "1961"
    },
    "eftAccountId": "0053581756",
    "expiryDate": {
      "day": "20",
      "month": "05",
      "year": "2025"
    },
    "ficoScore": "274",
    "firstName": "Immanuel",
    "governmentId": "00000000000049368437",
    "groupId": "",
    "lastName": "Kessler",
    "middleName": "Madeline",
    "openDate": {
      "day": "20",
      "month": "11",
      "year": "2014"
    },
    "phone1": {
      "areaCode": "908",
      "lineNumber": "8310",
      "prefix": "119"
    },
    "phone2": {
      "areaCode": "373",
      "lineNumber": "8684",
      "prefix": "693"
    },
    "primaryCardHolder": "Y",
    "reissueDate": {
      "day": "20",
      "month": "05",
      "year": "2025"
    },
    "ssn": {
      "part1": "020",
      "part2": "97",
      "part3": "3888"
    },
    "state": "NC",
    "zip": "12546"
  },
  "zip": "12546"
}
```

#### S06 fetch account 00000000010 for update (COACTUPC)

```
GET /api/v1/accounts/00000000010

HTTP 200
{
  "accountVersion": 0,
  "acctId": "00000000010",
  "activeStatus": "Y",
  "addressLine1": "77933 Adah Dale",
  "addressLine2": "Suite 343",
  "cardNum": "3260763612337560",
  "cardNumbers": [
    "3260763612337560"
  ],
  "cashCreditLimit": 4442,
  "city": "Andersonfurt",
  "country": "USA",
  "creditLimit": 5401,
  "currentBalance": 159,
  "currentCycleCredit": 0,
  "currentCycleDebit": 0,
  "custId": "000000010",
  "customerVersion": 0,
  "dateOfBirth": "1980-06-11",
  "eftAccountId": "0093803568",
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COACTVWC",
    "fromTranId": "CAVW",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "expirationDate": "2023-01-27",
  "ficoScore": "476",
  "firstName": "Maybell",
  "governmentId": "00000000000212824755",
  "groupId": "",
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COACTVWC",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CAVW"
  },
  "infoMessage": "Enter or update id of account to display",
  "lastName": "Mann",
  "message": "",
  "middleName": "Creola",
  "openDate": "2015-09-13",
  "phone1": "(614)594-2619",
  "phone2": "(667)057-0235",
  "primaryCardHolder": "Y",
  "reissueDate": "2023-01-27",
  "ssn": "754-75-5746",
  "state": "CT",
  "updateForm": {
    "accountVersion": 0,
    "activeStatus": "Y",
    "addressLine1": "77933 Adah Dale",
    "addressLine2": "Suite 343",
    "cashCreditLimit": "4442.00",
    "city": "Andersonfurt",
    "confirm": false,
    "creditLimit": "5401.00",
    "currentBalance": "159.00",
    "currentCycleCredit": "0.00",
    "currentCycleDebit": "0.00",
    "customerVersion": 0,
    "dateOfBirth": {
      "day": "11",
      "month": "06",
      "year": "1980"
    },
    "eftAccountId": "0093803568",
    "expiryDate": {
      "day": "27",
      "month": "01",
      "year": "2023"
    },
    "ficoScore": "476",
    "firstName": "Maybell",
    "governmentId": "00000000000212824755",
    "groupId": "",
    "lastName": "Mann",
    "middleName": "Creola",
    "openDate": {
      "day": "13",
      "month": "09",
      "year": "2015"
    },
    "phone1": {
      "areaCode": "614",
      "lineNumber": "2619",
      "prefix": "594"
    },
    "phone2": {
      "areaCode": "667",
      "lineNumber": "0235",
      "prefix": "057"
    },
    "primaryCardHolder": "Y",
    "reissueDate": {
      "day": "27",
      "month": "01",
      "year": "2023"
    },
    "ssn": {
      "part1": "754",
      "part2": "75",
      "part3": "5746"
    },
    "state": "CT",
    "zip": "44803"
  },
  "zip": "44803-4279"
}
```

#### S07 account update ENTER: validate (confirm=false)

```
PUT /api/v1/accounts/00000000010
{
  "accountVersion": 0,
  "activeStatus": "Y",
  "addressLine1": "77933 Adah Dale",
  "addressLine2": "Suite 343",
  "cashCreditLimit": "4442.00",
  "city": "Andersonfurt",
  "confirm": false,
  "creditLimit": "6000.00",
  "currentBalance": "159.00",
  "currentCycleCredit": "0.00",
  "currentCycleDebit": "0.00",
  "customerVersion": 0,
  "dateOfBirth": {
    "day": "11",
    "month": "06",
    "year": "1980"
  },
  "eftAccountId": "0093803568",
  "expiryDate": {
    "day": "27",
    "month": "01",
    "year": "2023"
  },
  "ficoScore": "476",
  "firstName": "Maybell",
  "governmentId": "00000000000212824755",
  "groupId": "",
  "lastName": "Mann",
  "middleName": "Creola",
  "openDate": {
    "day": "13",
    "month": "09",
    "year": "2015"
  },
  "phone1": {
    "areaCode": "614",
    "lineNumber": "2619",
    "prefix": "594"
  },
  "phone2": {
    "areaCode": "667",
    "lineNumber": "0235",
    "prefix": "057"
  },
  "primaryCardHolder": "Y",
  "reissueDate": {
    "day": "27",
    "month": "01",
    "year": "2023"
  },
  "ssn": {
    "part1": "754",
    "part2": "75",
    "part3": "5746"
  },
  "state": "CT",
  "zip": "61003"
}

HTTP 200
{
  "account": {
    "accountVersion": 0,
    "acctId": "00000000010",
    "activeStatus": "Y",
    "addressLine1": "77933 Adah Dale",
    "addressLine2": "Suite 343",
    "cardNum": "3260763612337560",
    "cardNumbers": [
      "3260763612337560"
    ],
    "cashCreditLimit": 4442,
    "city": "Andersonfurt",
    "country": "USA",
    "creditLimit": 5401,
    "currentBalance": 159,
    "currentCycleCredit": 0,
    "currentCycleDebit": 0,
    "custId": "000000010",
    "customerVersion": 0,
    "dateOfBirth": "1980-06-11",
    "eftAccountId": "0093803568",
    "exit": {
      "acctId": null,
      "cardNum": null,
      "custId": null,
      "fromProgram": "COACTUPC",
      "fromTranId": "CAUP",
      "pgmContext": "ENTER",
      "toProgram": "COMEN01C",
      "toTranId": "CM00"
    },
    "expirationDate": "2023-01-27",
    "ficoScore": "476",
    "firstName": "Maybell",
    "governmentId": "00000000000212824755",
    "groupId": "",
    "header": {
      "applId": "CARDDEMO",
      "currentDate": "07/06/22",
      "currentTime": "00:00:00",
      "programName": "COACTUPC",
      "sysId": "CDMO",
      "title01": "AWS Mainframe Modernization",
      "title02": "CardDemo",
      "tranId": "CAUP"
    },
    "infoMessage": "Changes validated.Press F5 to save",
    "lastName": "Mann",
    "message": "",
    "middleName": "Creola",
    "openDate": "2015-09-13",
    "phone1": "(614)594-2619",
    "phone2": "(667)057-0235",
    "primaryCardHolder": "Y",
    "reissueDate": "2023-01-27",
    "ssn": "754-75-5746",
    "state": "CT",
    "updateForm": {
      "accountVersion": 0,
      "activeStatus": "Y",
      "addressLine1": "77933 Adah Dale",
      "addressLine2": "Suite 343",
      "cashCreditLimit": "4442.00",
      "city": "Andersonfurt",
      "confirm": false,
      "creditLimit": "5401.00",
      "currentBalance": "159.00",
      "currentCycleCredit": "0.00",
      "currentCycleDebit": "0.00",
      "customerVersion": 0,
      "dateOfBirth": {
        "day": "11",
        "month": "06",
        "year": "1980"
      },
      "eftAccountId": "0093803568",
      "expiryDate": {
        "day": "27",
        "month": "01",
        "year": "2023"
      },
      "ficoScore": "476",
      "firstName": "Maybell",
      "governmentId": "00000000000212824755",
      "groupId": "",
      "lastName": "Mann",
      "middleName": "Creola",
      "openDate": {
        "day": "13",
        "month": "09",
        "year": "2015"
      },
      "phone1": {
        "areaCode": "614",
        "lineNumber": "2619",
        "prefix": "594"
      },
      "phone2": {
        "areaCode": "667",
        "lineNumber": "0235",
        "prefix": "057"
      },
      "primaryCardHolder": "Y",
      "reissueDate": {
        "day": "27",
        "month": "01",
        "year": "2023"
      },
      "ssn": {
        "part1": "754",
        "part2": "75",
        "part3": "5746"
      },
      "state": "CT",
      "zip": "44803"
    },
    "zip": "44803-4279"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COACTUPC",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CAUP"
  },
  "infoMessage": "Changes validated.Press F5 to save",
  "message": "",
  "state": "VALIDATED",
  "updated": false
}
```

#### S08 account update PF5: commit (confirm=true)

```
PUT /api/v1/accounts/00000000010
{
  "accountVersion": 0,
  "activeStatus": "Y",
  "addressLine1": "77933 Adah Dale",
  "addressLine2": "Suite 343",
  "cashCreditLimit": "4442.00",
  "city": "Andersonfurt",
  "confirm": true,
  "creditLimit": "6000.00",
  "currentBalance": "159.00",
  "currentCycleCredit": "0.00",
  "currentCycleDebit": "0.00",
  "customerVersion": 0,
  "dateOfBirth": {
    "day": "11",
    "month": "06",
    "year": "1980"
  },
  "eftAccountId": "0093803568",
  "expiryDate": {
    "day": "27",
    "month": "01",
    "year": "2023"
  },
  "ficoScore": "476",
  "firstName": "Maybell",
  "governmentId": "00000000000212824755",
  "groupId": "",
  "lastName": "Mann",
  "middleName": "Creola",
  "openDate": {
    "day": "13",
    "month": "09",
    "year": "2015"
  },
  "phone1": {
    "areaCode": "614",
    "lineNumber": "2619",
    "prefix": "594"
  },
  "phone2": {
    "areaCode": "667",
    "lineNumber": "0235",
    "prefix": "057"
  },
  "primaryCardHolder": "Y",
  "reissueDate": {
    "day": "27",
    "month": "01",
    "year": "2023"
  },
  "ssn": {
    "part1": "754",
    "part2": "75",
    "part3": "5746"
  },
  "state": "CT",
  "zip": "61003"
}

HTTP 200
{
  "account": {
    "accountVersion": 1,
    "acctId": "00000000010",
    "activeStatus": "Y",
    "addressLine1": "77933 Adah Dale",
    "addressLine2": "Suite 343",
    "cardNum": "3260763612337560",
    "cardNumbers": [
      "3260763612337560"
    ],
    "cashCreditLimit": 4442,
    "city": "Andersonfurt",
    "country": "USA",
    "creditLimit": 6000,
    "currentBalance": 159,
    "currentCycleCredit": 0,
    "currentCycleDebit": 0,
    "custId": "000000010",
    "customerVersion": 1,
    "dateOfBirth": "1980-06-11",
    "eftAccountId": "0093803568",
    "exit": {
      "acctId": null,
      "cardNum": null,
      "custId": null,
      "fromProgram": "COACTUPC",
      "fromTranId": "CAUP",
      "pgmContext": "ENTER",
      "toProgram": "COMEN01C",
      "toTranId": "CM00"
    },
    "expirationDate": "2023-01-27",
    "ficoScore": "476",
    "firstName": "Maybell",
    "governmentId": "00000000000212824755",
    "groupId": "",
    "header": {
      "applId": "CARDDEMO",
      "currentDate": "07/06/22",
      "currentTime": "00:00:00",
      "programName": "COACTUPC",
      "sysId": "CDMO",
      "title01": "AWS Mainframe Modernization",
      "title02": "CardDemo",
      "tranId": "CAUP"
    },
    "infoMessage": "Changes committed to database",
    "lastName": "Mann",
    "message": "",
    "middleName": "Creola",
    "openDate": "2015-09-13",
    "phone1": "(614)594-2619",
    "phone2": "(667)057-0235",
    "primaryCardHolder": "Y",
    "reissueDate": "2023-01-27",
    "ssn": "754-75-5746",
    "state": "CT",
    "updateForm": {
      "accountVersion": 1,
      "activeStatus": "Y",
      "addressLine1": "77933 Adah Dale",
      "addressLine2": "Suite 343",
      "cashCreditLimit": "4442.00",
      "city": "Andersonfurt",
      "confirm": false,
      "creditLimit": "6000.00",
      "currentBalance": "159.00",
      "currentCycleCredit": "0.00",
      "currentCycleDebit": "0.00",
      "customerVersion": 1,
      "dateOfBirth": {
        "day": "11",
        "month": "06",
        "year": "1980"
      },
      "eftAccountId": "0093803568",
      "expiryDate": {
        "day": "27",
        "month": "01",
        "year": "2023"
      },
      "ficoScore": "476",
      "firstName": "Maybell",
      "governmentId": "00000000000212824755",
      "groupId": "",
      "lastName": "Mann",
      "middleName": "Creola",
      "openDate": {
        "day": "13",
        "month": "09",
        "year": "2015"
      },
      "phone1": {
        "areaCode": "614",
        "lineNumber": "2619",
        "prefix": "594"
      },
      "phone2": {
        "areaCode": "667",
        "lineNumber": "0235",
        "prefix": "057"
      },
      "primaryCardHolder": "Y",
      "reissueDate": {
        "day": "27",
        "month": "01",
        "year": "2023"
      },
      "ssn": {
        "part1": "754",
        "part2": "75",
        "part3": "5746"
      },
      "state": "CT",
      "zip": "61003"
    },
    "zip": "61003"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COACTUPC",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CAUP"
  },
  "infoMessage": "Changes committed to database",
  "message": "",
  "state": "COMMITTED",
  "updated": true
}
```

#### S09 list cards of account 00000000010 (COCRDLIC)

```
GET /api/v1/cards?accountId=00000000010

HTTP 200
{
  "accountId": "00000000010",
  "cardNumber": null,
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COCRDLIC",
    "fromTranId": "CCLI",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "hasNextPage": false,
  "hasPreviousPage": false,
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COCRDLIC",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CCLI"
  },
  "infoMessage": "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD",
  "message": "NO MORE RECORDS TO SHOW",
  "nextPage": null,
  "pageSize": 7,
  "previousPage": null,
  "rows": [
    {
      "accountId": "00000000010",
      "activeStatus": "Y",
      "cardNumber": "************7560",
      "cardRef": "<opaque, per run>",
      "row": 1
    }
  ]
}
```

#### S10 view card 3260763612337560 (COCRDSLC)

```
GET /api/v1/cards/3260763612337560?accountId=00000000010

HTTP 200
{
  "accountId": "00000000010",
  "activeStatus": "Y",
  "cardNumber": "3260763612337560",
  "cardRef": "<opaque, per run>",
  "embossedName": "Maybell Mann",
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COCRDSLC",
    "fromTranId": "CCDL",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "expiryMonth": "01",
  "expiryYear": "2023",
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COCRDSLC",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CCDL"
  },
  "infoMessage": "   Displaying requested details",
  "message": "",
  "updateForm": {
    "accountId": "00000000010",
    "activeStatus": "Y",
    "confirm": false,
    "embossedName": "MAYBELL MANN",
    "expiryMonth": "01",
    "expiryYear": "2023",
    "version": 0
  },
  "version": 0
}
```

#### S11 card update ENTER: validate (confirm=false)

```
PUT /api/v1/cards/3260763612337560
{
  "accountId": "00000000010",
  "activeStatus": "Y",
  "confirm": false,
  "embossedName": "Maybell C Mann",
  "expiryMonth": "12",
  "expiryYear": "2027",
  "version": 0
}

HTTP 200
{
  "card": {
    "accountId": "00000000010",
    "activeStatus": "Y",
    "cardNumber": "3260763612337560",
    "cardRef": "<opaque, per run>",
    "embossedName": "Maybell Mann",
    "exit": {
      "acctId": null,
      "cardNum": null,
      "custId": null,
      "fromProgram": "COCRDUPC",
      "fromTranId": "CCUP",
      "pgmContext": "ENTER",
      "toProgram": "COMEN01C",
      "toTranId": "CM00"
    },
    "expiryMonth": "01",
    "expiryYear": "2023",
    "header": {
      "applId": "CARDDEMO",
      "currentDate": "07/06/22",
      "currentTime": "00:00:00",
      "programName": "COCRDUPC",
      "sysId": "CDMO",
      "title01": "AWS Mainframe Modernization",
      "title02": "CardDemo",
      "tranId": "CCUP"
    },
    "infoMessage": "Changes validated.Press F5 to save",
    "message": "",
    "updateForm": {
      "accountId": "00000000010",
      "activeStatus": "Y",
      "confirm": false,
      "embossedName": "MAYBELL MANN",
      "expiryMonth": "01",
      "expiryYear": "2023",
      "version": 0
    },
    "version": 0
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COCRDUPC",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CCUP"
  },
  "infoMessage": "Changes validated.Press F5 to save",
  "message": "",
  "state": "VALIDATED",
  "updated": false
}
```

#### S12 card update PF5: commit (confirm=true)

```
PUT /api/v1/cards/3260763612337560
{
  "accountId": "00000000010",
  "activeStatus": "Y",
  "confirm": true,
  "embossedName": "Maybell C Mann",
  "expiryMonth": "12",
  "expiryYear": "2027",
  "version": 0
}

HTTP 200
{
  "card": {
    "accountId": "00000000010",
    "activeStatus": "Y",
    "cardNumber": "3260763612337560",
    "cardRef": "<opaque, per run>",
    "embossedName": "Maybell C Mann",
    "exit": {
      "acctId": null,
      "cardNum": null,
      "custId": null,
      "fromProgram": "COCRDUPC",
      "fromTranId": "CCUP",
      "pgmContext": "ENTER",
      "toProgram": "COMEN01C",
      "toTranId": "CM00"
    },
    "expiryMonth": "12",
    "expiryYear": "2027",
    "header": {
      "applId": "CARDDEMO",
      "currentDate": "07/06/22",
      "currentTime": "00:00:00",
      "programName": "COCRDUPC",
      "sysId": "CDMO",
      "title01": "AWS Mainframe Modernization",
      "title02": "CardDemo",
      "tranId": "CCUP"
    },
    "infoMessage": "Changes committed to database",
    "message": "",
    "updateForm": {
      "accountId": "00000000010",
      "activeStatus": "Y",
      "confirm": false,
      "embossedName": "MAYBELL C MANN",
      "expiryMonth": "12",
      "expiryYear": "2027",
      "version": 1
    },
    "version": 1
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COCRDUPC",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CCUP"
  },
  "infoMessage": "Changes committed to database",
  "message": "",
  "state": "COMMITTED",
  "updated": true
}
```

#### S13 list transactions (COTRN00C, TRANSACT empty after initial-load)

```
GET /api/v1/transactions

HTTP 200
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COTRN00C",
    "fromTranId": "CT00",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "hasNextPage": false,
  "hasPreviousPage": false,
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COTRN00C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CT00"
  },
  "message": "You are at the top of the page...",
  "nextPage": null,
  "pageNumber": 0,
  "pageSize": 10,
  "previousPage": null,
  "rows": []
}
```

#### S14 add transaction 1 (COTRN02C, confirm Y)

```
POST /api/v1/transactions
{
  "accountId": "00000000001",
  "amount": "+00000050.25",
  "cardNumber": "",
  "categoryCode": "0001",
  "confirm": "Y",
  "description": "GOLDEN SET PURCHASE ONE",
  "merchantCity": "SEATTLE",
  "merchantId": "800000001",
  "merchantName": "GOLDEN GROCERY",
  "merchantZip": "98101",
  "origDate": "2022-06-14",
  "procDate": "2022-06-15",
  "source": "POS TERM",
  "typeCode": "01"
}

HTTP 201
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COTRN02C",
    "fromTranId": "CT02",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "form": {
    "accountId": "",
    "amount": "",
    "cardNumber": "",
    "categoryCode": "",
    "confirm": "",
    "copyLast": false,
    "description": "",
    "merchantCity": "",
    "merchantId": "",
    "merchantName": "",
    "merchantZip": "",
    "origDate": "",
    "procDate": "",
    "source": "",
    "typeCode": ""
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COTRN02C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CT02"
  },
  "message": "Transaction added successfully.  Your Tran ID is 0000000000000001.",
  "state": "ADDED",
  "transaction": {
    "amount": "+00000050.25",
    "cardNumber": "9680294154603697",
    "categoryCode": "0001",
    "description": "GOLDEN SET PURCHASE ONE",
    "merchantCity": "SEATTLE",
    "merchantId": "800000001",
    "merchantName": "GOLDEN GROCERY",
    "merchantZip": "98101",
    "origTimestamp": "2022-06-14",
    "procTimestamp": "2022-06-15",
    "source": "POS TERM",
    "tranId": "0000000000000001",
    "typeCode": "01"
  }
}
```

#### S15 add transaction 2 (COTRN02C, confirm Y)

```
POST /api/v1/transactions
{
  "accountId": "",
  "amount": "-00000012.34",
  "cardNumber": "3999169246375885",
  "categoryCode": "0001",
  "confirm": "Y",
  "description": "GOLDEN SET CREDIT TWO",
  "merchantCity": "PORTLAND",
  "merchantId": "800000002",
  "merchantName": "GOLDEN HARDWARE",
  "merchantZip": "97201",
  "origDate": "2022-06-30",
  "procDate": "2022-07-01",
  "source": "OPERATOR",
  "typeCode": "03"
}

HTTP 201
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COTRN02C",
    "fromTranId": "CT02",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "form": {
    "accountId": "",
    "amount": "",
    "cardNumber": "",
    "categoryCode": "",
    "confirm": "",
    "copyLast": false,
    "description": "",
    "merchantCity": "",
    "merchantId": "",
    "merchantName": "",
    "merchantZip": "",
    "origDate": "",
    "procDate": "",
    "source": "",
    "typeCode": ""
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COTRN02C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CT02"
  },
  "message": "Transaction added successfully.  Your Tran ID is 0000000000000002.",
  "state": "ADDED",
  "transaction": {
    "amount": "-00000012.34",
    "cardNumber": "3999169246375885",
    "categoryCode": "0001",
    "description": "GOLDEN SET CREDIT TWO",
    "merchantCity": "PORTLAND",
    "merchantId": "800000002",
    "merchantName": "GOLDEN HARDWARE",
    "merchantZip": "97201",
    "origTimestamp": "2022-06-30",
    "procTimestamp": "2022-07-01",
    "source": "OPERATOR",
    "tranId": "0000000000000002",
    "typeCode": "03"
  }
}
```

#### S16 list transactions (COTRN00C)

```
GET /api/v1/transactions

HTTP 200
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COTRN00C",
    "fromTranId": "CT00",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "hasNextPage": false,
  "hasPreviousPage": false,
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COTRN00C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CT00"
  },
  "message": "You have reached the bottom of the page...",
  "nextPage": null,
  "pageNumber": 1,
  "pageSize": 10,
  "previousPage": null,
  "rows": [
    {
      "amount": "+00000050.25",
      "date": "06/14/22",
      "description": "GOLDEN SET PURCHASE ONE",
      "row": 1,
      "tranId": "0000000000000001"
    },
    {
      "amount": "-00000012.34",
      "date": "06/30/22",
      "description": "GOLDEN SET CREDIT TWO",
      "row": 2,
      "tranId": "0000000000000002"
    }
  ]
}
```

#### S17 view transaction 0000000000000001 (COTRN01C)

```
GET /api/v1/transactions/0000000000000001

HTTP 200
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COTRN01C",
    "fromTranId": "CT01",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COTRN01C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CT01"
  },
  "list": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COTRN01C",
    "fromTranId": "CT01",
    "pgmContext": "ENTER",
    "toProgram": "COTRN00C",
    "toTranId": "CT00"
  },
  "message": "",
  "transaction": {
    "amount": "+00000050.25",
    "cardNumber": "9680294154603697",
    "categoryCode": "0001",
    "description": "GOLDEN SET PURCHASE ONE",
    "merchantCity": "SEATTLE",
    "merchantId": "800000001",
    "merchantName": "GOLDEN GROCERY",
    "merchantZip": "98101",
    "origTimestamp": "2022-06-14",
    "procTimestamp": "2022-06-15",
    "source": "POS TERM",
    "tranId": "0000000000000001",
    "typeCode": "01"
  }
}
```

#### S18 bill payment ENTER: show balance of account 00000000002 (COBIL00C)

```
POST /api/v1/accounts/00000000002/bill-payment
{
  "confirm": ""
}

HTTP 200
{
  "accountId": "00000000002",
  "currentBalance": "158.00",
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COBIL00C",
    "fromTranId": "CB00",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COBIL00C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CB00"
  },
  "message": "Confirm to make a bill payment...",
  "state": "SHOW",
  "transaction": null,
  "version": 0
}
```

#### S19 bill payment confirm Y

```
POST /api/v1/accounts/00000000002/bill-payment
{
  "confirm": "Y",
  "version": 0
}

HTTP 200
{
  "accountId": "00000000002",
  "currentBalance": "0.00",
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COBIL00C",
    "fromTranId": "CB00",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COBIL00C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CB00"
  },
  "message": "Payment successful.  Your Transaction ID is 0000000000000003.",
  "state": "PAID",
  "transaction": {
    "amount": "+00000158.00",
    "cardNumber": "0923877193247330",
    "categoryCode": "0002",
    "description": "BILL PAYMENT - ONLINE",
    "merchantCity": "N/A",
    "merchantId": "999999999",
    "merchantName": "BILL PAYMENT",
    "merchantZip": "N/A",
    "origTimestamp": "2022-07-06 00:00:00.000000",
    "procTimestamp": "2022-07-06 00:00:00.000000",
    "source": "POS TERM",
    "tranId": "0000000000000003",
    "typeCode": "02"
  },
  "version": 1
}
```

#### S20 view account 00000000002 after the payment (balance 0)

```
GET /api/v1/accounts/00000000002

HTTP 200
{
  "accountVersion": 1,
  "acctId": "00000000002",
  "activeStatus": "Y",
  "addressLine1": "4917 Myrna Flats",
  "addressLine2": "Apt. 453",
  "cardNum": "0923877193247330",
  "cardNumbers": [
    "0923877193247330"
  ],
  "cashCreditLimit": 5448,
  "city": "West Bernita",
  "country": "USA",
  "creditLimit": 6130,
  "currentBalance": 0,
  "currentCycleCredit": 0,
  "currentCycleDebit": 0,
  "custId": "000000002",
  "customerVersion": 0,
  "dateOfBirth": "1961-10-08",
  "eftAccountId": "0069194009",
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COACTVWC",
    "fromTranId": "CAVW",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "expirationDate": "2024-08-11",
  "ficoScore": "268",
  "firstName": "Enrico",
  "governmentId": "00000000000506210371",
  "groupId": "",
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COACTVWC",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CAVW"
  },
  "infoMessage": "Enter or update id of account to display",
  "lastName": "Rosenbaum",
  "message": "",
  "middleName": "April",
  "openDate": "2013-06-19",
  "phone1": "(429)706-9510",
  "phone2": "(744)950-5272",
  "primaryCardHolder": "Y",
  "reissueDate": "2024-08-11",
  "ssn": "587-51-8382",
  "state": "IN",
  "updateForm": {
    "accountVersion": 1,
    "activeStatus": "Y",
    "addressLine1": "4917 Myrna Flats",
    "addressLine2": "Apt. 453",
    "cashCreditLimit": "5448.00",
    "city": "West Bernita",
    "confirm": false,
    "creditLimit": "6130.00",
    "currentBalance": "0.00",
    "currentCycleCredit": "0.00",
    "currentCycleDebit": "0.00",
    "customerVersion": 0,
    "dateOfBirth": {
      "day": "08",
      "month": "10",
      "year": "1961"
    },
    "eftAccountId": "0069194009",
    "expiryDate": {
      "day": "11",
      "month": "08",
      "year": "2024"
    },
    "ficoScore": "268",
    "firstName": "Enrico",
    "governmentId": "00000000000506210371",
    "groupId": "",
    "lastName": "Rosenbaum",
    "middleName": "April",
    "openDate": {
      "day": "19",
      "month": "06",
      "year": "2013"
    },
    "phone1": {
      "areaCode": "429",
      "lineNumber": "9510",
      "prefix": "706"
    },
    "phone2": {
      "areaCode": "744",
      "lineNumber": "5272",
      "prefix": "950"
    },
    "primaryCardHolder": "Y",
    "reissueDate": {
      "day": "11",
      "month": "08",
      "year": "2024"
    },
    "ssn": {
      "part1": "587",
      "part2": "51",
      "part3": "8382"
    },
    "state": "IN",
    "zip": "22770"
  },
  "zip": "22770"
}
```

#### S21 list transactions: the payment is type 02 / category 2

```
GET /api/v1/transactions

HTTP 200
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COTRN00C",
    "fromTranId": "CT00",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "hasNextPage": false,
  "hasPreviousPage": false,
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COTRN00C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CT00"
  },
  "message": "You have reached the bottom of the page...",
  "nextPage": null,
  "pageNumber": 1,
  "pageSize": 10,
  "previousPage": null,
  "rows": [
    {
      "amount": "+00000050.25",
      "date": "06/14/22",
      "description": "GOLDEN SET PURCHASE ONE",
      "row": 1,
      "tranId": "0000000000000001"
    },
    {
      "amount": "-00000012.34",
      "date": "06/30/22",
      "description": "GOLDEN SET CREDIT TWO",
      "row": 2,
      "tranId": "0000000000000002"
    },
    {
      "amount": "+00000158.00",
      "date": "07/06/22",
      "description": "BILL PAYMENT - ONLINE",
      "row": 3,
      "tranId": "0000000000000003"
    }
  ]
}
```

#### S22 add user GOLDEN01 (COUSR01C)

```
POST /api/v1/users
{
  "firstName": "Grace",
  "lastName": "Golden",
  "password": "GOLDPASS",
  "userId": "GOLDEN01",
  "userType": "U"
}

HTTP 201
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COUSR01C",
    "fromTranId": "CU01",
    "pgmContext": "ENTER",
    "toProgram": "COADM01C",
    "toTranId": "CA00"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COUSR01C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CU01"
  },
  "message": "User GOLDEN01 has been added ...",
  "state": "ADDED",
  "user": {
    "firstName": "Grace",
    "lastName": "Golden",
    "password": null,
    "userId": "GOLDEN01",
    "userType": "U",
    "version": 0
  }
}
```

#### S23 fetch user GOLDEN01 (COUSR02C ENTER)

```
GET /api/v1/users/GOLDEN01?fromProgram=COUSR00C

HTTP 200
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COUSR02C",
    "fromTranId": "CU02",
    "pgmContext": "ENTER",
    "toProgram": "COUSR00C",
    "toTranId": "CU00"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COUSR02C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CU02"
  },
  "message": "Press PF5 key to save your updates ...",
  "state": "SHOW",
  "user": {
    "firstName": "Grace",
    "lastName": "Golden",
    "password": "GOLDPASS",
    "userId": "GOLDEN01",
    "userType": "U",
    "version": 0
  }
}
```

#### S24 update user GOLDEN01 (COUSR02C PF5)

```
PUT /api/v1/users/GOLDEN01
{
  "firstName": "Grace",
  "lastName": "Goldensen",
  "password": "GOLDPASS",
  "userType": "A",
  "version": 0
}

HTTP 200
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COUSR02C",
    "fromTranId": "CU02",
    "pgmContext": "ENTER",
    "toProgram": "COADM01C",
    "toTranId": "CA00"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COUSR02C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CU02"
  },
  "message": "User GOLDEN01 has been updated ...",
  "state": "UPDATED",
  "user": {
    "firstName": "Grace",
    "lastName": "Goldensen",
    "password": "GOLDPASS",
    "userId": "GOLDEN01",
    "userType": "A",
    "version": 1
  }
}
```

#### S25 delete user USER0005 ENTER: show and ask to confirm (COUSR03C)

```
DELETE /api/v1/users/USER0005?fromProgram=COUSR00C

HTTP 200
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COUSR03C",
    "fromTranId": "CU03",
    "pgmContext": "ENTER",
    "toProgram": "COUSR00C",
    "toTranId": "CU00"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COUSR03C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CU03"
  },
  "message": "Press PF5 key to delete this user ...",
  "state": "VALIDATED",
  "user": {
    "firstName": "LEE",
    "lastName": "TING",
    "password": null,
    "userId": "USER0005",
    "userType": "U",
    "version": 0
  }
}
```

#### S26 delete user USER0005 confirm Y (COUSR03C PF5)

```
DELETE /api/v1/users/USER0005?confirm=Y&version=0

HTTP 200
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COUSR03C",
    "fromTranId": "CU03",
    "pgmContext": "ENTER",
    "toProgram": "COADM01C",
    "toTranId": "CA00"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COUSR03C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CU03"
  },
  "message": "User USER0005 has been deleted ...",
  "state": "DELETED",
  "user": {
    "firstName": "LEE",
    "lastName": "TING",
    "password": null,
    "userId": "USER0005",
    "userType": "U",
    "version": 0
  }
}
```

#### S27 list users (COUSR00C)

```
GET /api/v1/users

HTTP 200
{
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "COUSR00C",
    "fromTranId": "CU00",
    "pgmContext": "ENTER",
    "toProgram": "COADM01C",
    "toTranId": "CA00"
  },
  "hasNextPage": false,
  "hasPreviousPage": false,
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "COUSR00C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CU00"
  },
  "message": "You have reached the bottom of the page...",
  "nextPage": null,
  "pageNumber": 1,
  "pageSize": 10,
  "previousPage": null,
  "rows": [
    {
      "firstName": "MARGARET",
      "lastName": "GOLD",
      "row": 1,
      "userId": "ADMIN001",
      "userType": "A"
    },
    {
      "firstName": "RUSSELL",
      "lastName": "RUSSELL",
      "row": 2,
      "userId": "ADMIN002",
      "userType": "A"
    },
    {
      "firstName": "RAYMOND",
      "lastName": "WHITMORE",
      "row": 3,
      "userId": "ADMIN003",
      "userType": "A"
    },
    {
      "firstName": "EMMANUEL",
      "lastName": "CASGRAIN",
      "row": 4,
      "userId": "ADMIN004",
      "userType": "A"
    },
    {
      "firstName": "GRANVILLE",
      "lastName": "LACHAPELLE",
      "row": 5,
      "userId": "ADMIN005",
      "userType": "A"
    },
    {
      "firstName": "Grace",
      "lastName": "Goldensen",
      "row": 6,
      "userId": "GOLDEN01",
      "userType": "A"
    },
    {
      "firstName": "LAWRENCE",
      "lastName": "THOMAS",
      "row": 7,
      "userId": "USER0001",
      "userType": "U"
    },
    {
      "firstName": "AJITH",
      "lastName": "KUMAR",
      "row": 8,
      "userId": "USER0002",
      "userType": "U"
    },
    {
      "firstName": "LAURITZ",
      "lastName": "ALME",
      "row": 9,
      "userId": "USER0003",
      "userType": "U"
    },
    {
      "firstName": "AVERARDO",
      "lastName": "MAZZI",
      "row": 10,
      "userId": "USER0004",
      "userType": "U"
    }
  ]
}
```

#### S28 submit Custom report for the DATEPARM window (CORPT00C, confirm Y)

```
POST /api/v1/reports/transactions
{
  "confirm": "Y",
  "endDate": {
    "day": "06",
    "month": "07",
    "year": "2022"
  },
  "reportType": "Custom",
  "startDate": {
    "day": "01",
    "month": "01",
    "year": "2022"
  }
}

HTTP 202
{
  "endDate": {
    "day": "06",
    "month": "07",
    "year": "2022"
  },
  "executionId": 1,
  "exit": {
    "acctId": null,
    "cardNum": null,
    "custId": null,
    "fromProgram": "CORPT00C",
    "fromTranId": "CR00",
    "pgmContext": "ENTER",
    "toProgram": "COMEN01C",
    "toTranId": "CM00"
  },
  "header": {
    "applId": "CARDDEMO",
    "currentDate": "07/06/22",
    "currentTime": "00:00:00",
    "programName": "CORPT00C",
    "sysId": "CDMO",
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CR00"
  },
  "message": "Custom report submitted for printing ...",
  "parmEndDate": "2022-07-06",
  "parmStartDate": "2022-01-01",
  "reportName": "Custom",
  "startDate": {
    "day": "01",
    "month": "01",
    "year": "2022"
  },
  "state": "SUBMITTED",
  "status": "COMPLETED",
  "statusUrl": "/api/v1/reports/transactions/1"
}
```

#### S29 report execution 1 status (COMPLETED)

```
GET /api/v1/reports/transactions/1

HTTP 200
{
  "endDate": "2022-07-06",
  "endedAt": "2022-07-06T00:00:00Z",
  "executionId": 1,
  "jobStream": "tranrept",
  "jobs": [
    {
      "endTime": "<wall clock>",
      "jobExecutionId": 2,
      "jobName": "reproc",
      "readCount": 3,
      "returnCode": "RC=0000",
      "startTime": "<wall clock>",
      "status": "COMPLETED",
      "writeCount": 3
    },
    {
      "endTime": "<wall clock>",
      "jobExecutionId": 3,
      "jobName": "tranrept-sort",
      "readCount": 3,
      "returnCode": "RC=0000",
      "startTime": "<wall clock>",
      "status": "COMPLETED",
      "writeCount": 3
    },
    {
      "endTime": "<wall clock>",
      "jobExecutionId": 4,
      "jobName": "cbtrn03c",
      "readCount": 3,
      "returnCode": "RC=0000",
      "startTime": "<wall clock>",
      "status": "COMPLETED",
      "writeCount": 14
    }
  ],
  "message": "TRANREPT 14 lines",
  "report": {
    "downloadUrl": "/api/v1/reports/transactions/1/report",
    "encoding": "ASCII",
    "fileName": "TRANREPT.2022-07-06.4",
    "lines": [
      "DALYREPT                              Daily Transaction Report                 Date Range: 2022-01-01 to 2022-07-06",
      "",
      "Transaction ID   Account ID  Transaction Type   Tran Category                      Tran Source            Amount",
      "-------------------------------------------------------------------------------------------------------------------------------------",
      "0000000000000003 00000000002 02-Payment         0002-Electronic payment            POS TERM               158.00",
      "Account Total....................................................................................+        158.00",
      "-------------------------------------------------------------------------------------------------------------------------------------",
      "0000000000000002 00000000003 03-Credit          0001-Credit to Account             OPERATOR      -         12.34",
      "Account Total....................................................................................-         12.34",
      "-------------------------------------------------------------------------------------------------------------------------------------",
      "0000000000000001 00000000001 01-Purchase        0001-Regular Sales Draft           POS TERM                50.25",
      "Page Total ......................................................................................+        246.16",
      "-------------------------------------------------------------------------------------------------------------------------------------",
      "Grand Total......................................................................................+        246.16"
    ],
    "outputFileId": 3,
    "recordCount": 14,
    "sha256": "65218b40b16ac761b192ae317045485a57f0dee84f1a373146f639ecce4c1d10"
  },
  "reportName": "Custom",
  "requestedBy": "ADMIN001",
  "returnCode": 0,
  "runDate": "2022-07-06",
  "startDate": "2022-01-01",
  "startedAt": "2022-07-06T00:00:00Z",
  "status": "COMPLETED",
  "submittedAt": "<wall clock>"
}
```

#### S30 download report 1

```
GET /api/v1/reports/transactions/1/report

1862 bytes, sha256 65218b40b16ac761b192ae317045485a57f0dee84f1a373146f639ecce4c1d10 (saved as online.TRANREPT)
DALYREPT                              Daily Transaction Report                 Date Range: 2022-01-01 to 2022-07-06                                                                                                                                                       Transaction ID   Account ID  Transaction Type   Tran Category                      Tran Source            Amount                     -------------------------------------------------------------------------------------------------------------------------------------0000000000000003 00000000002 02-Payment         0002-Electronic payment            POS TERM               158.00                     Account Total....................................................................................+        158.00                     -------------------------------------------------------------------------------------------------------------------------------------0000000000000002 00000000003 03-Credit          0001-Credit to Account             OPERATOR      -         12.34                     Account Total....................................................................................-         12.34                     -------------------------------------------------------------------------------------------------------------------------------------0000000000000001 00000000001 01-Purchase        0001-Regular Sales Draft           POS TERM                50.25                     Page Total ......................................................................................+        246.16                     -------------------------------------------------------------------------------------------------------------------------------------Grand Total......................................................................................+        246.16                     ```

