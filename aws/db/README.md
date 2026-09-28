# CardDemo database (Aurora PostgreSQL) and data migration

Target schema `carddemo` on Aurora PostgreSQL / PostgreSQL 15+, as specified by
`aws/contracts/data-model.md`, plus the ETL that turns the mainframe sample data (`app/data/EBCDIC/`,
IMS unload) into seed CSVs and loads them.

| Path | Content |
|---|---|
| `schema.sql` | Core VSAM tables + technical tables (`processed_message`, `batch_job_run`). Idempotent. |
| `db2/authfrds.sql` | DB2 `CARDDEMO.AUTHFRDS` + index `XAUTHFRD` → `authfrds` |
| `db2/transaction_type.sql` | DB2 `TRANSACTION_TYPE` / `TRANSACTION_TYPE_CATEGORY` reconciliation + DB2-named compatibility views |
| `ims/pending_auth.sql` | IMS `DBPAUTP0` (`PAUTSUM0` / `PAUTDTL1`) → `pending_auth_summary` / `pending_auth_detail` (**replatform candidate**) |
| `../etl/` | Python 3.11 copybook-driven EBCDIC decoder, CSV converter, `COPY` loader, tests |
| `../etl/output/` | Committed seed CSVs (one per table; `export/` holds the `EXPORT.DATA.PS` record types) |

All DDL files are idempotent (`CREATE ... IF NOT EXISTS`, `CREATE OR REPLACE VIEW`). Apply in the order
`schema.sql`, `db2/*.sql`, `ims/*.sql` (the loader's `--apply-schema` does exactly that).

## Running

```bash
cd aws/etl
python3.11 -m venv .venv && . .venv/bin/activate
pip install -r requirements.txt

python -m etl layouts                                   # list layouts (one per source file)
python -m etl convert --layout account --input ../../app/data/EBCDIC/AWS.M2.CARDDEMO.ACCTDATA.PS --out /tmp/account.csv
python -m etl convert --layout export --out /tmp/export/  # multi-record layout: --out is a directory
python -m etl convert-all                               # regenerate every CSV in output/
python -m etl crosscheck                                # EBCDIC vs app/data/ASCII field-by-field; exits 1 only on differences not in KNOWN_DIFFERENCES

docker run -d --name carddemo-pg -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=carddemo -p 55432:5432 postgres:15
python -m etl load --dsn postgresql://postgres:postgres@localhost:55432/carddemo --apply-schema
# --dsn may be omitted when DB_HOST/DB_PORT/DB_NAME/DB_USER/DB_PASSWORD are set (conventions.md section 3).
# Truncate+reload refuses a CSV set that omits tables referencing the ones being reloaded (use --no-truncate to append).

pytest -q tests                                         # unit + file tests
CARDDEMO_TEST_DSN=postgresql://postgres:postgres@localhost:55432/carddemo pytest -q tests  # + DB integration
```

`--input` defaults to the layout's sample file, so any EBCDIC file with the same layout (e.g. a fresh
production unload) can be converted with the same command. `load` truncates the loaded tables and reloads
them in one transaction in FK order (`--no-truncate` appends); it uses `COPY ... FROM STDIN (FORMAT csv)`,
where an unquoted empty field is `NULL` and `""` is an empty string. `load` only touches tables for which a
CSV exists; `export_*` CSVs have no target table (they are the `CBEXPORT`/`CBIMPORT` interchange format and
are kept for the validation session).

## Layouts (one per file)

| Layout | Source file(s) | Copybook | LRECL | Target | Rows |
|---|---|---|---|---|---|
| `usrsec` | `USRSEC.PS` | `CSUSR01Y` | 80 | `user_security` | 10 |
| `account` | `ACCTDATA.PS` (`ACCDATA.PS` is byte-identical → skipped) | `CVACT01Y` | 300 | `account` | 50 |
| `card` | `CARDDATA.PS` | `CVACT02Y` | 150 | `card` | 50 |
| `cardxref` | `CARDXREF.PS` | `CVACT03Y` | 50 | `card_xref` | 50 |
| `customer` | `CUSTDATA.PS` | `CVCUS01Y` | 500 | `customer` | 50 |
| `dalytran` | `DALYTRAN.PS` | `CVTRA06Y` | 350 | `daily_transaction` (`run_id='SEED'`, `load_seq` = record #) | 300 |
| `tranfile_init` | `DALYTRAN.PS.INIT` | `CVTRA05Y` | 350 | `transaction` | 0 (low-values priming record) |
| `discgrp` | `DISCGRP.PS` | `CVTRA02Y` | 50 | `disclosure_group` | 51 |
| `tcatbal` | `TCATBALF.PS` | `CVTRA01Y` | 50 | `tran_cat_balance` | 50 |
| `trancatg` | `TRANCATG.PS` | `CVTRA04Y` | 60 | `transaction_category` | 18 |
| `trantype` | `TRANTYPE.PS` | `CVTRA03Y` | 60 | `transaction_type` | 7 |
| `export` | `EXPORT.DATA.PS` | `CVEXPORT` | 500 | `export/export_{customer,account,transaction,card_xref,card}.csv` | 50/50/300/50/50 |
| `dbpautp0` | `IMSDATA.DBPAUTP0.dat` (IMS HD unload, RDW-framed) | `CIPAUSMY` / `CIPAUDTY` | 100 / 200 | `pending_auth_summary` / `pending_auth_detail` | 21 / 202 |

Every file in `app/data/EBCDIC/` is covered (asserted by `tests/test_files.py`).

## COBOL → PostgreSQL type mapping

| COBOL picture / usage | Storage | PostgreSQL | Example |
|---|---|---|---|
| `X(n)` free text | EBCDIC cp037 | `VARCHAR(n)`, trailing spaces/low-values trimmed, all-blank → `NULL` (NOT NULL columns keep `''`) | `ACCT-ADDR-ZIP X(10)` → `addr_zip VARCHAR(10)` |
| `X(n)` fixed code / key | cp037 | `CHAR(n)` | `CARD-NUM X(16)` → `card_num CHAR(16)`; `TRAN-TYPE-CD X(02)` |
| `X(01)` flag | cp037 | `CHAR(1)` + `CHECK` | `ACCT-ACTIVE-STATUS` → `CHECK (active_status IN ('Y','N'))` |
| `X(10)` `YYYY-MM-DD` | cp037 | `DATE` (spaces/zeros → `NULL`) | `ACCT-OPEN-DATE` |
| `X(26)` `YYYY-MM-DD-HH.MM.SS.ffffff` or ISO | cp037 | `TIMESTAMP(6)` | `TRAN-ORIG-TS` |
| `9(n)` key / identifier, n ≤ 9 | zoned | `INTEGER` | `CUST-ID 9(09)` |
| `9(11)` | zoned | `NUMERIC(11,0)` (exceeds `INTEGER`) | `ACCT-ID 9(11)` |
| `9(09)` with leading zeros significant | zoned | `CHAR(9)` + `CHECK (~ '^[0-9]{9}$')` | `CUST-SSN` |
| `9(03)` / `9(04)` small codes | zoned | `SMALLINT` | `CARD-CVV-CD`, `TRAN-CAT-CD 9(04)` → `cat_cd` |
| `S9(p-s)V9(s)` | zoned, sign in last zone nibble (C/F = +, D = −) | `NUMERIC(p,s)` | `ACCT-CURR-BAL S9(10)V99` → `NUMERIC(12,2)`; `TRAN-AMT S9(09)V99` → `NUMERIC(11,2)`; `DIS-INT-RATE S9(04)V99` → `NUMERIC(6,2)` |
| `S9(p-s)V9(s) COMP-3` | packed, ⌈(p+1)/2⌉ bytes, sign nibble C/F/A/E = +, D/B = − | `NUMERIC(p,s)` | `PA-TRANSACTION-AMT S9(10)V99 COMP-3` → `NUMERIC(12,2)`; `PA-CREDIT-LIMIT S9(09)V99 COMP-3` → `NUMERIC(11,2)` |
| `S9(11) COMP-3` key | packed | `NUMERIC(11,0)` | `PA-ACCT-ID` |
| `S9(05)` / `S9(09) COMP-3` | packed | `INTEGER` / `BIGINT` | `PA-AUTH-DATE-9C`, `PA-AUTH-TIME-9C` |
| `S9(04) COMP` | big-endian binary, 2 bytes | `SMALLINT` | `PA-APPROVED-AUTH-CNT` |
| `9(09) COMP` | big-endian binary, 4 bytes | `INTEGER` | `EXPORT-SEQUENCE-NUM` |
| `X(n) OCCURS k` | k consecutive elements | `CHAR(n)[]` (blank element → `NULL`) or separate columns | see below |
| `FILLER` | — | dropped | |
| `SEC-USR-PWD X(08)` | plain text | `VARCHAR(100)` BCrypt hash (cost 10) of the upper-cased value | the legacy sign-on upper-cases input |

ASCII copies (`app/data/ASCII/`) use the same widths with the sign overpunched in the last character
(`{`/`A`–`I` = +0..+9, `}`/`J`–`R` = −0..−9); the decoder maps each ASCII line back to cp037 and runs the
identical copybook decoder, so both sources go through one code path.

## REDEFINES and OCCURS decisions

* **`CVEXPORT` (`EXPORT.DATA.PS`)**: 500-byte record with a 40-byte header (`EXPORT-REC-TYPE X(1)`,
  `EXPORT-TIMESTAMP X(26)`, `EXPORT-SEQUENCE-NUM 9(9) COMP`, `EXPORT-BRANCH-ID`, `EXPORT-REGION-CODE`) and
  `EXPORT-RECORD-DATA X(460)` redefined five times. The branch is selected by `EXPORT-REC-TYPE`:
  `C` → `EXPORT-CUSTOMER-DATA`, `A` → `EXPORT-ACCOUNT-DATA`, `T` → `EXPORT-TRANSACTION-DATA`,
  `X` → `EXPORT-CARD-XREF-DATA`, `D` → `EXPORT-CARD-DATA` (the layout asserts every mapped field belongs to
  the chosen branch; an unknown type fails the conversion). Each branch becomes its own CSV with the header
  columns repeated. The branches mix `COMP` and `COMP-3` (e.g. `EXP-ACCT-CURR-BAL S9(10)V99 COMP-3`,
  `EXP-CARD-CVV-CD 9(03) COMP`), which the decoder handles per field. The export records are **not loaded**
  into tables: they duplicate the core files (same keys and values, names upper-cased) and are the
  `CBEXPORT`/`CBIMPORT` interchange format. Sequence numbers are 1–450 and 460–509 in the sample (a gap in the
  source file, asserted by the tests).
* **`CVEXPORT` `EXP-CUST-ADDR-LINE X(50) OCCURS 3`** / **`EXP-CUST-PHONE-NUM X(15) OCCURS 2`** → separate
  columns `addr_line_1..3`, `phone_num_1..2`, matching the non-repeating fields of `CVCUS01Y` / `customer`.
* **`CIPAUSMY` `PA-ACCOUNT-STATUS X(02) OCCURS 5`** → one `CHAR(2)[]` column `account_status`
  (contract §3.2); all five elements are kept positionally, blank elements are `NULL`.
* **IMS segments**: the unload is one physical file with two segment types; the IMS segment name in the
  record prefix plays the role of the discriminator (`PAUTSUM0` → summary, `PAUTDTL1` → detail). Each detail
  belongs to the most recent root (hierarchical sequence), which gives `pending_auth_detail.acct_id`. The
  final all-spaces root segment is an unload terminator and is skipped.
* Other REDEFINES in the core copybooks are date/number views of the same bytes (no discriminator);
  the base field is decoded.

## DB2 vs VSAM reference data (which is authoritative)

`TRANSACTION_TYPE` / `TRANSACTION_TYPE_CATEGORY` (DB2 sub-app) and `TRANTYPE` / `TRANCATG` (VSAM) describe
the same entities; on the mainframe DB2 is the master (`COTRTUPC` maintains it, `TRANEXTR.jcl` re-extracts
the VSAM files from it). On AWS they are **one** set of tables: `carddemo.transaction_type` and
`carddemo.transaction_category`, created by `schema.sql`, are authoritative. `db2/transaction_type.sql` only
adds DB2-named views (`db2_transaction_type`, `db2_transaction_type_category` with `TRC_TYPE_CATEGORY`
re-padded to `CHAR(4)`). The DB2 FK `TRANSACTION_TYPE_CATEGORY → TRANSACTION_TYPE` is kept
(`ON DELETE RESTRICT`); `XTRAN_TYPE` / `X_TRAN_TYPE_CATG` are the primary keys.

Seed data comes from the VSAM (EBCDIC) files. The DB2 seed statements (`DB2LTTYP.ctl`, `DB2LTCAT.ctl`) have the
same 7 / 18 keys (asserted by `tests/test_db2_reconcile.py`); descriptions differ only in case except
type `06` (`REVERAL` typo in DB2) and category `06/0002` (`NON FRAUD REVERSAL` vs `Non-fraud reversal`).

`AUTHFRDS` (fraud reports written by `COPAUS2C`) maps 1:1 to `authfrds`; `MERCHANT_CATAGORY_CODE` is renamed
`merchant_category_code`; `XAUTHFRD (CARD_NUM ASC, AUTH_TS DESC)` → `ix_authfrds_card_ts`. There is no sample
data for it.

## IMS `DBPAUTP0`

Table design is in `ims/pending_auth.sql` (contract §3.2–3.3): root → `pending_auth_summary`
(PK `acct_id`), child → `pending_auth_detail` (PK `acct_id`, `auth_date_9c`, `auth_time_9c`; FK to the
summary `ON DELETE CASCADE`, mirroring a DL/I `DLET` of the root). The 9's-complement keys are stored as-is so
the IMS read order (newest first) is `ORDER BY auth_date_9c, auth_time_9c`. The unload is decoded and loaded
(21 summaries, 202 details; every detail card/account matches `card_xref`). **The programs using this database
(`COPAUA0C`, `COPAUS0C`/`1C`/`2C`, `CBPAUP0C`, `PAUDBUNL`, `PAUDBLOD`, `DBUNLDGS`) remain replatform
candidates** (`aws/migration-inventory.md` §9); the tables only make the data available if they are refactored.

## Sample-data findings

* `DALYTRAN.PS.INIT` (used by `TRANFILE.jcl` to prime `TRANSACT`) is 350 bytes of low-values — a dummy record
  that lets CICS open a non-empty KSDS. It is not a transaction and is not loaded; `transaction` starts empty.
* EBCDIC vs ASCII: all nine files that exist in both forms decode to identical rows except two records whose
  **content** differs between the two sample sets (verified at byte level, not decoder issues). The EBCDIC value
  is loaded (contract §5):
  * `ACCTDATA` account 49: `ACCT-ADDR-ZIP` = `ZEROAPR` (EBCDIC) vs `A000000000` (ASCII);
  * `DISCGRP` record 34 (`DEFAULT`/`07`/`0001`): `DIS-INT-RATE` = `15.00` (EBCDIC) vs `0.00` (ASCII).
* `account.group_id` is blank for all 50 sample accounts (→ `NULL`), so interest calculation uses the
  `DEFAULT` disclosure group; `addr_zip` holds the group-like values `A000000000`/`ZEROAPR`.
* `customer.fico_credit_score` sample values go below 300; the schema checks only the `9(03)` domain because
  `COACTUPC`'s 300–850 rule is an update-time validation.
