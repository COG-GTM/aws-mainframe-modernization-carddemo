# 03 — Data model: VSAM / sequential layouts → PostgreSQL (CardDemo)

Step s3.1. Implemented by Flyway `modernization/carddemo-app/src/main/resources/db/migration/V2__core_schema.sql`
(on top of `V1__spring_batch_job_repository.sql`); later changes get their own `V3__…` migrations, V2 is never edited.
The field → column decisions live in one machine-readable file,
`modernization/carddemo-app/src/main/resources/db/copybook-column-map.csv`, and are enforced by tests:

| Test | Checks |
| --- | --- |
| `CopybookColumnMapTest` | the 11 layouts parse to their record lengths; **every leaf field** returned by `RecordLayout.leaves()` appears once, in storage order; only FILLER is unstored |
| `DataModelDocTest` | §5 of this document equals what the codec + CSV generate (regenerate: `mvn test -Dtest=DataModelDocTest -Dcarddemo.docs.write=true`) |
| `CoreSchemaIT` (Testcontainers `postgres:16-alpine`) | Flyway reaches V2; each mapped column has the type the rules below derive from the PIC, is `NOT NULL` and carries the copybook name as comment; PKs, AIX indexes, XREF FKs, `version` columns; **every sample record (EBCDIC and ASCII) loads with zero rejects** |

Binding ADRs: ADR-0003 (PIC X → string), ADR-0004 (NUMERIC, no float), ADR-0006 (level-88), ADR-0010 (`version`),
ADR-0011 (KSDS/AIX → table/index, keyset browse), ADR-0012 (GDG → dated storage).

## 1. Tables

| Dataset (DD / CICS file) | Copybook | Bytes | Organisation | Table | Primary key | Other indexes | `version` |
| --- | --- | ---: | --- | --- | --- | --- | --- |
| USRSEC | `CSUSR01Y` | 80 | KSDS `KEYS(8 0)` | `user_security` | `usr_id` | — | yes (COUSR02C rewrite, COUSR03C delete) |
| CUSTDATA / CUSTDAT | `CVCUS01Y` | 500 | KSDS `KEYS(9 0)` | `customer` | `cust_id` | — | yes (COACTUPC) |
| ACCTDATA / ACCTDAT | `CVACT01Y` | 300 | KSDS `KEYS(11 0)` | `account` | `acct_id` | — | yes (COACTUPC, COBIL00C) |
| CARDDATA / CARDDAT | `CVACT02Y` | 150 | KSDS `KEYS(16 0)`, AIX `CARDAIX KEYS(11 16)` | `card` | `card_num` | `card_acct_id_ix (acct_id, card_num)` | yes (COCRDUPC) |
| CARDXREF / CCXREF | `CVACT03Y` | 50 | KSDS `KEYS(16 0)`, AIX `CXACAIX KEYS(11 25)` | `card_xref` | `card_num` | `card_xref_acct_id_ix (acct_id, card_num)`, `card_xref_cust_id_ix` | no (read-only online) |
| TRANSACT | `CVTRA05Y` | 350 | KSDS `KEYS(16 0)`, AIX `KEYS(26 304) NONUNIQUEKEY` | `transaction` | `tran_id` | `transaction_proc_ts_ix (proc_ts, tran_id)` | no (insert-only: COTRN02C, COBIL00C) |
| DALYTRAN | `CVTRA06Y` | 350 | sequential (POSTTRAN input) | `daily_transaction` | `tran_id` | — | no |
| TCATBALF | `CVTRA01Y` | 50 | KSDS `KEYS(17 0)` | `tran_cat_balance` | `(acct_id, tran_type_cd, tran_cat_cd)` | — | no (batch only) |
| DISCGRP | `CVTRA02Y` | 50 | KSDS `KEYS(16 0)` | `disclosure_group` | `(acct_group_id, tran_type_cd, tran_cat_cd)` | — | no |
| TRANTYPE | `CVTRA03Y` | 60 | KSDS `KEYS(2 0)` | `transaction_type` | `tran_type_cd` | — | no |
| TRANCATG | `CVTRA04Y` | 60 | KSDS `KEYS(6 0)` | `transaction_category` | `(tran_type_cd, tran_cat_cd)` | — | no |
| GDG generations | — | — | GDG `LIMIT(5)` | `batch_output_file` | `output_file_id` | `(gdg_base, business_date DESC, job_execution_id DESC)` | no |

Composite keys keep the COBOL field order of the KSDS key (ADR-0011). The AIX indexes append the primary key so a
browse "by account, then card" is a single index range scan with a deterministic order (VSAM returns duplicate AIX
keys in primary-key order).

**DALYTRAN.** ADR-0011 keeps sequential files as files: the POSTTRAN step still reads the dated daily-transaction
file. `daily_transaction` is a staging table with the same layout so the phase-3 load and API/reconciliation queries
can see the current input; it has no FKs because unknown cards/accounts are POSTTRAN *rejects* (DALYREJS), not load
errors. `DALYTRAN.PS.INIT` (one zero-filled record) is the empty-file seed and is not loaded.

### Foreign keys

| FK | Why |
| --- | --- |
| `card_xref.card_num → card`, `card_xref.cust_id → customer`, `card_xref.acct_id → account` | the XREF file is the junction of card, customer and account |
| `tran_cat_balance.acct_id → account` | every TCATBALF key starts with an account id that CBACT04C reads from ACCTDAT |
| `transaction_category.tran_type_cd → transaction_type` | a category exists only under its type |

Deliberately **no** FK: `account.group_id → disclosure_group` (CBACT04C falls back to group `DEFAULT` when a group
has no row, and DISCGRP's key is group+type+category); `transaction.card_num`/type/category (COTRN02C and CBTRN02C
write them without reading TRANCATG); anything on `daily_transaction` (see above); `card.acct_id → account`
(COCRDUPC/COCRDLIC never validate it against ACCTDAT; kept loose so the phase-3 load order is free — revisit in s3.x
if a program starts relying on it).

## 2. Type rules (ADR-0003, ADR-0004)

| COBOL | PostgreSQL | Notes |
| --- | --- | --- |
| `PIC X(n)` | `VARCHAR(n)` | stored with trailing spaces trimmed (ADR-0003); all-space field → `''` |
| `PIC S9(n)V9(m)` DISPLAY or COMP-3 | `NUMERIC(n+m, m)` | e.g. `S9(10)V99` → `NUMERIC(12,2)`, `S9(04)V99` → `NUMERIC(6,2)` |
| `PIC 9(n)`, n ≤ 9 | `INTEGER` + `CHECK (col BETWEEN 0 AND 10^n − 1)` | unsigned, keeps the PIC width |
| `PIC 9(n)`, 10 ≤ n ≤ 18 | `BIGINT` + same range CHECK | `ACCT-ID 9(11)` |
| `real` / `double precision` / `money` | never | asserted by `CoreSchemaIT` |

Every copybook column is `NOT NULL`: a fixed-width record always holds a value. Numeric IDs (`CUST-ID`, `ACCT-ID`)
become integers; the leading zeros are presentation (`%011d`) and are restored by the codec on write.

### Dates and timestamps

| Field(s) | Stored as | Derived column | Why text is kept |
| --- | --- | --- | --- |
| `ACCT-OPEN-DATE`, `ACCT-EXPIRAION-DATE`, `ACCT-REISSUE-DATE` `X(10)` | `VARCHAR(10)` | `open_date_dt`, `expiration_date_dt`, `reissue_date_dt DATE` | CBTRN02C: `IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)` (text compare); COACTUPC edits them as year/month/day substrings |
| `CARD-EXPIRAION-DATE` `X(10)` | `VARCHAR(10)` | `expiration_date_dt DATE` | COCRDUPC splits and re-assembles the text |
| `CUST-DOB-YYYY-MM-DD` `X(10)` | `VARCHAR(10)` | `dob_dt DATE` | COACTUPC edits it as substrings |
| `TRAN-ORIG-TS`, `TRAN-PROC-TS`, `DALYTRAN-*-TS` `X(26)` | `VARCHAR(26)` | — | CBTRN03C filters `TRAN-PROC-TS (1:10)` between text dates; COTRN00C/the AIX order on the raw string; DB2-format `YYYY-MM-DD-HH.MM.SS.nnnnnn` is not a PostgreSQL timestamp literal |

No program treats any of these fields *only* as a date, so per the step rule the text column is the source of truth
and the `*_dt` columns are `GENERATED ALWAYS AS (cobol_iso_date(text)) STORED`. `cobol_iso_date` returns `NULL`
for anything that is not a valid `YYYY-MM-DD` calendar date (so the load never rejects a record); all 50 sample
accounts, cards and customers convert (asserted).

## 3. Level-88 values → CHECK constraints (ADR-0006)

The data copybooks declare no 88s; the condition names live in the programs that edit the fields. A CHECK is added
only where the 88 is the complete domain **and** every sample record (EBCDIC and ASCII) satisfies it:

| Column | 88 source | CHECK | Sample data |
| --- | --- | --- | --- |
| `user_security.usr_type` | COCOM01Y `CDEMO-USRTYP-ADMIN 'A'`, `CDEMO-USRTYP-USER 'U'` | `IN ('A','U')` | 10/10 `A` or `U` |
| `account.active_status` | COACTUPC `FLG-ACCT-STATUS-ISVALID VALUES 'Y','N'` | `IN ('Y','N')` | 50/50 |
| `card.active_status` | COCRDUPC card-status edit (`Y`/`N`) | `IN ('Y','N')` | 50/50 |
| `customer.pri_card_holder_ind` | COACTUPC `FLG-PRI-CARDHOLDER-ISVALID VALUES 'Y','N'` | `IN ('Y','N')` | 50/50 |

Divergence to note for s3.x: COUSR01C/COUSR02C accept any non-blank user type; ADR-0006 rejects undefined codes at
the API boundary, and the CHECK is the database backstop for the same rule.

Deliberately **not** constrained (sample data or programs contradict a tighter rule):

- ZIP codes (`account.addr_zip`, `customer.addr_zip`, `*.merchant_zip`): ACCTDATA record 49 holds `ZEROAPR` in
  `ACCT-ADDR-ZIP` in the EBCDIC sample (`A000000000` in the ASCII twin); no numeric/format CHECK.
- `account.group_id` / `disclosure_group.acct_group_id`: free text (`A000000000`, `DEFAULT`, `ZEROAPR`, …).
- `disclosure_group.int_rate`: no range CHECK; DISCGRP record 34 differs between the EBCDIC and ASCII samples
  (`src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt`) and both variants must load.
- Transaction type/category codes, `source`, state/country codes, phone numbers, SSN format: validated by programs,
  not 88s; no CHECK beyond the PIC width.

## 4. Browse bounds (ADR-0011) and optimistic locking (ADR-0010)

| Program | VSAM browse | Index used | Keyset query |
| --- | --- | --- | --- |
| COUSR00C | `STARTBR USRSEC` by user id | `user_security_pk` | first `usr_id >= :start`, next `usr_id > :last`, prev `usr_id < :first ORDER BY usr_id DESC` |
| COCRDLIC | `STARTBR CARDDAT` by card number (filter account/card) | `card_pk` | same pattern on `card_num` |
| COTRN00C | `STARTBR TRANSACT` by transaction id | `transaction_pk` | same pattern on `tran_id` |
| CBTRN03C / reports | sequential by processed timestamp (AIX) | `transaction_proc_ts_ix` | `(proc_ts, tran_id) > (:lastTs, :lastId) ORDER BY proc_ts, tran_id` |
| account → cards | `CARDAIX` / `CXACAIX` reads | `card_acct_id_ix`, `card_xref_acct_id_ix` | `acct_id = :acct AND card_num > :last ORDER BY card_num` |

`STARTBR` (GTEQ) includes the start key, continuation cursors exclude the boundary row, never `OFFSET`.

`version BIGINT NOT NULL DEFAULT 0` exists on exactly the tables the online programs `REWRITE`/`DELETE`:
`user_security`, `customer`, `account`, `card`. Insert-only (`transaction`) and batch-only tables have none; batch
steps that change a versioned row increment it (ADR-0010).

## GDG replacement (ADR-0012)

Decision: **dated files under a configurable output directory, catalogued in `batch_output_file`.**

- Every `(+1)` generation — `DALYREJS`, `SYSTRAN`, `TRANSACT.BKUP`, `TRANREPT`, and likewise `TRANSACT.DALY`,
  `TRANSACT.COMBINED`, `TCATBALF.BKUP`, `DISCGRP.BKUP`, `TRANCATG.PS.BKUP`, `TRANTYPE.BKUP` — is written to
  `${carddemo.batch.output-dir}/<gdg_base>/<gdg_base>.<businessDate>.<jobExecutionId>` (business date from the
  pinned `Clock`, ADR-0014) in the same record format the COBOL job writes, so golden-set comparison against
  `docs/validation/baseline/<JOB>/` stays a file diff.
- The step that writes it inserts one `batch_output_file` row (`gdg_base` without the `AWS.M2.CARDDEMO.` prefix,
  `business_date`, `job_execution_id` → `batch_job_execution`, `file_path`, `record_count`, `sha256`).
  `(0)` = newest row per `gdg_base`, `(-1)` = the one before (index `batch_output_file_generation_ix`).
- A restart reads the generation recorded in its job execution context, never "latest at restart time".
- Housekeeping keeps the newest 5 rows/files per base (`LIMIT(5) SCRATCH`).
- History-table rows for backups (ADR-0012's alternative) were not chosen: every backup is consumed as a file
  (REPRO / sort input), and files keep the baseline diff simple.

## 5. Field → column mapping (generated)

All 11 layouts, every elementary item from `Copybook.layout(...).leaves()` with its byte offset and length. "Null"
is the column's nullability. FILLER is not stored (ADR-0011).

<!-- BEGIN GENERATED: copybook-column-map (DataModelDocTest) -->

#### USRSEC (`CSUSR01Y`, 80 bytes) → `user_security`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 8 | `SEC-USR-ID` | `X(8)` | DISPLAY | `usr_id` | `varchar(8)` | NOT NULL |
| 8 | 20 | `SEC-USR-FNAME` | `X(20)` | DISPLAY | `first_name` | `varchar(20)` | NOT NULL |
| 28 | 20 | `SEC-USR-LNAME` | `X(20)` | DISPLAY | `last_name` | `varchar(20)` | NOT NULL |
| 48 | 8 | `SEC-USR-PWD` | `X(8)` | DISPLAY | `password` | `varchar(8)` | NOT NULL |
| 56 | 1 | `SEC-USR-TYPE` | `X` | DISPLAY | `usr_type` | `varchar(1)` | NOT NULL |
| 57 | 23 | `SEC-USR-FILLER` | `X(23)` | DISPLAY | — | not stored (FILLER) | — |

#### CUSTDATA (`CVCUS01Y`, 500 bytes) → `customer`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 9 | `CUST-ID` | `9(9)` | DISPLAY | `cust_id` | `integer` | NOT NULL |
| 9 | 25 | `CUST-FIRST-NAME` | `X(25)` | DISPLAY | `first_name` | `varchar(25)` | NOT NULL |
| 34 | 25 | `CUST-MIDDLE-NAME` | `X(25)` | DISPLAY | `middle_name` | `varchar(25)` | NOT NULL |
| 59 | 25 | `CUST-LAST-NAME` | `X(25)` | DISPLAY | `last_name` | `varchar(25)` | NOT NULL |
| 84 | 50 | `CUST-ADDR-LINE-1` | `X(50)` | DISPLAY | `addr_line_1` | `varchar(50)` | NOT NULL |
| 134 | 50 | `CUST-ADDR-LINE-2` | `X(50)` | DISPLAY | `addr_line_2` | `varchar(50)` | NOT NULL |
| 184 | 50 | `CUST-ADDR-LINE-3` | `X(50)` | DISPLAY | `addr_line_3` | `varchar(50)` | NOT NULL |
| 234 | 2 | `CUST-ADDR-STATE-CD` | `XX` | DISPLAY | `addr_state_cd` | `varchar(2)` | NOT NULL |
| 236 | 3 | `CUST-ADDR-COUNTRY-CD` | `X(3)` | DISPLAY | `addr_country_cd` | `varchar(3)` | NOT NULL |
| 239 | 10 | `CUST-ADDR-ZIP` | `X(10)` | DISPLAY | `addr_zip` | `varchar(10)` | NOT NULL |
| 249 | 15 | `CUST-PHONE-NUM-1` | `X(15)` | DISPLAY | `phone_num_1` | `varchar(15)` | NOT NULL |
| 264 | 15 | `CUST-PHONE-NUM-2` | `X(15)` | DISPLAY | `phone_num_2` | `varchar(15)` | NOT NULL |
| 279 | 9 | `CUST-SSN` | `9(9)` | DISPLAY | `ssn` | `integer` | NOT NULL |
| 288 | 20 | `CUST-GOVT-ISSUED-ID` | `X(20)` | DISPLAY | `govt_issued_id` | `varchar(20)` | NOT NULL |
| 308 | 10 | `CUST-DOB-YYYY-MM-DD` | `X(10)` | DISPLAY | `dob` | `varchar(10)` | NOT NULL |
| 318 | 10 | `CUST-EFT-ACCOUNT-ID` | `X(10)` | DISPLAY | `eft_account_id` | `varchar(10)` | NOT NULL |
| 328 | 1 | `CUST-PRI-CARD-HOLDER-IND` | `X` | DISPLAY | `pri_card_holder_ind` | `varchar(1)` | NOT NULL |
| 329 | 3 | `CUST-FICO-CREDIT-SCORE` | `9(3)` | DISPLAY | `fico_credit_score` | `integer` | NOT NULL |
| 332 | 168 | `FILLER` | `X(168)` | DISPLAY | — | not stored (FILLER) | — |

#### ACCTDATA (`CVACT01Y`, 300 bytes) → `account`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 11 | `ACCT-ID` | `9(11)` | DISPLAY | `acct_id` | `bigint` | NOT NULL |
| 11 | 1 | `ACCT-ACTIVE-STATUS` | `X` | DISPLAY | `active_status` | `varchar(1)` | NOT NULL |
| 12 | 12 | `ACCT-CURR-BAL` | `S9(10)V99` | DISPLAY | `curr_bal` | `numeric(12,2)` | NOT NULL |
| 24 | 12 | `ACCT-CREDIT-LIMIT` | `S9(10)V99` | DISPLAY | `credit_limit` | `numeric(12,2)` | NOT NULL |
| 36 | 12 | `ACCT-CASH-CREDIT-LIMIT` | `S9(10)V99` | DISPLAY | `cash_credit_limit` | `numeric(12,2)` | NOT NULL |
| 48 | 10 | `ACCT-OPEN-DATE` | `X(10)` | DISPLAY | `open_date` | `varchar(10)` | NOT NULL |
| 58 | 10 | `ACCT-EXPIRAION-DATE` | `X(10)` | DISPLAY | `expiration_date` | `varchar(10)` | NOT NULL |
| 68 | 10 | `ACCT-REISSUE-DATE` | `X(10)` | DISPLAY | `reissue_date` | `varchar(10)` | NOT NULL |
| 78 | 12 | `ACCT-CURR-CYC-CREDIT` | `S9(10)V99` | DISPLAY | `curr_cyc_credit` | `numeric(12,2)` | NOT NULL |
| 90 | 12 | `ACCT-CURR-CYC-DEBIT` | `S9(10)V99` | DISPLAY | `curr_cyc_debit` | `numeric(12,2)` | NOT NULL |
| 102 | 10 | `ACCT-ADDR-ZIP` | `X(10)` | DISPLAY | `addr_zip` | `varchar(10)` | NOT NULL |
| 112 | 10 | `ACCT-GROUP-ID` | `X(10)` | DISPLAY | `group_id` | `varchar(10)` | NOT NULL |
| 122 | 178 | `FILLER` | `X(178)` | DISPLAY | — | not stored (FILLER) | — |

#### CARDDATA (`CVACT02Y`, 150 bytes) → `card`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 16 | `CARD-NUM` | `X(16)` | DISPLAY | `card_num` | `varchar(16)` | NOT NULL |
| 16 | 11 | `CARD-ACCT-ID` | `9(11)` | DISPLAY | `acct_id` | `bigint` | NOT NULL |
| 27 | 3 | `CARD-CVV-CD` | `9(3)` | DISPLAY | `cvv_cd` | `integer` | NOT NULL |
| 30 | 50 | `CARD-EMBOSSED-NAME` | `X(50)` | DISPLAY | `embossed_name` | `varchar(50)` | NOT NULL |
| 80 | 10 | `CARD-EXPIRAION-DATE` | `X(10)` | DISPLAY | `expiration_date` | `varchar(10)` | NOT NULL |
| 90 | 1 | `CARD-ACTIVE-STATUS` | `X` | DISPLAY | `active_status` | `varchar(1)` | NOT NULL |
| 91 | 59 | `FILLER` | `X(59)` | DISPLAY | — | not stored (FILLER) | — |

#### CARDXREF (`CVACT03Y`, 50 bytes) → `card_xref`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 16 | `XREF-CARD-NUM` | `X(16)` | DISPLAY | `card_num` | `varchar(16)` | NOT NULL |
| 16 | 9 | `XREF-CUST-ID` | `9(9)` | DISPLAY | `cust_id` | `integer` | NOT NULL |
| 25 | 11 | `XREF-ACCT-ID` | `9(11)` | DISPLAY | `acct_id` | `bigint` | NOT NULL |
| 36 | 14 | `FILLER` | `X(14)` | DISPLAY | — | not stored (FILLER) | — |

#### TRANSACT (`CVTRA05Y`, 350 bytes) → `transaction`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 16 | `TRAN-ID` | `X(16)` | DISPLAY | `tran_id` | `varchar(16)` | NOT NULL |
| 16 | 2 | `TRAN-TYPE-CD` | `XX` | DISPLAY | `tran_type_cd` | `varchar(2)` | NOT NULL |
| 18 | 4 | `TRAN-CAT-CD` | `9(4)` | DISPLAY | `tran_cat_cd` | `integer` | NOT NULL |
| 22 | 10 | `TRAN-SOURCE` | `X(10)` | DISPLAY | `source` | `varchar(10)` | NOT NULL |
| 32 | 100 | `TRAN-DESC` | `X(100)` | DISPLAY | `description` | `varchar(100)` | NOT NULL |
| 132 | 11 | `TRAN-AMT` | `S9(9)V99` | DISPLAY | `amount` | `numeric(11,2)` | NOT NULL |
| 143 | 9 | `TRAN-MERCHANT-ID` | `9(9)` | DISPLAY | `merchant_id` | `integer` | NOT NULL |
| 152 | 50 | `TRAN-MERCHANT-NAME` | `X(50)` | DISPLAY | `merchant_name` | `varchar(50)` | NOT NULL |
| 202 | 50 | `TRAN-MERCHANT-CITY` | `X(50)` | DISPLAY | `merchant_city` | `varchar(50)` | NOT NULL |
| 252 | 10 | `TRAN-MERCHANT-ZIP` | `X(10)` | DISPLAY | `merchant_zip` | `varchar(10)` | NOT NULL |
| 262 | 16 | `TRAN-CARD-NUM` | `X(16)` | DISPLAY | `card_num` | `varchar(16)` | NOT NULL |
| 278 | 26 | `TRAN-ORIG-TS` | `X(26)` | DISPLAY | `orig_ts` | `varchar(26)` | NOT NULL |
| 304 | 26 | `TRAN-PROC-TS` | `X(26)` | DISPLAY | `proc_ts` | `varchar(26)` | NOT NULL |
| 330 | 20 | `FILLER` | `X(20)` | DISPLAY | — | not stored (FILLER) | — |

#### DALYTRAN (`CVTRA06Y`, 350 bytes) → `daily_transaction`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 16 | `DALYTRAN-ID` | `X(16)` | DISPLAY | `tran_id` | `varchar(16)` | NOT NULL |
| 16 | 2 | `DALYTRAN-TYPE-CD` | `XX` | DISPLAY | `tran_type_cd` | `varchar(2)` | NOT NULL |
| 18 | 4 | `DALYTRAN-CAT-CD` | `9(4)` | DISPLAY | `tran_cat_cd` | `integer` | NOT NULL |
| 22 | 10 | `DALYTRAN-SOURCE` | `X(10)` | DISPLAY | `source` | `varchar(10)` | NOT NULL |
| 32 | 100 | `DALYTRAN-DESC` | `X(100)` | DISPLAY | `description` | `varchar(100)` | NOT NULL |
| 132 | 11 | `DALYTRAN-AMT` | `S9(9)V99` | DISPLAY | `amount` | `numeric(11,2)` | NOT NULL |
| 143 | 9 | `DALYTRAN-MERCHANT-ID` | `9(9)` | DISPLAY | `merchant_id` | `integer` | NOT NULL |
| 152 | 50 | `DALYTRAN-MERCHANT-NAME` | `X(50)` | DISPLAY | `merchant_name` | `varchar(50)` | NOT NULL |
| 202 | 50 | `DALYTRAN-MERCHANT-CITY` | `X(50)` | DISPLAY | `merchant_city` | `varchar(50)` | NOT NULL |
| 252 | 10 | `DALYTRAN-MERCHANT-ZIP` | `X(10)` | DISPLAY | `merchant_zip` | `varchar(10)` | NOT NULL |
| 262 | 16 | `DALYTRAN-CARD-NUM` | `X(16)` | DISPLAY | `card_num` | `varchar(16)` | NOT NULL |
| 278 | 26 | `DALYTRAN-ORIG-TS` | `X(26)` | DISPLAY | `orig_ts` | `varchar(26)` | NOT NULL |
| 304 | 26 | `DALYTRAN-PROC-TS` | `X(26)` | DISPLAY | `proc_ts` | `varchar(26)` | NOT NULL |
| 330 | 20 | `FILLER` | `X(20)` | DISPLAY | — | not stored (FILLER) | — |

#### TCATBALF (`CVTRA01Y`, 50 bytes) → `tran_cat_balance`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 11 | `TRANCAT-ACCT-ID` | `9(11)` | DISPLAY | `acct_id` | `bigint` | NOT NULL |
| 11 | 2 | `TRANCAT-TYPE-CD` | `XX` | DISPLAY | `tran_type_cd` | `varchar(2)` | NOT NULL |
| 13 | 4 | `TRANCAT-CD` | `9(4)` | DISPLAY | `tran_cat_cd` | `integer` | NOT NULL |
| 17 | 11 | `TRAN-CAT-BAL` | `S9(9)V99` | DISPLAY | `balance` | `numeric(11,2)` | NOT NULL |
| 28 | 22 | `FILLER` | `X(22)` | DISPLAY | — | not stored (FILLER) | — |

#### DISCGRP (`CVTRA02Y`, 50 bytes) → `disclosure_group`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 10 | `DIS-ACCT-GROUP-ID` | `X(10)` | DISPLAY | `acct_group_id` | `varchar(10)` | NOT NULL |
| 10 | 2 | `DIS-TRAN-TYPE-CD` | `XX` | DISPLAY | `tran_type_cd` | `varchar(2)` | NOT NULL |
| 12 | 4 | `DIS-TRAN-CAT-CD` | `9(4)` | DISPLAY | `tran_cat_cd` | `integer` | NOT NULL |
| 16 | 6 | `DIS-INT-RATE` | `S9(4)V99` | DISPLAY | `int_rate` | `numeric(6,2)` | NOT NULL |
| 22 | 28 | `FILLER` | `X(28)` | DISPLAY | — | not stored (FILLER) | — |

#### TRANTYPE (`CVTRA03Y`, 60 bytes) → `transaction_type`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 2 | `TRAN-TYPE` | `XX` | DISPLAY | `tran_type_cd` | `varchar(2)` | NOT NULL |
| 2 | 50 | `TRAN-TYPE-DESC` | `X(50)` | DISPLAY | `description` | `varchar(50)` | NOT NULL |
| 52 | 8 | `FILLER` | `X(8)` | DISPLAY | — | not stored (FILLER) | — |

#### TRANCATG (`CVTRA04Y`, 60 bytes) → `transaction_category`

| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |
|---:|---:|---|---|---|---|---|---|
| 0 | 2 | `TRAN-TYPE-CD` | `XX` | DISPLAY | `tran_type_cd` | `varchar(2)` | NOT NULL |
| 2 | 4 | `TRAN-CAT-CD` | `9(4)` | DISPLAY | `tran_cat_cd` | `integer` | NOT NULL |
| 6 | 50 | `TRAN-CAT-TYPE-DESC` | `X(50)` | DISPLAY | `description` | `varchar(50)` | NOT NULL |
| 56 | 4 | `FILLER` | `X(4)` | DISPLAY | — | not stored (FILLER) | — |

<!-- END GENERATED: copybook-column-map -->
