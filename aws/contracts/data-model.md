# Contract: Canonical data model (Aurora PostgreSQL)

Status: **v1 (Discovery session)**. Schema: **`carddemo`**. Owner of DDL: data-migration session
(Flyway). Online services, batch and validation MUST use exactly these table/column names and types.

## 1. Naming and type rules

1. Table name = singular snake_case entity name (listed per table below).
2. Column name = snake_case of the copybook field with the record prefix dropped
   (`ACCT-CURR-BAL` → `curr_bal`, `CARD-EMBOSSED-NAME` → `embossed_name`).
   **Exception:** identifier/key columns keep their entity prefix so they are unambiguous in joins
   (`acct_id`, `card_num`, `cust_id`, `tran_id`, `user_id`, `type_cd`, `cat_cd`).
3. Source misspellings are corrected in column names and recorded here:
   `ACCT-EXPIRAION-DATE`/`CARD-EXPIRAION-DATE` → `expiration_date`;
   DB2 `MERCHANT_CATAGORY_CODE` / `PA-MERCHANT-CATAGORY-CODE` → `merchant_category_code`.
4. Type mapping:

| COBOL / DB2 | PostgreSQL |
|---|---|
| `PIC 9(n)` identifier (account, customer, card, merchant ids) | `NUMERIC(n,0)` for n ≥ 10, `INTEGER` for n ≤ 9 *only* when used arithmetically; ids are stored as listed per table |
| `PIC S9(p-s)V9(s)` (display or `COMP-3`) | `NUMERIC(p,s)` e.g. `S9(10)V99` → `NUMERIC(12,2)`, `S9(09)V99` → `NUMERIC(11,2)`, `S9(04)V99` → `NUMERIC(6,2)` |
| `PIC S9(4) COMP` counters | `SMALLINT` |
| `PIC X(n)` fixed code (1–4 chars, e.g. status, type code) | `CHAR(n)` |
| `PIC X(n)` free text | `VARCHAR(n)` (trailing spaces trimmed on load) |
| `PIC X(10)` holding `YYYY-MM-DD` | `DATE` (all-spaces/zeros → `NULL`) |
| `PIC X(26)` holding DB2-format timestamp `YYYY-MM-DD-HH.MM.SS.NNNNNN` | `TIMESTAMP(6)` (no zone) |
| DB2 `CHAR(n)` / `VARCHAR(n)` / `DECIMAL(p,s)` / `SMALLINT` / `DATE` / `TIMESTAMP` | same PostgreSQL type |
| `FILLER` | not stored |

5. Every mutable master table carries a technical column `version BIGINT NOT NULL DEFAULT 0`
   (optimistic locking; replaces the CICS `READ UPDATE` + "Record changed by some one else" compare
   in `COACTUPC`/`COCRDUPC`). Technical columns are not part of the legacy record layout.
6. Card numbers are `CHAR(16)` everywhere (`CARD-NUM PIC X(16)`; `CDEMO-CARD-NUM PIC 9(16)` in the
   COMMAREA is the same value).
7. VSAM KSDS primary key → `PRIMARY KEY`; VSAM alternate index (AIX/PATH) → secondary index (non-unique
   unless the AIX was defined `UNIQUEKEY`).

## 2. Core tables (from VSAM KSDS)

### 2.1 `user_security` ← `CSUSR01Y` / `AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS` (CICS file `USRSEC`, 80 bytes, key 8 @0)

| Column | Type | Null | Source field |
|---|---|---|---|
| `user_id` | `VARCHAR(8)` | PK | `SEC-USR-ID X(08)` |
| `first_name` | `VARCHAR(20)` | not null | `SEC-USR-FNAME X(20)` |
| `last_name` | `VARCHAR(20)` | not null | `SEC-USR-LNAME X(20)` |
| `password_hash` | `VARCHAR(100)` | not null | `SEC-USR-PWD X(08)` — **BCrypt hash of the legacy value** (legacy stores plain text; loader hashes) |
| `user_type` | `CHAR(1)` | not null, `CHECK (user_type IN ('A','U'))` | `SEC-USR-TYPE X(01)` (`A` admin → role `ADMIN`, `U` → `USER`) |
| `version` | `BIGINT` | not null default 0 | technical |

User ids are stored upper-case trimmed (COSGN00C upper-cases the entered id/password before the READ).
Because the legacy password compare is case-insensitive in effect (input upper-cased), the loader hashes the
upper-cased legacy password and the signon service upper-cases input before verifying.

### 2.2 `account` ← `CVACT01Y` / `ACCTDATA.VSAM.KSDS` (CICS `ACCTDAT`, 300 bytes, key 11 @0)

| Column | Type | Null | Source field |
|---|---|---|---|
| `acct_id` | `NUMERIC(11,0)` | PK | `ACCT-ID 9(11)` |
| `active_status` | `CHAR(1)` | not null | `ACCT-ACTIVE-STATUS X(01)` (`Y`/`N`) |
| `curr_bal` | `NUMERIC(12,2)` | not null | `ACCT-CURR-BAL S9(10)V99` |
| `credit_limit` | `NUMERIC(12,2)` | not null | `ACCT-CREDIT-LIMIT S9(10)V99` |
| `cash_credit_limit` | `NUMERIC(12,2)` | not null | `ACCT-CASH-CREDIT-LIMIT S9(10)V99` |
| `open_date` | `DATE` | null | `ACCT-OPEN-DATE X(10)` |
| `expiration_date` | `DATE` | null | `ACCT-EXPIRAION-DATE X(10)` |
| `reissue_date` | `DATE` | null | `ACCT-REISSUE-DATE X(10)` |
| `curr_cyc_credit` | `NUMERIC(12,2)` | not null | `ACCT-CURR-CYC-CREDIT S9(10)V99` |
| `curr_cyc_debit` | `NUMERIC(12,2)` | not null | `ACCT-CURR-CYC-DEBIT S9(10)V99` |
| `addr_zip` | `VARCHAR(10)` | null | `ACCT-ADDR-ZIP X(10)` |
| `group_id` | `VARCHAR(10)` | null | `ACCT-GROUP-ID X(10)` (joins `disclosure_group.acct_group_id`) |
| `version` | `BIGINT` | not null default 0 | technical |

### 2.3 `card` ← `CVACT02Y` / `CARDDATA.VSAM.KSDS` (CICS `CARDDAT`, 150 bytes, key 16 @0; AIX `CARDAIX` key 11 @16 non-unique)

| Column | Type | Null | Source field |
|---|---|---|---|
| `card_num` | `CHAR(16)` | PK | `CARD-NUM X(16)` |
| `acct_id` | `NUMERIC(11,0)` | not null, FK → `account` | `CARD-ACCT-ID 9(11)` |
| `cvv_cd` | `SMALLINT` | not null | `CARD-CVV-CD 9(03)` |
| `embossed_name` | `VARCHAR(50)` | not null | `CARD-EMBOSSED-NAME X(50)` |
| `expiration_date` | `DATE` | null | `CARD-EXPIRAION-DATE X(10)` |
| `active_status` | `CHAR(1)` | not null | `CARD-ACTIVE-STATUS X(01)` (`Y`/`N`) |
| `version` | `BIGINT` | not null default 0 | technical |

Index: `ix_card_acct_id (acct_id)` ← AIX `CARDDATA.VSAM.AIX` (path `CARDAIX`).

### 2.4 `card_xref` ← `CVACT03Y` / `CARDXREF.VSAM.KSDS` (CICS `CCXREF`, 50 bytes, key 16 @0; AIX `CXACAIX` key 11 @25 non-unique)

| Column | Type | Null | Source field |
|---|---|---|---|
| `card_num` | `CHAR(16)` | PK, FK → `card` | `XREF-CARD-NUM X(16)` |
| `cust_id` | `INTEGER` | not null, FK → `customer` | `XREF-CUST-ID 9(09)` |
| `acct_id` | `NUMERIC(11,0)` | not null, FK → `account` | `XREF-ACCT-ID 9(11)` |

Index: `ix_card_xref_acct_id (acct_id)` ← AIX `CARDXREF.VSAM.AIX` (path `CXACAIX`).
FKs are declared `DEFERRABLE INITIALLY DEFERRED` so loaders can insert in any order.

### 2.5 `customer` ← `CVCUS01Y` (same layout as `CUSTREC`) / `CUSTDATA.VSAM.KSDS` (CICS `CUSTDAT`, 500 bytes, key 9 @0)

| Column | Type | Null | Source field |
|---|---|---|---|
| `cust_id` | `INTEGER` | PK | `CUST-ID 9(09)` |
| `first_name` | `VARCHAR(25)` | not null | `CUST-FIRST-NAME X(25)` |
| `middle_name` | `VARCHAR(25)` | null | `CUST-MIDDLE-NAME X(25)` |
| `last_name` | `VARCHAR(25)` | not null | `CUST-LAST-NAME X(25)` |
| `addr_line_1` | `VARCHAR(50)` | null | `CUST-ADDR-LINE-1 X(50)` |
| `addr_line_2` | `VARCHAR(50)` | null | `CUST-ADDR-LINE-2 X(50)` |
| `addr_line_3` | `VARCHAR(50)` | null | `CUST-ADDR-LINE-3 X(50)` |
| `addr_state_cd` | `CHAR(2)` | null | `CUST-ADDR-STATE-CD X(02)` |
| `addr_country_cd` | `CHAR(3)` | null | `CUST-ADDR-COUNTRY-CD X(03)` |
| `addr_zip` | `VARCHAR(10)` | null | `CUST-ADDR-ZIP X(10)` |
| `phone_num_1` | `VARCHAR(15)` | null | `CUST-PHONE-NUM-1 X(15)` (format `(999)999-9999`) |
| `phone_num_2` | `VARCHAR(15)` | null | `CUST-PHONE-NUM-2 X(15)` |
| `ssn` | `CHAR(9)` | not null | `CUST-SSN 9(09)` (kept as zero-padded text) |
| `govt_issued_id` | `VARCHAR(20)` | null | `CUST-GOVT-ISSUED-ID X(20)` |
| `dob` | `DATE` | null | `CUST-DOB-YYYY-MM-DD X(10)` |
| `eft_account_id` | `VARCHAR(10)` | null | `CUST-EFT-ACCOUNT-ID X(10)` |
| `pri_card_holder_ind` | `CHAR(1)` | null | `CUST-PRI-CARD-HOLDER-IND X(01)` (`Y`/`N`) |
| `fico_credit_score` | `SMALLINT` | null | `CUST-FICO-CREDIT-SCORE 9(03)` (COACTUPC validates 300–850) |
| `version` | `BIGINT` | not null default 0 | technical |

### 2.6 `transaction` ← `CVTRA05Y` / `TRANSACT.VSAM.KSDS` (CICS `TRANSACT`, 350 bytes, key 16 @0; AIX key 26 @304 non-unique = `TRAN-PROC-TS`)

| Column | Type | Null | Source field |
|---|---|---|---|
| `tran_id` | `CHAR(16)` | PK | `TRAN-ID X(16)` (numeric string; online add = max + 1, zero-padded to 16) |
| `type_cd` | `CHAR(2)` | not null, FK → `transaction_type` | `TRAN-TYPE-CD X(02)` |
| `cat_cd` | `SMALLINT` | not null | `TRAN-CAT-CD 9(04)`; FK (`type_cd`,`cat_cd`) → `transaction_category` |
| `source` | `VARCHAR(10)` | null | `TRAN-SOURCE X(10)` |
| `description` | `VARCHAR(100)` | null | `TRAN-DESC X(100)` |
| `amt` | `NUMERIC(11,2)` | not null | `TRAN-AMT S9(09)V99` |
| `merchant_id` | `INTEGER` | null | `TRAN-MERCHANT-ID 9(09)` |
| `merchant_name` | `VARCHAR(50)` | null | `TRAN-MERCHANT-NAME X(50)` |
| `merchant_city` | `VARCHAR(50)` | null | `TRAN-MERCHANT-CITY X(50)` |
| `merchant_zip` | `VARCHAR(10)` | null | `TRAN-MERCHANT-ZIP X(10)` |
| `card_num` | `CHAR(16)` | not null | `TRAN-CARD-NUM X(16)` (FK → `card` not enforced: batch may post before card load) |
| `orig_ts` | `TIMESTAMP(6)` | null | `TRAN-ORIG-TS X(26)` |
| `proc_ts` | `TIMESTAMP(6)` | null | `TRAN-PROC-TS X(26)` |

Indexes: `ix_transaction_proc_ts (proc_ts)` ← AIX `TRANSACT.VSAM.AIX` (offset 304 len 26);
`ix_transaction_card_num (card_num)` (needed by statements `CBSTM03A`, which re-keys by card via `TRXFL`).

### 2.7 `daily_transaction` ← `CVTRA06Y` / `AWS.M2.CARDDEMO.DALYTRAN.PS` (sequential, FB 350)

Staging table for the daily posting job (`POSTTRAN`). Same columns and types as `transaction`
(field prefix `DALYTRAN-` dropped) plus:

| Column | Type | Null | Meaning |
|---|---|---|---|
| `run_id` | `VARCHAR(40)` | PK part 1 | batch run that loaded the file (see `batch.md`) |
| `load_seq` | `INTEGER` | PK part 2 | record order in the input file (processing order must be preserved) |
| `tran_id` | `CHAR(16)` | not null, **not unique** | `DALYTRAN-ID` |
| `post_status` | `CHAR(1)` | null | `P` posted, `R` rejected, null = not yet processed; set atomically with the posting changes (restart marker, `batch.md` §4) |
| `reject_reason` | `SMALLINT` | null | `CBTRN02C` validation code 100/101/102/103/109 when `post_status='R'` |

Staging never de-duplicates: every input record is kept. A repeated `tran_id` fails when posted to `transaction`
(PK), which is the legacy `CBTRN02C` `WRITE TRANSACT` error path (`APPL-RESULT 12` → abend) → job exit 12.

The primary input is the S3 file (`batch.md` §1.2, §2.4); the table is an optional staging copy used by the batch
session. No FKs.

### 2.8 `tran_cat_balance` ← `CVTRA01Y` / `TCATBALF.VSAM.KSDS` (50 bytes, key 17 @0)

| Column | Type | Null | Source field |
|---|---|---|---|
| `acct_id` | `NUMERIC(11,0)` | PK part 1 | `TRANCAT-ACCT-ID 9(11)` |
| `type_cd` | `CHAR(2)` | PK part 2 | `TRANCAT-TYPE-CD X(02)` |
| `cat_cd` | `SMALLINT` | PK part 3 | `TRANCAT-CD 9(04)` |
| `balance` | `NUMERIC(11,2)` | not null | `TRAN-CAT-BAL S9(09)V99` |
| `version` | `BIGINT` | not null default 0 | technical |

### 2.9 `disclosure_group` ← `CVTRA02Y` / `DISCGRP.VSAM.KSDS` (50 bytes, key 16 @0)

| Column | Type | Null | Source field |
|---|---|---|---|
| `acct_group_id` | `VARCHAR(10)` | PK part 1 | `DIS-ACCT-GROUP-ID X(10)` (`DEFAULT` row is the fallback used by `CBACT04C`) |
| `type_cd` | `CHAR(2)` | PK part 2 | `DIS-TRAN-TYPE-CD X(02)` |
| `cat_cd` | `SMALLINT` | PK part 3 | `DIS-TRAN-CAT-CD 9(04)` |
| `int_rate` | `NUMERIC(6,2)` | not null | `DIS-INT-RATE S9(04)V99` (annual %, `CBACT04C`: monthly interest = balance × rate / 1200) |

### 2.10 `transaction_type` ← `CVTRA03Y` / `TRANTYPE.VSAM.KSDS` (60 bytes, key 2 @0) **and** DB2 `CARDDEMO.TRANSACTION_TYPE`

One table serves both the core VSAM reference file and the optional DB2 sub-app.

| Column | Type | Null | Source field |
|---|---|---|---|
| `type_cd` | `CHAR(2)` | PK | `TRAN-TYPE X(02)` / DB2 `TR_TYPE CHAR(2)` |
| `description` | `VARCHAR(50)` | not null | `TRAN-TYPE-DESC X(50)` / DB2 `TR_DESCRIPTION VARCHAR(50)` |
| `version` | `BIGINT` | not null default 0 | technical (mutable via `api.md` §10.2) |

### 2.11 `transaction_category` ← `CVTRA04Y` / `TRANCATG.VSAM.KSDS` (60 bytes, key 6 @0) **and** DB2 `CARDDEMO.TRANSACTION_TYPE_CATEGORY`

| Column | Type | Null | Source field |
|---|---|---|---|
| `type_cd` | `CHAR(2)` | PK part 1, FK → `transaction_type` | `TRAN-TYPE-CD X(02)` / DB2 `TRC_TYPE_CODE CHAR(2)` |
| `cat_cd` | `SMALLINT` | PK part 2 | `TRAN-CAT-CD 9(04)` / DB2 `TRC_TYPE_CATEGORY CHAR(4)` (numeric string → integer) |
| `description` | `VARCHAR(50)` | not null | `TRAN-CAT-TYPE-DESC X(50)` / DB2 `TRC_CAT_DATA VARCHAR(50)` |

DB2 FK `TRANSACTION_TYPE_CATEGORY → TRANSACTION_TYPE` is kept (`ON DELETE RESTRICT`, matching DB2 default).
`COTRTUPC`/`COTRTLIC` delete of a type that has categories therefore fails with a FK violation → HTTP 409.

## 3. Optional sub-app tables

### 3.1 `authfrds` ← DB2 `CARDDEMO.AUTHFRDS` (`app/app-authorization-ims-db2-mq/ddl/AUTHFRDS.ddl`, index `XAUTHFRD`)

| Column | Type | Null |
|---|---|---|
| `card_num` | `CHAR(16)` | PK part 1 |
| `auth_ts` | `TIMESTAMP(6)` | PK part 2 |
| `auth_type` | `CHAR(4)` | null |
| `card_expiry_date` | `CHAR(4)` | null |
| `message_type` | `CHAR(6)` | null |
| `message_source` | `CHAR(6)` | null |
| `auth_id_code` | `CHAR(6)` | null |
| `auth_resp_code` | `CHAR(2)` | null |
| `auth_resp_reason` | `CHAR(4)` | null |
| `processing_code` | `CHAR(6)` | null |
| `transaction_amt` | `NUMERIC(12,2)` | null |
| `approved_amt` | `NUMERIC(12,2)` | null |
| `merchant_category_code` | `CHAR(4)` | null (DB2 `MERCHANT_CATAGORY_CODE`) |
| `acqr_country_code` | `CHAR(3)` | null |
| `pos_entry_mode` | `SMALLINT` | null |
| `merchant_id` | `CHAR(15)` | null |
| `merchant_name` | `VARCHAR(22)` | null |
| `merchant_city` | `CHAR(13)` | null |
| `merchant_state` | `CHAR(2)` | null |
| `merchant_zip` | `CHAR(9)` | null |
| `transaction_id` | `CHAR(15)` | null |
| `match_status` | `CHAR(1)` | null (`P` pending, `D` declined, `E` expired, `M` matched — `CIPAUDTY`) |
| `auth_fraud` | `CHAR(1)` | null (`F` confirmed, `R` removed) |
| `fraud_rpt_date` | `DATE` | null |
| `acct_id` | `NUMERIC(11,0)` | null |
| `cust_id` | `NUMERIC(9,0)` | null |

Index: `ix_authfrds_card_ts (card_num, auth_ts DESC)` ← `XAUTHFRD`.

### 3.2 `pending_auth_summary` ← IMS segment `PAUTSUM0` (DBD `DBPAUTP0`, 100 bytes) / copybook `CIPAUSMY`

**Replatform candidate** (see inventory §10). Defined so a relational refactor has a fixed target if chosen.

| Column | Type | Null | Source field |
|---|---|---|---|
| `acct_id` | `NUMERIC(11,0)` | PK | `PA-ACCT-ID S9(11) COMP-3` (IMS key `ACCNTID`, 6 bytes packed) |
| `cust_id` | `INTEGER` | not null | `PA-CUST-ID 9(09)` |
| `auth_status` | `CHAR(1)` | null | `PA-AUTH-STATUS X(01)` |
| `account_status` | `CHAR(2)[]` (max 5) | null | `PA-ACCOUNT-STATUS X(02) OCCURS 5` |
| `credit_limit` | `NUMERIC(11,2)` | not null | `PA-CREDIT-LIMIT S9(09)V99 COMP-3` |
| `cash_limit` | `NUMERIC(11,2)` | not null | `PA-CASH-LIMIT` |
| `credit_balance` | `NUMERIC(11,2)` | not null | `PA-CREDIT-BALANCE` |
| `cash_balance` | `NUMERIC(11,2)` | not null | `PA-CASH-BALANCE` |
| `approved_auth_cnt` | `SMALLINT` | not null | `PA-APPROVED-AUTH-CNT S9(04) COMP` |
| `declined_auth_cnt` | `SMALLINT` | not null | `PA-DECLINED-AUTH-CNT` |
| `approved_auth_amt` | `NUMERIC(11,2)` | not null | `PA-APPROVED-AUTH-AMT` |
| `declined_auth_amt` | `NUMERIC(11,2)` | not null | `PA-DECLINED-AUTH-AMT` |
| `version` | `BIGINT` | not null default 0 | technical |

### 3.3 `pending_auth_detail` ← IMS segment `PAUTDTL1` (child of `PAUTSUM0`, 200 bytes) / copybook `CIPAUDTY`

| Column | Type | Null | Source field |
|---|---|---|---|
| `acct_id` | `NUMERIC(11,0)` | PK part 1, FK → `pending_auth_summary` ON DELETE CASCADE | parent key |
| `auth_date_9c` | `INTEGER` | PK part 2 | `PA-AUTH-DATE-9C S9(05) COMP-3` (complemented date → newest first) |
| `auth_time_9c` | `BIGINT` | PK part 3 | `PA-AUTH-TIME-9C S9(09) COMP-3` |
| `auth_orig_date` | `CHAR(6)` | null | `PA-AUTH-ORIG-DATE X(06)` (YYMMDD) |
| `auth_orig_time` | `CHAR(6)` | null | `PA-AUTH-ORIG-TIME X(06)` (HHMMSS) |
| `card_num` | `CHAR(16)` | not null | `PA-CARD-NUM` |
| `auth_type` | `CHAR(4)` | null | `PA-AUTH-TYPE` |
| `card_expiry_date` | `CHAR(4)` | null | `PA-CARD-EXPIRY-DATE` |
| `message_type` | `CHAR(6)` | null | `PA-MESSAGE-TYPE` |
| `message_source` | `CHAR(6)` | null | `PA-MESSAGE-SOURCE` |
| `auth_id_code` | `CHAR(6)` | null | `PA-AUTH-ID-CODE` |
| `auth_resp_code` | `CHAR(2)` | null | `PA-AUTH-RESP-CODE` (`00` approved, `05` declined) |
| `auth_resp_reason` | `CHAR(4)` | null | `PA-AUTH-RESP-REASON` (see `messaging.md` §4.3) |
| `processing_code` | `INTEGER` | null | `PA-PROCESSING-CODE 9(06)` |
| `transaction_amt` | `NUMERIC(12,2)` | not null | `PA-TRANSACTION-AMT S9(10)V99 COMP-3` |
| `approved_amt` | `NUMERIC(12,2)` | not null | `PA-APPROVED-AMT S9(10)V99 COMP-3` |
| `merchant_category_code` | `CHAR(4)` | null | `PA-MERCHANT-CATAGORY-CODE` |
| `acqr_country_code` | `CHAR(3)` | null | `PA-ACQR-COUNTRY-CODE` |
| `pos_entry_mode` | `SMALLINT` | null | `PA-POS-ENTRY-MODE 9(02)` |
| `merchant_id` | `VARCHAR(15)` | null | `PA-MERCHANT-ID` |
| `merchant_name` | `VARCHAR(22)` | null | `PA-MERCHANT-NAME` |
| `merchant_city` | `VARCHAR(13)` | null | `PA-MERCHANT-CITY` |
| `merchant_state` | `CHAR(2)` | null | `PA-MERCHANT-STATE` |
| `merchant_zip` | `VARCHAR(9)` | null | `PA-MERCHANT-ZIP` |
| `transaction_id` | `VARCHAR(15)` | null | `PA-TRANSACTION-ID` |
| `match_status` | `CHAR(1)` | null | `PA-MATCH-STATUS` (`P`/`D`/`E`/`M`) |
| `auth_fraud` | `CHAR(1)` | null | `PA-AUTH-FRAUD` (`F`/`R`) |
| `fraud_rpt_date` | `CHAR(8)` | null | `PA-FRAUD-RPT-DATE X(08)` |

Index: `ix_pending_auth_detail_card (card_num)`. The IMS secondary index DBD `DBPAUTX0` (segment
`PAUTINDX`, key `INDXSEQ` over `ACCNTID`) is covered by the primary key on `pending_auth_summary`.

## 4. Record → table cross-reference

| Copybook | Legacy dataset | Table |
|---|---|---|
| `CSUSR01Y` | `USRSEC.VSAM.KSDS` (+ `USRSEC.VSAM.ESDS`/`.RRDS` demo copies) | `user_security` |
| `CVACT01Y` | `ACCTDATA.VSAM.KSDS` | `account` |
| `CVACT02Y` | `CARDDATA.VSAM.KSDS` (+AIX) | `card` |
| `CVACT03Y` | `CARDXREF.VSAM.KSDS` (+AIX) | `card_xref` |
| `CVCUS01Y`, `CUSTREC` | `CUSTDATA.VSAM.KSDS` | `customer` |
| `CVTRA05Y`, `COSTM01` (re-keyed copy) | `TRANSACT.VSAM.KSDS` (+AIX), `TRXFL.VSAM.KSDS` | `transaction` |
| `CVTRA06Y` | `DALYTRAN.PS` | `daily_transaction` (staging) / S3 |
| `CVTRA01Y` | `TCATBALF.VSAM.KSDS` | `tran_cat_balance` |
| `CVTRA02Y` | `DISCGRP.VSAM.KSDS` | `disclosure_group` |
| `CVTRA03Y`, DB2 `DCLTRTYP` | `TRANTYPE.VSAM.KSDS`, DB2 `TRANSACTION_TYPE` | `transaction_type` |
| `CVTRA04Y`, DB2 `DCLTRCAT` | `TRANCATG.VSAM.KSDS`, DB2 `TRANSACTION_TYPE_CATEGORY` | `transaction_category` |
| DB2 `AUTHFRDS.dcl` | DB2 `AUTHFRDS` | `authfrds` |
| `CIPAUSMY` | IMS `DBPAUTP0`/`PAUTSUM0` | `pending_auth_summary` (replatform candidate) |
| `CIPAUDTY` | IMS `DBPAUTP0`/`PAUTDTL1` | `pending_auth_detail` (replatform candidate) |
| `CVEXPORT` | `EXPORT.DATA` (500-byte multi-record) | S3 only (`batch.md`) |
| `CVTRA07Y` | `TRANREPT` GDG (133-byte report) | S3 only |

## 5. Data load rules (for data-migration session)

* EBCDIC files in `app/data/EBCDIC/` are fixed-length, code page **CP037**; all numeric fields in the core
  copybooks are zoned decimal (`DISPLAY`) with the sign in the last byte's zone nibble (no `COMP-3` in core
  VSAM records). The IMS unload `AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat` contains `COMP`/`COMP-3` fields.
* ASCII files in `app/data/ASCII/` are line-delimited, same field widths; signed amounts use the
  overpunch convention (`{`, `A`–`I`, `}`, `J`–`R`) in the last position. Terminators are LF or CRLF
  (`tcatbal.txt`, `trancatg.txt`, `trantype.txt` contain CRLF) — strip a trailing CR. Lines shorter than the
  copybook length are right-padded with spaces before parsing (`cardxref.txt` rows are 36 chars: the 14-byte
  `CVACT03Y` filler is omitted); lines longer than the copybook length after CR stripping are rejected.
* Trim trailing spaces for `VARCHAR`; spaces/zeros in date fields → `NULL`.
* Reconciliation: row counts per table must equal the record counts in `migration-inventory.md` §6.2.
