# Copybook-to-schema mapping

Field-by-field mapping of every CardDemo record layout to the PostgreSQL column that holds it,
following [`conventions.md`](conventions.md). Every elementary item of each copybook appears in one
row: either with its column or, for `FILLER`, as **dropped** (padding; recreated as spaces when a
fixed-width record is written back). Rows marked *(new)* are columns with no copybook field
(optimistic-lock `version`, surrogate keys, batch markers).

This file is checked by tests, so keep the table format:

- `common` `CopybookCoverageTest` parses each copybook under `app/cpy` and fails if an elementary
  item is missing from its section or its picture differs.
- each service's `SchemaMappingTest` runs its Flyway migrations on PostgreSQL 16 (Testcontainers)
  and fails if a documented column is missing, has another type or length, or if the schema has a
  column this file does not document.

Section headings name the copybook in backticks and the target `schema.table`; table rows are
`| Field | Picture | Column | Type | Notes |`.

`modernization/docs/COPYBOOK-TO-SCHEMA-MAPPING.md` (the five-service branch's original note) is
superseded by this file.

## USRSEC — `CSUSR01Y` → `auth.users`

VSAM KSDS `AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS`, RECORDSIZE 80, key `KEYS(8,0)` (`DUSRSECJ.jcl`).
Accessed by COSGN00C (READ by key), COUSR00C (STARTBR/READNEXT/READPREV), COUSR01C-03C
(WRITE/READ UPDATE/REWRITE/DELETE).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `SEC-USR-ID` | `X(08)` | `user_id` | `VARCHAR(8)` | primary key |
| `SEC-USR-FNAME` | `X(20)` | `first_name` | `VARCHAR(20)` | |
| `SEC-USR-LNAME` | `X(20)` | `last_name` | `VARCHAR(20)` | |
| `SEC-USR-PWD` | `X(08)` | `password_hash` | `VARCHAR(100)` | BCrypt hash of the 8-character password; plaintext is not stored (conventions §1) |
| `SEC-USR-TYPE` | `X(01)` | `user_type` | `VARCHAR(1)` | `A`/`U` check |
| `SEC-USR-FILLER` | `X(23)` | — | dropped | FILLER |
| *(new)* | — | `version` | `BIGINT` | `@Version` for READ UPDATE/REWRITE |

Browse: `UserRepository.findByUserIdGreaterThanEqualOrderByUserIdAsc` (STARTBR + READNEXT) and
`findByUserIdLessThanOrderByUserIdDesc` (READPREV).

## CUSTDATA — `CVCUS01Y` → `customer.customers`

VSAM KSDS `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS`, RECLN 500, key `KEYS(9 0)` (`CUSTFILE.jcl`).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `CUST-ID` | `9(09)` | `cust_id` | `BIGINT` | primary key; range check 0..999999999 |
| `CUST-FIRST-NAME` | `X(25)` | `first_name` | `VARCHAR(25)` | |
| `CUST-MIDDLE-NAME` | `X(25)` | `middle_name` | `VARCHAR(25)` | |
| `CUST-LAST-NAME` | `X(25)` | `last_name` | `VARCHAR(25)` | |
| `CUST-ADDR-LINE-1` | `X(50)` | `addr_line_1` | `VARCHAR(50)` | |
| `CUST-ADDR-LINE-2` | `X(50)` | `addr_line_2` | `VARCHAR(50)` | |
| `CUST-ADDR-LINE-3` | `X(50)` | `addr_line_3` | `VARCHAR(50)` | |
| `CUST-ADDR-STATE-CD` | `X(02)` | `addr_state_cd` | `VARCHAR(2)` | |
| `CUST-ADDR-COUNTRY-CD` | `X(03)` | `addr_country_cd` | `VARCHAR(3)` | |
| `CUST-ADDR-ZIP` | `X(10)` | `addr_zip` | `VARCHAR(10)` | |
| `CUST-PHONE-NUM-1` | `X(15)` | `phone_num_1` | `VARCHAR(15)` | |
| `CUST-PHONE-NUM-2` | `X(15)` | `phone_num_2` | `VARCHAR(15)` | |
| `CUST-SSN` | `9(09)` | `ssn` | `BIGINT` | range check 0..999999999 |
| `CUST-GOVT-ISSUED-ID` | `X(20)` | `govt_issued_id` | `VARCHAR(20)` | |
| `CUST-DOB-YYYY-MM-DD` | `X(10)` | `date_of_birth` | `DATE` | COACTUPC edits it as a date |
| `CUST-EFT-ACCOUNT-ID` | `X(10)` | `eft_account_id` | `VARCHAR(10)` | |
| `CUST-PRI-CARD-HOLDER-IND` | `X(01)` | `pri_card_holder_ind` | `VARCHAR(1)` | |
| `CUST-FICO-CREDIT-SCORE` | `9(03)` | `fico_credit_score` | `INTEGER` | check 0..999 (the picture). 300..850 is COACTUPC's update edit and stays in the API; 21 of the 50 sample customers score below 300 (`V3__fico_picture_range.sql`) |
| `FILLER` | `X(168)` | — | dropped | FILLER |
| *(new)* | — | `version` | `BIGINT` | `@Version` |

## ACCTDATA — `CVACT01Y` → `account.accounts`

VSAM KSDS `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS`, RECLN 300, key `KEYS(11 0)` (`ACCTFILE.jcl`).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `ACCT-ID` | `9(11)` | `acct_id` | `BIGINT` | primary key; range check 0..99999999999 |
| `ACCT-ACTIVE-STATUS` | `X(01)` | `active_status` | `VARCHAR(1)` | `Y`/`N` check |
| `ACCT-CURR-BAL` | `S9(10)V99` | `curr_bal` | `NUMERIC(12,2)` | |
| `ACCT-CREDIT-LIMIT` | `S9(10)V99` | `credit_limit` | `NUMERIC(12,2)` | |
| `ACCT-CASH-CREDIT-LIMIT` | `S9(10)V99` | `cash_credit_limit` | `NUMERIC(12,2)` | |
| `ACCT-OPEN-DATE` | `X(10)` | `open_date` | `DATE` | COACTUPC edits it as a date |
| `ACCT-EXPIRAION-DATE` | `X(10)` | `expiration_date` | `DATE` | CBTRN02C compares it with the transaction date |
| `ACCT-REISSUE-DATE` | `X(10)` | `reissue_date` | `DATE` | COACTUPC edits it as a date |
| `ACCT-CURR-CYC-CREDIT` | `S9(10)V99` | `curr_cyc_credit` | `NUMERIC(12,2)` | |
| `ACCT-CURR-CYC-DEBIT` | `S9(10)V99` | `curr_cyc_debit` | `NUMERIC(12,2)` | |
| `ACCT-ADDR-ZIP` | `X(10)` | `addr_zip` | `VARCHAR(10)` | |
| `ACCT-GROUP-ID` | `X(10)` | `group_id` | `VARCHAR(10)` | index `ix_accounts_group_id` |
| `FILLER` | `X(178)` | — | dropped | FILLER |
| *(new)* | — | `version` | `BIGINT` | `@Version` |

Data note: in `app/data/ASCII/acctdata.txt` the `ACCT-ADDR-ZIP` positions hold `A000000000` and
`ACCT-GROUP-ID` is blank for all 50 records. Loaded as-is; CBACT04C falls back to the `DEFAULT`
disclosure group when the group lookup misses.

## Posting idempotency ledger (no copybook) → `account.account_postings`

No COBOL counterpart: CBTRN02C updates ACCTFILE and TRANFILE in one program, the Java port spans
account-service and transaction-service. One row per posting applied by `POST
/api/v1/accounts/{id}/postings`, written in the same database transaction as the account update
(`V2__posting_idempotency.sql`); a retry with the same transaction id returns the recorded figures
and does not post again. Emptied with the batch outputs by `data-loader --replace`.

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| *(new)* | — | `tran_id` | `VARCHAR(16)` | primary key; `DALYTRAN-ID` / `TRAN-ID` of the posting |
| *(new)* | — | `acct_id` | `BIGINT` | foreign key to `accounts` |
| *(new)* | — | `amount` | `NUMERIC(11,2)` | `DALYTRAN-AMT` |
| *(new)* | — | `transaction_date` | `DATE` | date part of `DALYTRAN-ORIG-TS` |
| *(new)* | — | `curr_bal` | `NUMERIC(12,2)` | `ACCT-CURR-BAL` after the posting |
| *(new)* | — | `curr_cyc_credit` | `NUMERIC(12,2)` | `ACCT-CURR-CYC-CREDIT` after the posting |
| *(new)* | — | `curr_cyc_debit` | `NUMERIC(12,2)` | `ACCT-CURR-CYC-DEBIT` after the posting |
| *(new)* | — | `posted_at` | `TIMESTAMP` | time the posting was applied |

## CARDDATA — `CVACT02Y` → `card.cards`

VSAM KSDS `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS`, RECLN 150, key `KEYS(16 0)`; AIX `CARDAIX` on
`CARD-ACCT-ID`, `KEYS(11 16)`, non-unique (`CARDFILE.jcl`).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `CARD-NUM` | `X(16)` | `card_num` | `VARCHAR(16)` | primary key |
| `CARD-ACCT-ID` | `9(11)` | `acct_id` | `BIGINT` | index `ix_cards_acct_id` (CARDAIX) |
| `CARD-CVV-CD` | `9(03)` | `cvv_cd` | `INTEGER` | check 0..999 |
| `CARD-EMBOSSED-NAME` | `X(50)` | `embossed_name` | `VARCHAR(50)` | |
| `CARD-EXPIRAION-DATE` | `X(10)` | `expiration_date` | `DATE` | COCRDUPC edits the month/year |
| `CARD-ACTIVE-STATUS` | `X(01)` | `active_status` | `VARCHAR(1)` | `Y`/`N` check |
| `FILLER` | `X(59)` | — | dropped | FILLER |
| *(new)* | — | `version` | `BIGINT` | `@Version` |

Access: `CardRepository.findByAcctId` (CARDAIX), `findByCardNumGreaterThanEqualOrderByCardNumAsc`
(COCRDLIC STARTBR GTEQ + READNEXT), `findByCardNumLessThanOrderByCardNumDesc` (READPREV).

## CARDXREF — `CVACT03Y` → `card.card_xref`

VSAM KSDS `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS`, RECLN 50, key `KEYS(16 0)`; AIX `CXACAIX` on
`XREF-ACCT-ID`, `KEYS(11,25)`, non-unique (`XREFFILE.jcl`). The ASCII sample file has 36-byte lines
(the trailing FILLER is trimmed).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `XREF-CARD-NUM` | `X(16)` | `card_num` | `VARCHAR(16)` | primary key; FK to `cards` |
| `XREF-CUST-ID` | `9(09)` | `cust_id` | `BIGINT` | index `ix_card_xref_cust_id` |
| `XREF-ACCT-ID` | `9(11)` | `acct_id` | `BIGINT` | index `ix_card_xref_acct_id` (CXACAIX) |
| `FILLER` | `X(14)` | — | dropped | FILLER |

Access: `CardXrefRepository.findByAcctId` / `findFirstByAcctIdOrderByCardNum` (CXACAIX),
`findByCustId`, `findById` (primary key).

## TRANSACT — `CVTRA05Y` → `transaction.transactions`

VSAM KSDS `AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS`, RECLN 350, key `KEYS(16 0)`; AIX on
`TRAN-PROC-TS`, `KEYS(26 304)`, non-unique (`TRANFILE.jcl`, `TRANIDX.jcl`).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `TRAN-ID` | `X(16)` | `tran_id` | `VARCHAR(16)` | primary key |
| `TRAN-TYPE-CD` | `X(02)` | `type_cd` | `VARCHAR(2)` | |
| `TRAN-CAT-CD` | `9(04)` | `cat_cd` | `INTEGER` | check 0..9999 |
| `TRAN-SOURCE` | `X(10)` | `source` | `VARCHAR(10)` | |
| `TRAN-DESC` | `X(100)` | `description` | `VARCHAR(100)` | |
| `TRAN-AMT` | `S9(09)V99` | `amount` | `NUMERIC(11,2)` | |
| `TRAN-MERCHANT-ID` | `9(09)` | `merchant_id` | `BIGINT` | check 0..999999999 |
| `TRAN-MERCHANT-NAME` | `X(50)` | `merchant_name` | `VARCHAR(50)` | |
| `TRAN-MERCHANT-CITY` | `X(50)` | `merchant_city` | `VARCHAR(50)` | |
| `TRAN-MERCHANT-ZIP` | `X(10)` | `merchant_zip` | `VARCHAR(10)` | |
| `TRAN-CARD-NUM` | `X(16)` | `card_num` | `VARCHAR(16)` | index `ix_transactions_card_num` (card_num, orig_ts) |
| `TRAN-ORIG-TS` | `X(26)` | `orig_ts` | `TIMESTAMP` | |
| `TRAN-PROC-TS` | `X(26)` | `proc_ts` | `TIMESTAMP` | index `ix_transactions_proc_ts` (the TRANSACT AIX) |
| `FILLER` | `X(20)` | — | dropped | FILLER |

Access: `TransactionRepository.findByCardNum`,
`findByTranIdGreaterThanEqualOrderByTranIdAsc` / `findByTranIdLessThanOrderByTranIdDesc`
(COTRN00C browse), `findTopByOrderByTranIdDesc` (COBIL00C/COTRN02C STARTBR HIGH-VALUES +
READPREV for the next id), `findByProcTsGreaterThanEqualAndProcTsLessThanOrderByCardNumAscTranIdAsc`
(TRANREPT date range, sorted by card number).

## DALYTRAN — `CVTRA06Y` → `transaction.daily_transactions`

Sequential `AWS.M2.CARDDEMO.DALYTRAN.PS`, LRECL 350, read by CBTRN01C/CBTRN02C.

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `DALYTRAN-ID` | `X(16)` | `tran_id` | `VARCHAR(16)` | primary key (the file has no key; ids are unique in the sample) |
| `DALYTRAN-TYPE-CD` | `X(02)` | `type_cd` | `VARCHAR(2)` | |
| `DALYTRAN-CAT-CD` | `9(04)` | `cat_cd` | `INTEGER` | check 0..9999 |
| `DALYTRAN-SOURCE` | `X(10)` | `source` | `VARCHAR(10)` | |
| `DALYTRAN-DESC` | `X(100)` | `description` | `VARCHAR(100)` | |
| `DALYTRAN-AMT` | `S9(09)V99` | `amount` | `NUMERIC(11,2)` | |
| `DALYTRAN-MERCHANT-ID` | `9(09)` | `merchant_id` | `BIGINT` | check 0..999999999 |
| `DALYTRAN-MERCHANT-NAME` | `X(50)` | `merchant_name` | `VARCHAR(50)` | |
| `DALYTRAN-MERCHANT-CITY` | `X(50)` | `merchant_city` | `VARCHAR(50)` | |
| `DALYTRAN-MERCHANT-ZIP` | `X(10)` | `merchant_zip` | `VARCHAR(10)` | |
| `DALYTRAN-CARD-NUM` | `X(16)` | `card_num` | `VARCHAR(16)` | |
| `DALYTRAN-ORIG-TS` | `X(26)` | `orig_ts` | `TIMESTAMP` | |
| `DALYTRAN-PROC-TS` | `X(26)` | `proc_ts` | `TIMESTAMP` | |
| `FILLER` | `X(20)` | — | dropped | FILLER |
| *(new)* | — | `processed` | `BOOLEAN` | posting marker; replaces reading the sequential file once per run |

Access: `DailyTransactionRepository.findByProcessedFalseOrderByTranId` (sequential read order).

## DALYREJS — `CBTRN02C` `REJECT-RECORD` → `transaction.transaction_rejects`

Sequential `AWS.M2.CARDDEMO.DALYREJS(+1)`, LRECL 430, written by CBTRN02C 2500-WRITE-REJECT-REC.
The record is defined inline in CBTRN02C (no copybook): `REJECT-TRAN-DATA X(350)` is the moved
`DALYTRAN-RECORD` (CVTRA06Y) and `VALIDATION-TRAILER X(80)` is `WS-VALIDATION-TRAILER`. Fixed in
`V5__copybook_audit.sql`: the table held only id/card/reason, so the rest of the rejected
transaction was lost.

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `DALYTRAN-ID` | `X(16)` | `tran_id` | `VARCHAR(16)` | not unique: one transaction may be rejected in several runs |
| `DALYTRAN-TYPE-CD` | `X(02)` | `type_cd` | `VARCHAR(2)` | |
| `DALYTRAN-CAT-CD` | `9(04)` | `cat_cd` | `INTEGER` | |
| `DALYTRAN-SOURCE` | `X(10)` | `source` | `VARCHAR(10)` | |
| `DALYTRAN-DESC` | `X(100)` | `description` | `VARCHAR(100)` | |
| `DALYTRAN-AMT` | `S9(09)V99` | `amount` | `NUMERIC(11,2)` | |
| `DALYTRAN-MERCHANT-ID` | `9(09)` | `merchant_id` | `BIGINT` | |
| `DALYTRAN-MERCHANT-NAME` | `X(50)` | `merchant_name` | `VARCHAR(50)` | |
| `DALYTRAN-MERCHANT-CITY` | `X(50)` | `merchant_city` | `VARCHAR(50)` | |
| `DALYTRAN-MERCHANT-ZIP` | `X(10)` | `merchant_zip` | `VARCHAR(10)` | |
| `DALYTRAN-CARD-NUM` | `X(16)` | `card_num` | `VARCHAR(16)` | |
| `DALYTRAN-ORIG-TS` | `X(26)` | `orig_ts` | `TIMESTAMP` | |
| `DALYTRAN-PROC-TS` | `X(26)` | `proc_ts` | `TIMESTAMP` | |
| `FILLER` | `X(20)` | — | dropped | FILLER of the moved DALYTRAN-RECORD |
| `WS-VALIDATION-FAIL-REASON` | `9(04)` | `reason_code` | `INTEGER` | check 0..9999 |
| `WS-VALIDATION-FAIL-REASON-DESC` | `X(76)` | `reason_desc` | `VARCHAR(76)` | was `VARCHAR(80)` |
| *(new)* | — | `reject_id` | `BIGINT` | surrogate key (`BIGSERIAL`) |
| *(new)* | — | `rejected_at` | `TIMESTAMP` | time the reject was written |

## TRANTYPE — `CVTRA03Y` → `transaction.transaction_types`

VSAM KSDS `AWS.M2.CARDDEMO.TRANTYPE.VSAM.KSDS`, RECLN 60, key `KEYS(2 0)` (`TRANTYPE.jcl`).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `TRAN-TYPE` | `X(02)` | `type_cd` | `VARCHAR(2)` | primary key |
| `TRAN-TYPE-DESC` | `X(50)` | `type_desc` | `VARCHAR(50)` | |
| `FILLER` | `X(08)` | — | dropped | FILLER |

## TRANCATG — `CVTRA04Y` → `transaction.transaction_categories`

VSAM KSDS `AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS`, RECLN 60, key `KEYS(6 0)` = type + category
(`TRANCATG.jcl`).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `TRAN-TYPE-CD` | `X(02)` | `type_cd` | `VARCHAR(2)` | primary key part 1; FK to `transaction_types` |
| `TRAN-CAT-CD` | `9(04)` | `cat_cd` | `INTEGER` | primary key part 2; check 0..9999 |
| `TRAN-CAT-TYPE-DESC` | `X(50)` | `cat_type_desc` | `VARCHAR(50)` | |
| `FILLER` | `X(04)` | — | dropped | FILLER |

## TCATBALF — `CVTRA01Y` → `transaction.category_balances`

VSAM KSDS `AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS`, RECLN 50, key `KEYS(17 0)` = account + type +
category (`TCATBALF.jcl`).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `TRANCAT-ACCT-ID` | `9(11)` | `acct_id` | `BIGINT` | primary key part 1; range check |
| `TRANCAT-TYPE-CD` | `X(02)` | `type_cd` | `VARCHAR(2)` | primary key part 2 |
| `TRANCAT-CD` | `9(04)` | `cat_cd` | `INTEGER` | primary key part 3; check 0..9999 |
| `TRAN-CAT-BAL` | `S9(09)V99` | `balance` | `NUMERIC(11,2)` | |
| `FILLER` | `X(22)` | — | dropped | FILLER |

Access: `CategoryBalanceRepository` account-ordered browse (CBACT04C sequential read by key).

## DISCGRP — `CVTRA02Y` → `transaction.disclosure_groups`

VSAM KSDS `AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS`, RECLN 50, key `KEYS(16 0)` = group + type +
category (`DISCGRP.jcl`).

| Field | Picture | Column | Type | Notes |
|---|---|---|---|---|
| `DIS-ACCT-GROUP-ID` | `X(10)` | `acct_group_id` | `VARCHAR(10)` | primary key part 1 |
| `DIS-TRAN-TYPE-CD` | `X(02)` | `type_cd` | `VARCHAR(2)` | primary key part 2 |
| `DIS-TRAN-CAT-CD` | `9(04)` | `cat_cd` | `INTEGER` | primary key part 3; check 0..9999 |
| `DIS-INT-RATE` | `S9(04)V99` | `int_rate` | `NUMERIC(6,2)` | |
| `FILLER` | `X(28)` | — | dropped | FILLER |
