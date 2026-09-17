# Copybook to relational schema mapping

Source of truth: the copybooks in `app/cpy/` and the `SELECT` statements (VSAM keys and alternate
indexes) in the COBOL programs in `app/cbl/`. Target: Amazon Aurora PostgreSQL (PostgreSQL 15
compatible). Every table below is created by a Flyway migration inside the owning service.

## Type conversion rules

| COBOL picture | Meaning | PostgreSQL type | Notes |
|---|---|---|---|
| `PIC X(n)` | fixed alphanumeric | `VARCHAR(n)` | trailing blanks trimmed on load |
| `PIC X(n)` used as a key | fixed alphanumeric key | `CHAR(n)` | keeps the fixed-width key semantics of a KSDS |
| `PIC 9(n)` | unsigned zoned decimal | `NUMERIC(n,0)` | keeps leading zeros meaningful ids exact |
| `PIC S9(n)V99` | signed decimal, 2 implied decimals | `NUMERIC(n+2,2)` | money; never `float` |
| `PIC X(10)` date | `YYYY-MM-DD` | `DATE` | invalid/blank legacy values load as `NULL` |
| `PIC X(26)` timestamp | DB2 format `YYYY-MM-DD-HH.MM.SS.mmmmmm` | `TIMESTAMP(6)` | converted on load |
| `FILLER` | record padding to the VSAM record length | *(dropped)* | padding only, carries no data |

`FILLER` fields exist purely to pad records to the fixed VSAM record length (300 for accounts, 150
for cards, ...). They are dropped; the record length is not a property of the relational model.

## USRSEC - `CSUSR01Y` (RECLN 80) -> `auth.users`

VSAM KSDS, key `SEC-USR-ID`. Owned by **auth-service**.

| Field | Picture | Column | Type |
|---|---|---|---|
| `SEC-USR-ID` | `X(08)` | `user_id` | `CHAR(8)` PK |
| `SEC-USR-FNAME` | `X(20)` | `first_name` | `VARCHAR(20)` |
| `SEC-USR-LNAME` | `X(20)` | `last_name` | `VARCHAR(20)` |
| `SEC-USR-PWD` | `X(08)` | `password_hash` | `VARCHAR(100)` |
| `SEC-USR-TYPE` | `X(01)` | `user_type` | `CHAR(1)` (`A` admin, `U` user) |
| `SEC-USR-FILLER` | `X(23)` | - | dropped |

The plaintext 8 byte password of the legacy file becomes a BCrypt hash; the column is widened
accordingly. `SEC-USR-TYPE` is the seed of the modern RBAC model (`ROLE_ADMIN` / `ROLE_USER`).

## CUSTDATA - `CVCUS01Y` (RECLN 500) -> `customer.customers`

VSAM KSDS, key `CUST-ID`. Owned by **customer-service**.

| Field | Picture | Column | Type |
|---|---|---|---|
| `CUST-ID` | `9(09)` | `cust_id` | `NUMERIC(9,0)` PK |
| `CUST-FIRST-NAME` | `X(25)` | `first_name` | `VARCHAR(25)` |
| `CUST-MIDDLE-NAME` | `X(25)` | `middle_name` | `VARCHAR(25)` |
| `CUST-LAST-NAME` | `X(25)` | `last_name` | `VARCHAR(25)` |
| `CUST-ADDR-LINE-1..3` | `X(50)` | `addr_line_1..3` | `VARCHAR(50)` |
| `CUST-ADDR-STATE-CD` | `X(02)` | `addr_state_cd` | `CHAR(2)` |
| `CUST-ADDR-COUNTRY-CD` | `X(03)` | `addr_country_cd` | `CHAR(3)` |
| `CUST-ADDR-ZIP` | `X(10)` | `addr_zip` | `VARCHAR(10)` |
| `CUST-PHONE-NUM-1/2` | `X(15)` | `phone_num_1/2` | `VARCHAR(15)` |
| `CUST-SSN` | `9(09)` | `ssn` | `NUMERIC(9,0)` |
| `CUST-GOVT-ISSUED-ID` | `X(20)` | `govt_issued_id` | `VARCHAR(20)` |
| `CUST-DOB-YYYY-MM-DD` | `X(10)` | `date_of_birth` | `DATE` |
| `CUST-EFT-ACCOUNT-ID` | `X(10)` | `eft_account_id` | `VARCHAR(10)` |
| `CUST-PRI-CARD-HOLDER-IND` | `X(01)` | `pri_card_holder_ind` | `CHAR(1)` |
| `CUST-FICO-CREDIT-SCORE` | `9(03)` | `fico_credit_score` | `SMALLINT` (300..850 checked) |

## ACCTDATA - `CVACT01Y` (RECLN 300) -> `account.accounts`

VSAM KSDS, key `ACCT-ID`. Owned by **account-service**.

| Field | Picture | Column | Type |
|---|---|---|---|
| `ACCT-ID` | `9(11)` | `acct_id` | `NUMERIC(11,0)` PK |
| `ACCT-ACTIVE-STATUS` | `X(01)` | `active_status` | `CHAR(1)` (`Y`/`N`) |
| `ACCT-CURR-BAL` | `S9(10)V99` | `curr_bal` | `NUMERIC(12,2)` |
| `ACCT-CREDIT-LIMIT` | `S9(10)V99` | `credit_limit` | `NUMERIC(12,2)` |
| `ACCT-CASH-CREDIT-LIMIT` | `S9(10)V99` | `cash_credit_limit` | `NUMERIC(12,2)` |
| `ACCT-OPEN-DATE` | `X(10)` | `open_date` | `DATE` |
| `ACCT-EXPIRAION-DATE` | `X(10)` | `expiration_date` | `DATE` (copybook typo kept out of the schema) |
| `ACCT-REISSUE-DATE` | `X(10)` | `reissue_date` | `DATE` |
| `ACCT-CURR-CYC-CREDIT` | `S9(10)V99` | `curr_cyc_credit` | `NUMERIC(12,2)` |
| `ACCT-CURR-CYC-DEBIT` | `S9(10)V99` | `curr_cyc_debit` | `NUMERIC(12,2)` |
| `ACCT-ADDR-ZIP` | `X(10)` | `addr_zip` | `VARCHAR(10)` |
| `ACCT-GROUP-ID` | `X(10)` | `group_id` | `VARCHAR(10)` - joins `disclosure_groups` |

A `version` column is added for optimistic locking; it replaces the record level lock CICS takes on
`READ ... UPDATE`.

## CARDDATA - `CVACT02Y` (RECLN 150) -> `card.cards`

VSAM KSDS, key `CARD-NUM`, alternate index on `CARD-ACCT-ID` (used by `COCRDLIC`).
Owned by **card-service**.

| Field | Picture | Column | Type |
|---|---|---|---|
| `CARD-NUM` | `X(16)` | `card_num` | `CHAR(16)` PK |
| `CARD-ACCT-ID` | `9(11)` | `acct_id` | `NUMERIC(11,0)`, indexed (AIX `CARDAIX`) |
| `CARD-CVV-CD` | `9(03)` | `cvv_cd` | `SMALLINT` |
| `CARD-EMBOSSED-NAME` | `X(50)` | `embossed_name` | `VARCHAR(50)` |
| `CARD-EXPIRAION-DATE` | `X(10)` | `expiration_date` | `DATE` |
| `CARD-ACTIVE-STATUS` | `X(01)` | `active_status` | `CHAR(1)` |

## CARDXREF - `CVACT03Y` (RECLN 50) -> `card.card_xref`

VSAM KSDS, key `XREF-CARD-NUM`, alternate index on `XREF-ACCT-ID` (used by `CBACT04C`).
Owned by **card-service**; it is the join between the three aggregates.

| Field | Picture | Column | Type |
|---|---|---|---|
| `XREF-CARD-NUM` | `X(16)` | `card_num` | `CHAR(16)` PK, FK -> `cards` |
| `XREF-CUST-ID` | `9(09)` | `cust_id` | `NUMERIC(9,0)`, indexed |
| `XREF-ACCT-ID` | `9(11)` | `acct_id` | `NUMERIC(11,0)`, indexed (AIX `XREFAIX`) |

The cross reference is kept as its own table rather than denormalised into `cards`: the mainframe
uses it as the entry point for card -> account -> customer resolution in both online (`COACTVWC`)
and batch (`CBTRN02C`, `CBACT04C`) paths, and keeping it explicit preserves that access path.

## TRANSACT - `CVTRA05Y` (RECLN 350) -> `transaction.transactions`

VSAM KSDS, key `TRAN-ID`. Owned by **transaction-service**.

| Field | Picture | Column | Type |
|---|---|---|---|
| `TRAN-ID` | `X(16)` | `tran_id` | `CHAR(16)` PK |
| `TRAN-TYPE-CD` | `X(02)` | `type_cd` | `CHAR(2)` FK -> `transaction_types` |
| `TRAN-CAT-CD` | `9(04)` | `cat_cd` | `SMALLINT`, (`type_cd`,`cat_cd`) FK -> `transaction_categories` |
| `TRAN-SOURCE` | `X(10)` | `source` | `VARCHAR(10)` |
| `TRAN-DESC` | `X(100)` | `description` | `VARCHAR(100)` |
| `TRAN-AMT` | `S9(09)V99` | `amount` | `NUMERIC(11,2)` |
| `TRAN-MERCHANT-ID` | `9(09)` | `merchant_id` | `NUMERIC(9,0)` |
| `TRAN-MERCHANT-NAME` | `X(50)` | `merchant_name` | `VARCHAR(50)` |
| `TRAN-MERCHANT-CITY` | `X(50)` | `merchant_city` | `VARCHAR(50)` |
| `TRAN-MERCHANT-ZIP` | `X(10)` | `merchant_zip` | `VARCHAR(10)` |
| `TRAN-CARD-NUM` | `X(16)` | `card_num` | `CHAR(16)`, indexed |
| `TRAN-ORIG-TS` | `X(26)` | `orig_ts` | `TIMESTAMP(6)` |
| `TRAN-PROC-TS` | `X(26)` | `proc_ts` | `TIMESTAMP(6)` |

## DALYTRAN - `CVTRA06Y` (RECLN 350) -> `transaction.daily_transactions`

Sequential input file of `CBTRN02C`. Same layout as `CVTRA05Y` with the `DALYTRAN-` prefix, plus a
`posting_status` column (`PENDING` / `POSTED` / `REJECTED`) that replaces "the file has been read"
as the progress marker of the batch run.

Rejected records (`DALYREJS`, the reject file written by `CBTRN02C` with
`WS-VALIDATION-FAIL-REASON` + description) become `transaction.transaction_rejects`:
`tran_id`, `card_num`, `amount`, `reason_code`, `reason_desc`, `rejected_at`.

## TRANTYPE - `CVTRA03Y` (RECLN 60) -> `transaction.transaction_types`

| Field | Picture | Column | Type |
|---|---|---|---|
| `TRAN-TYPE` | `X(02)` | `type_cd` | `CHAR(2)` PK |
| `TRAN-TYPE-DESC` | `X(50)` | `description` | `VARCHAR(50)` |

## TRANCATG - `CVTRA04Y` (RECLN 60) -> `transaction.transaction_categories`

Composite VSAM key `TRAN-CAT-KEY` = `TRAN-TYPE-CD` + `TRAN-CAT-CD` -> composite primary key.

| Field | Picture | Column | Type |
|---|---|---|---|
| `TRAN-TYPE-CD` | `X(02)` | `type_cd` | `CHAR(2)` PK, FK -> `transaction_types` |
| `TRAN-CAT-CD` | `9(04)` | `cat_cd` | `SMALLINT` PK |
| `TRAN-CAT-TYPE-DESC` | `X(50)` | `description` | `VARCHAR(50)` |

## DISCGRP - `CVTRA02Y` (RECLN 50) -> `transaction.disclosure_groups`

Composite VSAM key `DIS-GROUP-KEY` = account group + transaction type + transaction category.
`CBACT04C` falls back to the literal group id `DEFAULT` when the lookup misses (file status `23`),
so that row is part of the seed data.

| Field | Picture | Column | Type |
|---|---|---|---|
| `DIS-ACCT-GROUP-ID` | `X(10)` | `acct_group_id` | `VARCHAR(10)` PK |
| `DIS-TRAN-TYPE-CD` | `X(02)` | `type_cd` | `CHAR(2)` PK |
| `DIS-TRAN-CAT-CD` | `9(04)` | `cat_cd` | `SMALLINT` PK |
| `DIS-INT-RATE` | `S9(04)V99` | `int_rate` | `NUMERIC(6,2)` - annual percentage |

## TCATBALF - `CVTRA01Y` (RECLN 50) -> `transaction.transaction_category_balances`

Composite VSAM key `TRAN-CAT-KEY` = account + transaction type + transaction category.

| Field | Picture | Column | Type |
|---|---|---|---|
| `TRANCAT-ACCT-ID` | `9(11)` | `acct_id` | `NUMERIC(11,0)` PK |
| `TRANCAT-TYPE-CD` | `X(02)` | `type_cd` | `CHAR(2)` PK |
| `TRANCAT-CD` | `9(04)` | `cat_cd` | `SMALLINT` PK |
| `TRAN-CAT-BAL` | `S9(09)V99` | `balance` | `NUMERIC(11,2)` |

`CVTRA07Y` has no table: it is the print layout of the `CBTRN03C` report (headers, page and grand
totals, `PIC -ZZZ,ZZZ,ZZZ.ZZ` edited fields). It maps to the report DTO / rendering layer, not to
storage.

## Schema ownership and cross-service references

Each service owns its own PostgreSQL schema and no service reads another service's tables:

| Schema | Service | Tables |
|---|---|---|
| `auth` | auth-service | `users` |
| `customer` | customer-service | `customers` |
| `account` | account-service | `accounts` |
| `card` | card-service | `cards`, `card_xref` |
| `transaction` | transaction-service | `transactions`, `daily_transactions`, `transaction_rejects`, `transaction_types`, `transaction_categories`, `disclosure_groups`, `transaction_category_balances` |

Relationships that cross a service boundary (card -> account, xref -> customer, transaction ->
account) are **not** database foreign keys; they are ids resolved over REST. Inside a schema
(cards -> card_xref, transactions -> types/categories) they are real foreign keys.
