# 3. Relational data model derived from the copybooks

Two schemas are given. **§3.1** is the 1:1 copybook-faithful mapping — one table per record
layout, one column per elementary item, same widths, same key structure — which the Java
implementation and the transition-period file loaders are aligned to. **§3.2** lists, per table,
where a normalized model would differ and why each of those is a customer decision. The DDL is
ANSI/PostgreSQL-flavoured; `NUMERIC(p,s)` stands for the exact COBOL precision.

## 3.0 Mapping rules applied (and what the copybooks do and do not contain)

| COBOL construct | Occurs in these copybooks? | Mapping |
|---|---|---|
| `PIC X(n)` | all | `CHAR(n)` (fixed width preserved; trailing spaces significant on the mainframe). |
| `PIC 9(n)` unsigned zoned | all | `NUMERIC(n,0)` with `CHECK (col >= 0)`. Where the field is an identifier (`ACCT-ID`, `CUST-ID`, `MERCHANT-ID`, `CAT-CD`) it is *also* stored as text-of-digits in the file; we keep `NUMERIC` because the programs compare them numerically (`TRANCAT-ACCT-ID NOT= WS-LAST-ACCT-NUM` compares 9(11) to X(11) — mixed, see OQ-20). |
| `PIC S9(n)V99` signed zoned, trailing overpunch sign | amounts, limits, rates | `NUMERIC(n+2,2)`. |
| `COMP` / `COMP-3` (binary / packed decimal) | **None in the eight record copybooks.** `COMP-3` appears only in CBTRN03C working storage (`WS-LINE-COUNTER`, `WS-PAGE-SIZE`, lines 129-131) and `COMP` in the CBACT04C `PARM-LENGTH` (line 177); neither is persisted. | n/a — noted so the reader does not go looking. |
| `REDEFINES` | **None** in the record copybooks. The programs' FD areas (`FD-TRANS-DATA`, `FD-ACCT-DATA`, …) are separate 01 levels over the same bytes, i.e. an implicit redefinition of key + filler over the full record. | Modelled by the key constraints; no extra columns. |
| `OCCURS` | **None** in this flow's copybooks (CVACT02Y has none either; the CardDemo copybooks with OCCURS are the screen maps, out of scope). | n/a. |
| `FILLER` | every record | Not a column. Column `filler_image BYTEA` is **optional** and only in §3.1 "transition" tables so that unread bytes survive a rewrite (CBTRN02C rewrites ACCTFILE/TCATBALF records whose filler the program never touches; CVACT01Y filler is 178 of 300 bytes). |
| Group items (`TRAN-CAT-KEY`, `DIS-GROUP-KEY`, `TRAN-CAT-KEY` in CVTRA04Y) | CVTRA01Y, CVTRA02Y, CVTRA04Y | Composite primary keys, as VSAM defines them. |
| Implied hierarchy | XREF: card → customer → account; TCATBAL: account → (type,category) → balance; DISCGRP: group → (type,category) → rate | Foreign keys in §3.1 are **commented out** because the VSAM files do not enforce them and the sample data is not guaranteed to satisfy them (the shipped ACCTDATA has `ACCT-GROUP-ID` = spaces for every account; no DISCGRP row has a blank group). Enabling them is a §3.2 decision. |
| Dates/timestamps as `X(10)` / `X(26)` | ACCOUNT dates, TRAN timestamps | Kept `CHAR`. The programs compare them as strings (CBTRN02C:414, CBTRN03C:173-174) and the daily feed carries blank `PROC-TS`; converting to `DATE`/`TIMESTAMP` would reject blanks and change comparison semantics for malformed values. §3.2. |
| Nullability | — | Every column is `NOT NULL`: a fixed-width record always has bytes in every position. "Empty" is spaces or zeros, never NULL. |

## 3.1 Copybook-faithful DDL

```sql
-- CVTRA05Y / CVTRA06Y : TRAN-RECORD and DALYTRAN-RECORD share one layout (350 bytes).
-- VSAM: AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS KEYS(16 0), AIX KEYS(26 304) nonunique on TRAN-PROC-TS.
-- The same table shape serves DALYTRAN.PS (input feed), SYSTRAN, TRANSACT.BKUP and TRANSACT.DALY.
CREATE TABLE transaction_master (
    tran_id            CHAR(16)       NOT NULL,   -- TRAN-ID            X(16)   @0
    tran_type_cd       CHAR(2)        NOT NULL,   -- TRAN-TYPE-CD       X(02)   @16
    tran_cat_cd        NUMERIC(4,0)   NOT NULL,   -- TRAN-CAT-CD        9(04)   @18
    tran_source        CHAR(10)       NOT NULL,   -- TRAN-SOURCE        X(10)   @22
    tran_desc          CHAR(100)      NOT NULL,   -- TRAN-DESC          X(100)  @32
    tran_amt           NUMERIC(11,2)  NOT NULL,   -- TRAN-AMT           S9(09)V99 @132
    tran_merchant_id   NUMERIC(9,0)   NOT NULL,   -- TRAN-MERCHANT-ID   9(09)   @143
    tran_merchant_name CHAR(50)       NOT NULL,   -- TRAN-MERCHANT-NAME X(50)   @152
    tran_merchant_city CHAR(50)       NOT NULL,   -- TRAN-MERCHANT-CITY X(50)   @202
    tran_merchant_zip  CHAR(10)       NOT NULL,   -- TRAN-MERCHANT-ZIP  X(10)   @252
    tran_card_num      CHAR(16)       NOT NULL,   -- TRAN-CARD-NUM      X(16)   @262
    tran_orig_ts       CHAR(26)       NOT NULL,   -- TRAN-ORIG-TS       X(26)   @278
    tran_proc_ts       CHAR(26)       NOT NULL,   -- TRAN-PROC-TS       X(26)   @304
    -- FILLER X(20) @330 : not mapped
    CONSTRAINT pk_transaction_master PRIMARY KEY (tran_id),
    CONSTRAINT ck_tran_cat_cd CHECK (tran_cat_cd >= 0),
    CONSTRAINT ck_tran_merchant_id CHECK (tran_merchant_id >= 0)
);
CREATE INDEX aix_transaction_master_proc_ts ON transaction_master (tran_proc_ts);  -- TRANSACT.VSAM.AIX

-- Daily feed is a sequential file, not a KSDS: no PK is enforced by VSAM. Same columns, no PK,
-- plus a load sequence so duplicates in the feed can be represented.
CREATE TABLE daily_transaction (
    load_seq           BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    LIKE transaction_master INCLUDING DEFAULTS EXCLUDING CONSTRAINTS EXCLUDING INDEXES
);

-- CBTRN02C REJECT-RECORD (lines 128-139), 430 bytes, sequential GDG.
CREATE TABLE daily_transaction_reject (
    reject_seq          BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    LIKE transaction_master INCLUDING DEFAULTS EXCLUDING CONSTRAINTS EXCLUDING INDEXES,
    validation_fail_reason      NUMERIC(4,0) NOT NULL,   -- WS-VALIDATION-FAIL-REASON      9(04) @350
    validation_fail_reason_desc CHAR(76)     NOT NULL    -- WS-VALIDATION-FAIL-REASON-DESC X(76) @354
);

-- CVACT01Y : ACCOUNT-RECORD (300). VSAM ACCTDATA KEYS(11 0).
CREATE TABLE account (
    acct_id                NUMERIC(11,0) NOT NULL,  -- ACCT-ID               9(11)     @0
    acct_active_status     CHAR(1)       NOT NULL,  -- ACCT-ACTIVE-STATUS    X(01)     @11
    acct_curr_bal          NUMERIC(12,2) NOT NULL,  -- ACCT-CURR-BAL         S9(10)V99 @12
    acct_credit_limit      NUMERIC(12,2) NOT NULL,  -- ACCT-CREDIT-LIMIT     S9(10)V99 @24
    acct_cash_credit_limit NUMERIC(12,2) NOT NULL,  -- ACCT-CASH-CREDIT-LIMIT S9(10)V99 @36
    acct_open_date         CHAR(10)      NOT NULL,  -- ACCT-OPEN-DATE        X(10)     @48
    acct_expiration_date   CHAR(10)      NOT NULL,  -- ACCT-EXPIRAION-DATE   X(10)     @58 (source typo kept in docs, fixed in column name)
    acct_reissue_date      CHAR(10)      NOT NULL,  -- ACCT-REISSUE-DATE     X(10)     @68
    acct_curr_cyc_credit   NUMERIC(12,2) NOT NULL,  -- ACCT-CURR-CYC-CREDIT  S9(10)V99 @78
    acct_curr_cyc_debit    NUMERIC(12,2) NOT NULL,  -- ACCT-CURR-CYC-DEBIT   S9(10)V99 @90
    acct_addr_zip          CHAR(10)      NOT NULL,  -- ACCT-ADDR-ZIP         X(10)     @102
    acct_group_id          CHAR(10)      NOT NULL,  -- ACCT-GROUP-ID         X(10)     @112
    filler_image           BYTEA         NULL,      -- FILLER X(178) @122 : transition only (see 3.0)
    CONSTRAINT pk_account PRIMARY KEY (acct_id),
    CONSTRAINT ck_acct_id CHECK (acct_id >= 0)
);

-- CVACT03Y : CARD-XREF-RECORD (50). VSAM CARDXREF KEYS(16 0); AIX KEYS(11 25) NONUNIQUEKEY.
CREATE TABLE card_xref (
    xref_card_num CHAR(16)      NOT NULL,   -- XREF-CARD-NUM X(16) @0
    xref_cust_id  NUMERIC(9,0)  NOT NULL,   -- XREF-CUST-ID  9(09) @16
    xref_acct_id  NUMERIC(11,0) NOT NULL,   -- XREF-ACCT-ID  9(11) @25
    -- FILLER X(14) @36
    CONSTRAINT pk_card_xref PRIMARY KEY (xref_card_num)
    -- , CONSTRAINT fk_card_xref_account FOREIGN KEY (xref_acct_id) REFERENCES account(acct_id)  -- §3.2 decision
);
-- AIX order = (alternate key, base key). CBACT04C's "first XREF for account" is the min card number.
CREATE INDEX aix_card_xref_acct_id ON card_xref (xref_acct_id, xref_card_num);

-- CVTRA01Y : TRAN-CAT-BAL-RECORD (50). VSAM TCATBALF KEYS(17 0) = acct(11)+type(2)+cat(4).
CREATE TABLE transaction_category_balance (
    trancat_acct_id NUMERIC(11,0) NOT NULL,  -- TRANCAT-ACCT-ID 9(11) @0
    trancat_type_cd CHAR(2)       NOT NULL,  -- TRANCAT-TYPE-CD X(02) @11
    trancat_cd      NUMERIC(4,0)  NOT NULL,  -- TRANCAT-CD      9(04) @13
    tran_cat_bal    NUMERIC(11,2) NOT NULL,  -- TRAN-CAT-BAL    S9(09)V99 @17
    filler_image    BYTEA         NULL,      -- FILLER X(22) @28 : transition only
    CONSTRAINT pk_transaction_category_balance PRIMARY KEY (trancat_acct_id, trancat_type_cd, trancat_cd)
    -- , FOREIGN KEY (trancat_acct_id) REFERENCES account(acct_id)                                   -- §3.2
    -- , FOREIGN KEY (trancat_type_cd, trancat_cd) REFERENCES transaction_category(tran_type_cd, tran_cat_cd) -- §3.2
);

-- CVTRA02Y : DIS-GROUP-RECORD (50). VSAM DISCGRP KEYS(16 0) = group(10)+type(2)+cat(4).
CREATE TABLE disclosure_group (
    dis_acct_group_id CHAR(10)     NOT NULL,  -- DIS-ACCT-GROUP-ID X(10) @0
    dis_tran_type_cd  CHAR(2)      NOT NULL,  -- DIS-TRAN-TYPE-CD  X(02) @10
    dis_tran_cat_cd   NUMERIC(4,0) NOT NULL,  -- DIS-TRAN-CAT-CD   9(04) @12
    dis_int_rate      NUMERIC(6,2) NOT NULL,  -- DIS-INT-RATE      S9(04)V99 @16  (annual percent)
    -- FILLER X(28) @22
    CONSTRAINT pk_disclosure_group PRIMARY KEY (dis_acct_group_id, dis_tran_type_cd, dis_tran_cat_cd)
);

-- CVTRA03Y : TRAN-TYPE-RECORD (60). VSAM TRANTYPE KEYS(2 0).
CREATE TABLE transaction_type (
    tran_type      CHAR(2)  NOT NULL,   -- TRAN-TYPE      X(02) @0
    tran_type_desc CHAR(50) NOT NULL,   -- TRAN-TYPE-DESC X(50) @2
    -- FILLER X(08) @52
    CONSTRAINT pk_transaction_type PRIMARY KEY (tran_type)
);

-- CVTRA04Y : TRAN-CAT-RECORD (60). VSAM TRANCATG KEYS(6 0) = type(2)+cat(4).
CREATE TABLE transaction_category (
    tran_type_cd       CHAR(2)      NOT NULL,   -- TRAN-TYPE-CD       X(02) @0
    tran_cat_cd        NUMERIC(4,0) NOT NULL,   -- TRAN-CAT-CD        9(04) @2
    tran_cat_type_desc CHAR(50)     NOT NULL,   -- TRAN-CAT-TYPE-DESC X(50) @6
    -- FILLER X(04) @56
    CONSTRAINT pk_transaction_category PRIMARY KEY (tran_type_cd, tran_cat_cd)
    -- , FOREIGN KEY (tran_type_cd) REFERENCES transaction_type(tran_type)   -- §3.2
);

-- CBTRN03C DATEPARM (80-byte sequential, one record read). Not a copybook; layout from lines 122-126.
CREATE TABLE report_date_parm (
    start_date CHAR(10) NOT NULL,   -- WS-START-DATE @0
    end_date   CHAR(10) NOT NULL    -- WS-END-DATE   @11 (byte 10 is a separator FILLER)
);
```

Complete field → column table (every elementary item in the eight copybooks is listed above as
a column comment with its PIC and byte offset; `FILLER` items are listed as comments and are
the only items without a column).

## 3.2 Where a normalized model would differ — customer decisions, not made here

| # | Copybook fact | Normalized alternative | Why it is a decision, not a translation |
|---|---|---|---|
| D-1 | `transaction_master` carries merchant name/city/zip on every row (110 bytes). | `merchant(merchant_id PK, name, city, zip)` and a FK from the transaction. | The COBOL never treats merchant as an entity; `MERCHANT-ID = 0` with blank text is a legitimate value (CBACT04C:487-490). Deduplicating changes what a "merchant" is and would reject or coalesce interest transactions. |
| D-2 | `card_xref` is a flat card→customer→account triple. | `card(card_num PK, acct_id FK)` plus `account(acct_id, cust_id FK)`, or a junction table if a card may map to several accounts. | The file allows the same card to appear once only (primary key) but an account to have many cards (AIX NONUNIQUEKEY). Whether `cust_id` belongs to the card or to the account cannot be decided from this flow: none of the three programs reads `XREF-CUST-ID`. |
| D-3 | `transaction_category_balance` is a running total keyed by (account, type, category). | Drop the table and compute `SUM(tran_amt) GROUP BY` from `transaction_master`. | Not equivalent: CBTRN02C opens TRANSACT as OUTPUT (master is per-run) while TCATBALF persists across runs; and CBACT04C's interest transactions are *not* added to TCATBALF. The balance file is a distinct ledger with its own history. |
| D-4 | `disclosure_group` keyed by (group, type, category) with a literal `DEFAULT` group. | `account.acct_group_id` FK → `disclosure_group_header(group_id)`, and DEFAULT modelled as a nullable FK or a rule table. | The fallback-to-DEFAULT rule lives in program logic (CBACT04C:436-441). Moving it into the schema (e.g. `COALESCE`) changes the failure mode: today a missing DEFAULT row abends; with a NULL FK it would silently compute zero. |
| D-5 | Dates and timestamps are `CHAR(10)` / `CHAR(26)`. | `DATE` / `TIMESTAMP(6)`. | The daily feed has 26 spaces in `PROC-TS`; interest transactions write `mmm000` microseconds; the programs compare as strings. A typed column would refuse blank timestamps (or need a sentinel) and would reorder any malformed value differently from DFSORT's `CH` compare. |
| D-6 | `acct_id`, `cust_id`, `merchant_id`, `cat_cd` are `PIC 9` but function as identifiers. | `CHAR(n)` (preserve leading zeros textually) or `BIGINT`. | CBACT04C compares `TRANCAT-ACCT-ID 9(11)` to `WS-LAST-ACCT-NUM X(11)` (line 194): numeric vs alphanumeric comparison rules apply; a schema change here alters grouping behaviour for any non-canonical value. OQ-20. |
| D-7 | `ACCT-CURR-CYC-DEBIT` accumulates negative amounts (CBTRN02C:551). | A `debit` column that is positive with the sign carried by a `direction` column. | Changing the sign convention changes the over-limit formula (`CREDIT - DEBIT`), i.e. a business rule. OQ-04. |
| D-8 | No foreign keys are enforced anywhere. | Enable the commented FKs. | Shipped sample data violates D-4 (all `ACCT-GROUP-ID` blank). Enforcement would fail the load; the programs' behaviour on dangling references (reject 100/101, or abend) is part of the documented contract. |
| D-9 | Reject file = original 350 bytes + reason. | `reject(tran_id FK, reason_code, reason_desc, rejected_at)`. | The feed has no enforced unique key; two rejected rows may share `tran_id`. The verbatim copy is also the only place where a *malformed* input record is preserved byte-for-byte. |
| D-10 | `FILLER` bytes. | Drop. | Only safe once no COBOL program rewrites these records. The Java keeps the raw image through rewrites precisely so that dropping filler can be deferred. |

## 3.3 Volumes from the shipped sample (for sizing only)

DALYTRAN 300 rows; ACCTDATA 50; CARDXREF 50; TCATBALF 50; DISCGRP 51; TRANTYPE 7; TRANCATG 18.
