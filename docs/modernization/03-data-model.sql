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
