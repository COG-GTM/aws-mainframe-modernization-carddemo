-- CardDemo core VSAM files as relational tables. One table per KSDS/PS dataset; column comments name the
-- copybook item. FILLER areas are not persisted. VSAM has no referential integrity, so foreign keys between
-- master files are deliberately not enforced here (the sample data is not guaranteed to be consistent).

-- CSUSR01Y - USRSEC (RECLN 80)
CREATE TABLE user_security (
    user_id     VARCHAR(8)  NOT NULL PRIMARY KEY,   -- SEC-USR-ID
    first_name  VARCHAR(20),                        -- SEC-USR-FNAME
    last_name   VARCHAR(20),                        -- SEC-USR-LNAME
    password    VARCHAR(8),                         -- SEC-USR-PWD (legacy clear text)
    user_type   VARCHAR(1)                          -- SEC-USR-TYPE: A admin, U user
);

-- CVACT01Y - ACCTDAT (RECLN 300)
CREATE TABLE account (
    acct_id            BIGINT        NOT NULL PRIMARY KEY,  -- ACCT-ID 9(11)
    active_status      VARCHAR(1),                          -- ACCT-ACTIVE-STATUS
    curr_bal           NUMERIC(12,2),                       -- ACCT-CURR-BAL S9(10)V99
    credit_limit       NUMERIC(12,2),                       -- ACCT-CREDIT-LIMIT
    cash_credit_limit  NUMERIC(12,2),                       -- ACCT-CASH-CREDIT-LIMIT
    open_date          DATE,                                -- ACCT-OPEN-DATE X(10)
    expiration_date    DATE,                                -- ACCT-EXPIRAION-DATE X(10)
    reissue_date       DATE,                                -- ACCT-REISSUE-DATE X(10)
    curr_cyc_credit    NUMERIC(12,2),                       -- ACCT-CURR-CYC-CREDIT
    curr_cyc_debit     NUMERIC(12,2),                       -- ACCT-CURR-CYC-DEBIT
    addr_zip           VARCHAR(10),                         -- ACCT-ADDR-ZIP
    group_id           VARCHAR(10)                          -- ACCT-GROUP-ID
);

-- CVACT02Y - CARDDAT (RECLN 150), AIX CARDAIX on CARD-ACCT-ID
CREATE TABLE card (
    card_num         VARCHAR(16) NOT NULL PRIMARY KEY,  -- CARD-NUM
    acct_id          BIGINT      NOT NULL,              -- CARD-ACCT-ID 9(11)
    cvv_cd           INTEGER,                           -- CARD-CVV-CD 9(03)
    embossed_name    VARCHAR(50),                       -- CARD-EMBOSSED-NAME
    expiration_date  DATE,                              -- CARD-EXPIRAION-DATE X(10)
    active_status    VARCHAR(1)                         -- CARD-ACTIVE-STATUS
);
CREATE INDEX ix_card_acct_id ON card (acct_id);

-- CVCUS01Y - CUSTDAT (RECLN 500)
CREATE TABLE customer (
    cust_id              BIGINT      NOT NULL PRIMARY KEY,  -- CUST-ID 9(09)
    first_name           VARCHAR(25),                       -- CUST-FIRST-NAME
    middle_name          VARCHAR(25),                       -- CUST-MIDDLE-NAME
    last_name            VARCHAR(25),                       -- CUST-LAST-NAME
    addr_line_1          VARCHAR(50),                       -- CUST-ADDR-LINE-1
    addr_line_2          VARCHAR(50),                       -- CUST-ADDR-LINE-2
    addr_line_3          VARCHAR(50),                       -- CUST-ADDR-LINE-3
    addr_state_cd        VARCHAR(2),                        -- CUST-ADDR-STATE-CD
    addr_country_cd      VARCHAR(3),                        -- CUST-ADDR-COUNTRY-CD
    addr_zip             VARCHAR(10),                       -- CUST-ADDR-ZIP
    phone_num_1          VARCHAR(15),                       -- CUST-PHONE-NUM-1
    phone_num_2          VARCHAR(15),                       -- CUST-PHONE-NUM-2
    ssn                  VARCHAR(9),                        -- CUST-SSN 9(09)
    govt_issued_id       VARCHAR(20),                       -- CUST-GOVT-ISSUED-ID
    dob                  DATE,                              -- CUST-DOB-YYYY-MM-DD X(10)
    eft_account_id       VARCHAR(10),                       -- CUST-EFT-ACCOUNT-ID
    pri_card_holder_ind  VARCHAR(1),                        -- CUST-PRI-CARD-HOLDER-IND
    fico_credit_score    INTEGER                            -- CUST-FICO-CREDIT-SCORE 9(03)
);

-- CVACT03Y - CCXREF (RECLN 50), AIX CXACAIX on XREF-ACCT-ID
CREATE TABLE card_xref (
    card_num  VARCHAR(16) NOT NULL PRIMARY KEY,  -- XREF-CARD-NUM
    cust_id   BIGINT      NOT NULL,              -- XREF-CUST-ID 9(09)
    acct_id   BIGINT      NOT NULL               -- XREF-ACCT-ID 9(11)
);
CREATE INDEX ix_card_xref_acct_id ON card_xref (acct_id);

-- CVTRA05Y - TRANSACT (RECLN 350), AIX on TRAN-PROC-TS
CREATE TABLE transactions (
    tran_id        VARCHAR(16)   NOT NULL PRIMARY KEY,  -- TRAN-ID
    type_cd        VARCHAR(2),                          -- TRAN-TYPE-CD
    cat_cd         INTEGER,                             -- TRAN-CAT-CD 9(04)
    source         VARCHAR(10),                         -- TRAN-SOURCE
    description    VARCHAR(100),                        -- TRAN-DESC
    amount         NUMERIC(11,2),                       -- TRAN-AMT S9(09)V99
    merchant_id    BIGINT,                              -- TRAN-MERCHANT-ID 9(09)
    merchant_name  VARCHAR(50),                         -- TRAN-MERCHANT-NAME
    merchant_city  VARCHAR(50),                         -- TRAN-MERCHANT-CITY
    merchant_zip   VARCHAR(10),                         -- TRAN-MERCHANT-ZIP
    card_num       VARCHAR(16),                         -- TRAN-CARD-NUM
    orig_ts        TIMESTAMP(6),                        -- TRAN-ORIG-TS X(26)
    proc_ts        TIMESTAMP(6)                         -- TRAN-PROC-TS X(26)
);
CREATE INDEX ix_transactions_proc_ts ON transactions (proc_ts);
CREATE INDEX ix_transactions_card_num ON transactions (card_num);

-- CVTRA06Y - DALYTRAN (RECLN 350), sequential input of POSTTRAN
CREATE TABLE daily_transactions (
    tran_id        VARCHAR(16)   NOT NULL PRIMARY KEY,  -- DALYTRAN-ID
    type_cd        VARCHAR(2),
    cat_cd         INTEGER,
    source         VARCHAR(10),
    description    VARCHAR(100),
    amount         NUMERIC(11,2),
    merchant_id    BIGINT,
    merchant_name  VARCHAR(50),
    merchant_city  VARCHAR(50),
    merchant_zip   VARCHAR(10),
    card_num       VARCHAR(16),
    orig_ts        TIMESTAMP(6),
    proc_ts        TIMESTAMP(6)
);
CREATE INDEX ix_daily_transactions_card_num ON daily_transactions (card_num);

-- CVTRA01Y - TCATBALF (RECLN 50), key TRAN-CAT-KEY
CREATE TABLE tran_cat_balance (
    acct_id  BIGINT        NOT NULL,  -- TRANCAT-ACCT-ID 9(11)
    type_cd  VARCHAR(2)    NOT NULL,  -- TRANCAT-TYPE-CD
    cat_cd   INTEGER       NOT NULL,  -- TRANCAT-CD 9(04)
    balance  NUMERIC(11,2),           -- TRAN-CAT-BAL S9(09)V99
    PRIMARY KEY (acct_id, type_cd, cat_cd)
);

-- CVTRA02Y - DISCGRP (RECLN 50), key DIS-GROUP-KEY
CREATE TABLE disclosure_group (
    group_id       VARCHAR(10)  NOT NULL,  -- DIS-ACCT-GROUP-ID
    type_cd        VARCHAR(2)   NOT NULL,  -- DIS-TRAN-TYPE-CD
    cat_cd         INTEGER      NOT NULL,  -- DIS-TRAN-CAT-CD 9(04)
    interest_rate  NUMERIC(6,2),           -- DIS-INT-RATE S9(04)V99
    PRIMARY KEY (group_id, type_cd, cat_cd)
);

-- CVTRA03Y - TRANTYPE (RECLN 60)
CREATE TABLE transaction_type (
    type_cd      VARCHAR(2)  NOT NULL PRIMARY KEY,  -- TRAN-TYPE
    description  VARCHAR(50)                        -- TRAN-TYPE-DESC
);

-- CVTRA04Y - TRANCATG (RECLN 60), key TRAN-CAT-KEY
CREATE TABLE transaction_category (
    type_cd      VARCHAR(2)  NOT NULL,  -- TRAN-TYPE-CD
    cat_cd       INTEGER     NOT NULL,  -- TRAN-CAT-CD 9(04)
    description  VARCHAR(50),           -- TRAN-CAT-TYPE-DESC
    PRIMARY KEY (type_cd, cat_cd)
);
