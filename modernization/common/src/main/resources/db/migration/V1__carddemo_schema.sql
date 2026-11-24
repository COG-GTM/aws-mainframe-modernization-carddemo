-- CardDemo relational schema (PostgreSQL / Aurora PostgreSQL; also runs on H2 in PostgreSQL mode for tests).
-- Each table maps one VSAM KSDS. Alternate indexes (AIX) become secondary indexes.
-- Source copybooks/JCL are cited per table; see docs/modernization/02-data-model.md.

-- USRSEC  (KSDS key 8 @0, 80 bytes)  copybook CSUSR01Y  -- RACF/USRSEC replacement
CREATE TABLE user_security (
    user_id        VARCHAR(8)   NOT NULL PRIMARY KEY,
    first_name     VARCHAR(20)  NOT NULL,
    last_name      VARCHAR(20)  NOT NULL,
    password_hash  VARCHAR(100) NOT NULL,
    user_type      CHAR(1)      NOT NULL,
    version        BIGINT       NOT NULL DEFAULT 0,
    CONSTRAINT ck_user_type CHECK (user_type IN ('A', 'U'))
);

-- CUSTDAT (KSDS key 9 @0, 500 bytes)  copybook CVCUS01Y
CREATE TABLE customer (
    cust_id              BIGINT       NOT NULL PRIMARY KEY,
    first_name           VARCHAR(25)  NOT NULL,
    middle_name          VARCHAR(25),
    last_name            VARCHAR(25)  NOT NULL,
    addr_line_1          VARCHAR(50),
    addr_line_2          VARCHAR(50),
    addr_line_3          VARCHAR(50),
    addr_state_cd        CHAR(2),
    addr_country_cd      CHAR(3),
    addr_zip             VARCHAR(10),
    phone_num_1          VARCHAR(15),
    phone_num_2          VARCHAR(15),
    ssn                  CHAR(9),
    govt_issued_id       VARCHAR(20),
    dob                  DATE,
    eft_account_id       VARCHAR(10),
    pri_card_holder_ind  CHAR(1),
    fico_credit_score    INTEGER,
    version              BIGINT       NOT NULL DEFAULT 0
);

-- ACCTDAT (KSDS key 11 @0, 300 bytes)  copybook CVACT01Y
CREATE TABLE account (
    acct_id                  BIGINT        NOT NULL PRIMARY KEY,
    active_status            CHAR(1)       NOT NULL,
    curr_bal                 NUMERIC(12,2) NOT NULL DEFAULT 0,
    credit_limit             NUMERIC(12,2) NOT NULL DEFAULT 0,
    cash_credit_limit        NUMERIC(12,2) NOT NULL DEFAULT 0,
    open_date                DATE,
    expiration_date          DATE,
    reissue_date             DATE,
    curr_cyc_credit          NUMERIC(12,2) NOT NULL DEFAULT 0,
    curr_cyc_debit           NUMERIC(12,2) NOT NULL DEFAULT 0,
    addr_zip                 VARCHAR(10),
    group_id                 VARCHAR(10),
    version                  BIGINT        NOT NULL DEFAULT 0
);

-- CARDDAT (KSDS key 16 @0, 150 bytes)  copybook CVACT02Y
CREATE TABLE card (
    card_num         CHAR(16)     NOT NULL PRIMARY KEY,
    acct_id          BIGINT       NOT NULL REFERENCES account (acct_id),
    cvv_cd           CHAR(3),
    embossed_name    VARCHAR(50),
    expiration_date  DATE,
    active_status    CHAR(1)      NOT NULL,
    version          BIGINT       NOT NULL DEFAULT 0
);
-- CARDAIX: non-unique AIX on CARD-ACCT-ID (11 @16)
CREATE INDEX ix_card_acct_id ON card (acct_id);

-- CCXREF  (KSDS key 16 @0, 50 bytes)  copybook CVACT03Y
CREATE TABLE card_xref (
    card_num  CHAR(16) NOT NULL PRIMARY KEY,
    cust_id   BIGINT   NOT NULL REFERENCES customer (cust_id),
    acct_id   BIGINT   NOT NULL REFERENCES account (acct_id)
);
-- CXACAIX: non-unique AIX on XREF-ACCT-ID (11 @25)
CREATE INDEX ix_card_xref_acct_id ON card_xref (acct_id);

-- TRANTYPE (KSDS key 2 @0, 60 bytes)  copybook CVTRA03Y
CREATE TABLE transaction_type (
    type_cd      CHAR(2)     NOT NULL PRIMARY KEY,
    description  VARCHAR(50) NOT NULL
);

-- TRANCATG (KSDS key 6 @0, 60 bytes)  copybook CVTRA04Y
CREATE TABLE transaction_category (
    type_cd      CHAR(2)     NOT NULL REFERENCES transaction_type (type_cd),
    cat_cd       INTEGER     NOT NULL,
    description  VARCHAR(50) NOT NULL,
    PRIMARY KEY (type_cd, cat_cd)
);

-- DISCGRP (KSDS key 16 @0, 50 bytes)  copybook CVTRA02Y
CREATE TABLE disclosure_group (
    acct_group_id  VARCHAR(10)  NOT NULL,
    type_cd        CHAR(2)      NOT NULL,
    cat_cd         INTEGER      NOT NULL,
    int_rate       NUMERIC(6,2) NOT NULL,
    PRIMARY KEY (acct_group_id, type_cd, cat_cd)
);

-- TCATBALF (KSDS key 17 @0, 50 bytes)  copybook CVTRA01Y
CREATE TABLE tran_cat_balance (
    acct_id   BIGINT        NOT NULL REFERENCES account (acct_id),
    type_cd   CHAR(2)       NOT NULL,
    cat_cd    INTEGER       NOT NULL,
    balance   NUMERIC(11,2) NOT NULL DEFAULT 0,
    PRIMARY KEY (acct_id, type_cd, cat_cd)
);

-- TRANSACT (KSDS key 16 @0, 350 bytes)  copybook CVTRA05Y
CREATE TABLE card_transaction (
    tran_id        CHAR(16)      NOT NULL PRIMARY KEY,
    type_cd        CHAR(2)       NOT NULL,
    cat_cd         INTEGER       NOT NULL,
    source         VARCHAR(10),
    description    VARCHAR(100),
    amount         NUMERIC(11,2) NOT NULL,
    merchant_id    BIGINT,
    merchant_name  VARCHAR(50),
    merchant_city  VARCHAR(50),
    merchant_zip   VARCHAR(10),
    card_num       CHAR(16)      NOT NULL,
    orig_ts        TIMESTAMP,
    proc_ts        TIMESTAMP
);
-- TRANSACT AIX: non-unique on TRAN-PROC-TS (26 @304)
CREATE INDEX ix_card_transaction_proc_ts ON card_transaction (proc_ts);
CREATE INDEX ix_card_transaction_card_num ON card_transaction (card_num);

-- Replaces "browse to last TRAN-ID and add 1" in COBIL00C / COTRN02C.
CREATE SEQUENCE tran_id_seq START WITH 1 INCREMENT BY 1;

-- DALYTRAN input records staged for POSTTRAN (copybook CVTRA06Y, PS file)
CREATE TABLE daily_transaction (
    tran_id        CHAR(16)      NOT NULL PRIMARY KEY,
    type_cd        CHAR(2)       NOT NULL,
    cat_cd         INTEGER       NOT NULL,
    source         VARCHAR(10),
    description    VARCHAR(100),
    amount         NUMERIC(11,2) NOT NULL,
    merchant_id    BIGINT,
    merchant_name  VARCHAR(50),
    merchant_city  VARCHAR(50),
    merchant_zip   VARCHAR(10),
    card_num       CHAR(16)      NOT NULL,
    orig_ts        TIMESTAMP,
    proc_ts        TIMESTAMP,
    batch_id       VARCHAR(36),
    status         VARCHAR(10)   NOT NULL DEFAULT 'PENDING'
);

-- DALYREJS output (430 bytes = 350 tran + 80 validation trailer) from POSTTRAN
CREATE TABLE daily_transaction_reject (
    id               BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    tran_id          CHAR(16)     NOT NULL,
    card_num         CHAR(16),
    amount           NUMERIC(11,2),
    reason_code      INTEGER      NOT NULL,
    reason_desc      VARCHAR(76)  NOT NULL,
    batch_id         VARCHAR(36),
    rejected_at      TIMESTAMP    NOT NULL
);

-- CORPT00C submits TRANREPT via an internal reader; modern equivalent is a queued report request.
CREATE TABLE report_request (
    id            BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    report_type   VARCHAR(10)  NOT NULL,
    start_date    DATE         NOT NULL,
    end_date      DATE         NOT NULL,
    requested_by  VARCHAR(8)   NOT NULL,
    status        VARCHAR(12)  NOT NULL DEFAULT 'SUBMITTED',
    requested_at  TIMESTAMP    NOT NULL,
    completed_at  TIMESTAMP,
    output        VARCHAR(1000000)
);
