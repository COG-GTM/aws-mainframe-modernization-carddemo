-- TRANSACT VSAM KSDS (copybook CVTRA05Y, record length 350), key TRAN-ID.
CREATE TABLE transactions (
    tran_id             VARCHAR(16)     NOT NULL,
    type_cd             VARCHAR(2)      NOT NULL,
    cat_cd              INTEGER         NOT NULL,
    source              VARCHAR(10),
    description         VARCHAR(100),
    amount              NUMERIC(11, 2)  NOT NULL,
    merchant_id         BIGINT,
    merchant_name       VARCHAR(50),
    merchant_city       VARCHAR(50),
    merchant_zip        VARCHAR(10),
    card_num            VARCHAR(16)     NOT NULL,
    orig_ts             TIMESTAMP,
    proc_ts             TIMESTAMP,
    CONSTRAINT pk_transactions PRIMARY KEY (tran_id)
);

CREATE INDEX ix_transactions_card_num ON transactions (card_num, orig_ts);

-- DALYTRAN input file (copybook CVTRA06Y, record length 350) staged as a table.
CREATE TABLE daily_transactions (
    tran_id             VARCHAR(16)     NOT NULL,
    type_cd             VARCHAR(2)      NOT NULL,
    cat_cd              INTEGER         NOT NULL,
    source              VARCHAR(10),
    description         VARCHAR(100),
    amount              NUMERIC(11, 2)  NOT NULL,
    merchant_id         BIGINT,
    merchant_name       VARCHAR(50),
    merchant_city       VARCHAR(50),
    merchant_zip        VARCHAR(10),
    card_num            VARCHAR(16)     NOT NULL,
    orig_ts             TIMESTAMP,
    proc_ts             TIMESTAMP,
    processed           BOOLEAN         NOT NULL DEFAULT FALSE,
    CONSTRAINT pk_daily_transactions PRIMARY KEY (tran_id)
);

CREATE INDEX ix_daily_transactions_processed ON daily_transactions (processed, tran_id);

-- DALYREJS output of CBTRN02C paragraph 2500-WRITE-REJECT-REC.
CREATE TABLE transaction_rejects (
    reject_id           BIGSERIAL       NOT NULL,
    tran_id             VARCHAR(16)     NOT NULL,
    card_num            VARCHAR(16),
    reason_code         INTEGER         NOT NULL,
    reason_desc         VARCHAR(80),
    rejected_at         TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_transaction_rejects PRIMARY KEY (reject_id)
);

-- TRANTYPE (CVTRA03Y) and TRANCATG (CVTRA04Y).
CREATE TABLE transaction_types (
    type_cd             VARCHAR(2)      NOT NULL,
    type_desc           VARCHAR(50),
    CONSTRAINT pk_transaction_types PRIMARY KEY (type_cd)
);

CREATE TABLE transaction_categories (
    type_cd             VARCHAR(2)      NOT NULL,
    cat_cd              INTEGER         NOT NULL,
    cat_type_desc       VARCHAR(50),
    CONSTRAINT pk_transaction_categories PRIMARY KEY (type_cd, cat_cd),
    CONSTRAINT fk_transaction_categories_type FOREIGN KEY (type_cd)
        REFERENCES transaction_types (type_cd)
);

-- DISCGRP (CVTRA02Y): DIS-INT-RATE is PIC S9(04)V99.
CREATE TABLE disclosure_groups (
    acct_group_id       VARCHAR(10)     NOT NULL,
    type_cd             VARCHAR(2)      NOT NULL,
    cat_cd              INTEGER         NOT NULL,
    int_rate            NUMERIC(6, 2)   NOT NULL,
    CONSTRAINT pk_disclosure_groups PRIMARY KEY (acct_group_id, type_cd, cat_cd)
);

-- TCATBAL (CVTRA01Y): balance per account, transaction type and category.
CREATE TABLE category_balances (
    acct_id             BIGINT          NOT NULL,
    type_cd             VARCHAR(2)      NOT NULL,
    cat_cd              INTEGER         NOT NULL,
    balance             NUMERIC(11, 2)  NOT NULL DEFAULT 0,
    CONSTRAINT pk_category_balances PRIMARY KEY (acct_id, type_cd, cat_cd),
    CONSTRAINT ck_category_balances_acct_id CHECK (acct_id BETWEEN 0 AND 99999999999)
);
