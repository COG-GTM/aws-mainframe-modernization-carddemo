-- Local/test schema for the online services, derived from aws/contracts/data-model.md (v1).
-- The canonical DDL is owned by the data-migration session (aws/db, aws/data-migration); this copy only
-- exists so the service can run and be tested stand-alone. Keep it in sync with the contract.

CREATE TABLE IF NOT EXISTS user_security (
    user_id        VARCHAR(8)   PRIMARY KEY,
    first_name     VARCHAR(20)  NOT NULL,
    last_name      VARCHAR(20)  NOT NULL,
    password_hash  VARCHAR(100) NOT NULL,
    user_type      CHAR(1)      NOT NULL CHECK (user_type IN ('A', 'U')),
    version        BIGINT       NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS account (
    acct_id            NUMERIC(11,0) PRIMARY KEY,
    active_status      CHAR(1)       NOT NULL,
    curr_bal           NUMERIC(12,2) NOT NULL,
    credit_limit       NUMERIC(12,2) NOT NULL,
    cash_credit_limit  NUMERIC(12,2) NOT NULL,
    open_date          DATE,
    expiration_date    DATE,
    reissue_date       DATE,
    curr_cyc_credit    NUMERIC(12,2) NOT NULL,
    curr_cyc_debit     NUMERIC(12,2) NOT NULL,
    addr_zip           VARCHAR(10),
    group_id           VARCHAR(10),
    version            BIGINT        NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS customer (
    cust_id              INTEGER      PRIMARY KEY,
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
    ssn                  CHAR(9)      NOT NULL,
    govt_issued_id       VARCHAR(20),
    dob                  DATE,
    eft_account_id       VARCHAR(10),
    pri_card_holder_ind  CHAR(1),
    fico_credit_score    SMALLINT,
    version              BIGINT       NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS card (
    card_num         CHAR(16)      PRIMARY KEY,
    acct_id          NUMERIC(11,0) NOT NULL REFERENCES account (acct_id) DEFERRABLE INITIALLY DEFERRED,
    cvv_cd           SMALLINT      NOT NULL,
    embossed_name    VARCHAR(50)   NOT NULL,
    expiration_date  DATE,
    active_status    CHAR(1)       NOT NULL,
    version          BIGINT        NOT NULL DEFAULT 0
);
CREATE INDEX IF NOT EXISTS ix_card_acct_id ON card (acct_id);

CREATE TABLE IF NOT EXISTS card_xref (
    card_num  CHAR(16)      PRIMARY KEY REFERENCES card (card_num) DEFERRABLE INITIALLY DEFERRED,
    cust_id   INTEGER       NOT NULL REFERENCES customer (cust_id) DEFERRABLE INITIALLY DEFERRED,
    acct_id   NUMERIC(11,0) NOT NULL REFERENCES account (acct_id) DEFERRABLE INITIALLY DEFERRED
);
CREATE INDEX IF NOT EXISTS ix_card_xref_acct_id ON card_xref (acct_id);

CREATE TABLE IF NOT EXISTS transaction_type (
    type_cd      CHAR(2)     PRIMARY KEY,
    description  VARCHAR(50) NOT NULL,
    version      BIGINT      NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS transaction_category (
    type_cd      CHAR(2)     NOT NULL REFERENCES transaction_type (type_cd) ON DELETE RESTRICT,
    cat_cd       SMALLINT    NOT NULL,
    description  VARCHAR(50) NOT NULL,
    PRIMARY KEY (type_cd, cat_cd)
);

CREATE TABLE IF NOT EXISTS transaction (
    tran_id        CHAR(16)      PRIMARY KEY,
    type_cd        CHAR(2)       NOT NULL REFERENCES transaction_type (type_cd),
    cat_cd         SMALLINT      NOT NULL,
    source         VARCHAR(10),
    description    VARCHAR(100),
    amt            NUMERIC(11,2) NOT NULL,
    merchant_id    INTEGER,
    merchant_name  VARCHAR(50),
    merchant_city  VARCHAR(50),
    merchant_zip   VARCHAR(10),
    card_num       CHAR(16)      NOT NULL,
    orig_ts        TIMESTAMP(6),
    proc_ts        TIMESTAMP(6),
    FOREIGN KEY (type_cd, cat_cd) REFERENCES transaction_category (type_cd, cat_cd)
);
CREATE INDEX IF NOT EXISTS ix_transaction_proc_ts ON transaction (proc_ts);
CREATE INDEX IF NOT EXISTS ix_transaction_card_num ON transaction (card_num);

CREATE TABLE IF NOT EXISTS tran_cat_balance (
    acct_id  NUMERIC(11,0) NOT NULL,
    type_cd  CHAR(2)       NOT NULL,
    cat_cd   SMALLINT      NOT NULL,
    balance  NUMERIC(11,2) NOT NULL,
    version  BIGINT        NOT NULL DEFAULT 0,
    PRIMARY KEY (acct_id, type_cd, cat_cd)
);

CREATE TABLE IF NOT EXISTS disclosure_group (
    acct_group_id  VARCHAR(10)  NOT NULL,
    type_cd        CHAR(2)      NOT NULL,
    cat_cd         SMALLINT     NOT NULL,
    int_rate       NUMERIC(6,2) NOT NULL,
    PRIMARY KEY (acct_group_id, type_cd, cat_cd)
);

CREATE TABLE IF NOT EXISTS processed_message (
    message_id    UUID         PRIMARY KEY,
    queue         VARCHAR(80)  NOT NULL,
    reply_body    JSONB,
    processed_at  TIMESTAMPTZ  NOT NULL
);
