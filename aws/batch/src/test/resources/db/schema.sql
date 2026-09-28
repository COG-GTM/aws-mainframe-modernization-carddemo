-- Test/local schema for aws/batch, derived from aws/contracts/data-model.md (tables used by the batch jobs).
-- The authoritative Flyway schema is owned by aws/db/.
CREATE SCHEMA IF NOT EXISTS carddemo;
SET search_path TO carddemo;

CREATE TABLE IF NOT EXISTS customer (
    cust_id             INTEGER PRIMARY KEY,
    first_name          VARCHAR(25) NOT NULL,
    middle_name         VARCHAR(25),
    last_name           VARCHAR(25) NOT NULL,
    addr_line_1         VARCHAR(50),
    addr_line_2         VARCHAR(50),
    addr_line_3         VARCHAR(50),
    addr_state_cd       CHAR(2),
    addr_country_cd     CHAR(3),
    addr_zip            VARCHAR(10),
    phone_num_1         VARCHAR(15),
    phone_num_2         VARCHAR(15),
    ssn                 CHAR(9) NOT NULL,
    govt_issued_id      VARCHAR(20),
    dob                 DATE,
    eft_account_id      VARCHAR(10),
    pri_card_holder_ind CHAR(1),
    fico_credit_score   SMALLINT,
    version             BIGINT NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS account (
    acct_id           NUMERIC(11,0) PRIMARY KEY,
    active_status     CHAR(1) NOT NULL,
    curr_bal          NUMERIC(12,2) NOT NULL,
    credit_limit      NUMERIC(12,2) NOT NULL,
    cash_credit_limit NUMERIC(12,2) NOT NULL,
    open_date         DATE,
    expiration_date   DATE,
    reissue_date      DATE,
    curr_cyc_credit   NUMERIC(12,2) NOT NULL,
    curr_cyc_debit    NUMERIC(12,2) NOT NULL,
    addr_zip          VARCHAR(10),
    group_id          VARCHAR(10),
    version           BIGINT NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS card (
    card_num        CHAR(16) PRIMARY KEY,
    acct_id         NUMERIC(11,0) NOT NULL REFERENCES account (acct_id),
    cvv_cd          SMALLINT NOT NULL,
    embossed_name   VARCHAR(50) NOT NULL,
    expiration_date DATE,
    active_status   CHAR(1) NOT NULL,
    version         BIGINT NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS card_xref (
    card_num CHAR(16) PRIMARY KEY REFERENCES card (card_num),
    cust_id  INTEGER NOT NULL REFERENCES customer (cust_id),
    acct_id  NUMERIC(11,0) NOT NULL REFERENCES account (acct_id)
);
CREATE INDEX IF NOT EXISTS card_xref_acct_id_idx ON card_xref (acct_id);

CREATE TABLE IF NOT EXISTS transaction_type (
    type_cd     CHAR(2) PRIMARY KEY,
    description VARCHAR(50) NOT NULL,
    version     BIGINT NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS transaction_category (
    type_cd     CHAR(2) NOT NULL REFERENCES transaction_type (type_cd),
    cat_cd      SMALLINT NOT NULL,
    description VARCHAR(50) NOT NULL,
    PRIMARY KEY (type_cd, cat_cd)
);

CREATE TABLE IF NOT EXISTS transaction (
    tran_id       CHAR(16) PRIMARY KEY,
    type_cd       CHAR(2) NOT NULL REFERENCES transaction_type (type_cd),
    cat_cd        SMALLINT NOT NULL,
    source        VARCHAR(10),
    description   VARCHAR(100),
    amt           NUMERIC(11,2) NOT NULL,
    merchant_id   INTEGER,
    merchant_name VARCHAR(50),
    merchant_city VARCHAR(50),
    merchant_zip  VARCHAR(10),
    card_num      CHAR(16) NOT NULL,
    orig_ts       TIMESTAMP(6),
    proc_ts       TIMESTAMP(6),
    FOREIGN KEY (type_cd, cat_cd) REFERENCES transaction_category (type_cd, cat_cd)
);
CREATE INDEX IF NOT EXISTS transaction_card_num_idx ON transaction (card_num);
CREATE INDEX IF NOT EXISTS transaction_proc_ts_idx ON transaction (proc_ts);

CREATE TABLE IF NOT EXISTS daily_transaction (
    run_id        VARCHAR(40) NOT NULL,
    load_seq      INTEGER NOT NULL,
    tran_id       CHAR(16) NOT NULL,
    type_cd       CHAR(2) NOT NULL,
    cat_cd        SMALLINT NOT NULL,
    source        VARCHAR(10),
    description   VARCHAR(100),
    amt           NUMERIC(11,2) NOT NULL,
    merchant_id   INTEGER,
    merchant_name VARCHAR(50),
    merchant_city VARCHAR(50),
    merchant_zip  VARCHAR(10),
    card_num      CHAR(16) NOT NULL,
    orig_ts       TIMESTAMP(6),
    proc_ts       TIMESTAMP(6),
    post_status   CHAR(1),
    reject_reason SMALLINT,
    PRIMARY KEY (run_id, load_seq)
);

CREATE TABLE IF NOT EXISTS tran_cat_balance (
    acct_id NUMERIC(11,0) NOT NULL,
    type_cd CHAR(2) NOT NULL,
    cat_cd  SMALLINT NOT NULL,
    balance NUMERIC(11,2) NOT NULL,
    version BIGINT NOT NULL DEFAULT 0,
    PRIMARY KEY (acct_id, type_cd, cat_cd)
);

CREATE TABLE IF NOT EXISTS disclosure_group (
    acct_group_id VARCHAR(10) NOT NULL,
    type_cd       CHAR(2) NOT NULL,
    cat_cd        SMALLINT NOT NULL,
    int_rate      NUMERIC(6,2) NOT NULL,
    PRIMARY KEY (acct_group_id, type_cd, cat_cd)
);

CREATE TABLE IF NOT EXISTS batch_job_run (
    run_id        VARCHAR(40) NOT NULL,
    job_name      VARCHAR(64) NOT NULL,
    business_date DATE,
    status        VARCHAR(16) NOT NULL,
    exit_code     SMALLINT,
    started_at    TIMESTAMPTZ,
    ended_at      TIMESTAMPTZ,
    counts        JSONB,
    PRIMARY KEY (run_id, job_name)
);
