-- CardDemo canonical schema (Aurora PostgreSQL / PostgreSQL 15+).
-- Source of truth: aws/contracts/data-model.md. Idempotent: safe to run repeatedly.
-- Core VSAM tables + technical tables. Optional sub-app tables: db2/*.sql (DB2) and ims/*.sql (IMS).

CREATE SCHEMA IF NOT EXISTS carddemo;
SET search_path TO carddemo;

-- ---------------------------------------------------------------------------------------------
-- 2.1 user_security <- CSUSR01Y / USRSEC.VSAM.KSDS (80 bytes, key SEC-USR-ID 8 @0)
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS user_security (
    user_id        VARCHAR(8)   NOT NULL,
    first_name     VARCHAR(20)  NOT NULL,
    last_name      VARCHAR(20)  NOT NULL,
    password_hash  VARCHAR(100) NOT NULL,
    user_type      CHAR(1)      NOT NULL,
    version        BIGINT       NOT NULL DEFAULT 0,
    CONSTRAINT pk_user_security PRIMARY KEY (user_id),
    CONSTRAINT ck_user_security_user_id_upper CHECK (user_id = upper(btrim(user_id)) AND user_id <> ''),
    CONSTRAINT ck_user_security_user_type CHECK (user_type IN ('A', 'U')),
    CONSTRAINT ck_user_security_password_bcrypt CHECK (password_hash ~ '^\$2[aby]\$[0-9]{2}\$.{53}$')
);
COMMENT ON TABLE  user_security IS 'CSUSR01Y SEC-USER-DATA; VSAM USRSEC.VSAM.KSDS (CICS file USRSEC)';
COMMENT ON COLUMN user_security.user_id IS 'SEC-USR-ID PIC X(08), stored upper-case trimmed';
COMMENT ON COLUMN user_security.first_name IS 'SEC-USR-FNAME PIC X(20)';
COMMENT ON COLUMN user_security.last_name IS 'SEC-USR-LNAME PIC X(20)';
COMMENT ON COLUMN user_security.password_hash IS 'SEC-USR-PWD PIC X(08): BCrypt hash of the upper-cased legacy plain-text password';
COMMENT ON COLUMN user_security.user_type IS 'SEC-USR-TYPE PIC X(01): A=admin, U=user';
COMMENT ON COLUMN user_security.version IS 'technical: optimistic lock';

-- ---------------------------------------------------------------------------------------------
-- 2.2 account <- CVACT01Y / ACCTDATA.VSAM.KSDS (300 bytes, key ACCT-ID 11 @0)
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS account (
    acct_id            NUMERIC(11,0) NOT NULL,
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
    version            BIGINT        NOT NULL DEFAULT 0,
    CONSTRAINT pk_account PRIMARY KEY (acct_id),
    CONSTRAINT ck_account_acct_id CHECK (acct_id >= 0),
    CONSTRAINT ck_account_active_status CHECK (active_status IN ('Y', 'N'))
);
COMMENT ON TABLE  account IS 'CVACT01Y ACCOUNT-RECORD; VSAM ACCTDATA.VSAM.KSDS (CICS file ACCTDAT)';
COMMENT ON COLUMN account.acct_id IS 'ACCT-ID PIC 9(11)';
COMMENT ON COLUMN account.active_status IS 'ACCT-ACTIVE-STATUS PIC X(01) Y/N';
COMMENT ON COLUMN account.curr_bal IS 'ACCT-CURR-BAL PIC S9(10)V99';
COMMENT ON COLUMN account.credit_limit IS 'ACCT-CREDIT-LIMIT PIC S9(10)V99';
COMMENT ON COLUMN account.cash_credit_limit IS 'ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99';
COMMENT ON COLUMN account.open_date IS 'ACCT-OPEN-DATE PIC X(10) YYYY-MM-DD';
COMMENT ON COLUMN account.expiration_date IS 'ACCT-EXPIRAION-DATE PIC X(10) YYYY-MM-DD (source misspelling)';
COMMENT ON COLUMN account.reissue_date IS 'ACCT-REISSUE-DATE PIC X(10) YYYY-MM-DD';
COMMENT ON COLUMN account.curr_cyc_credit IS 'ACCT-CURR-CYC-CREDIT PIC S9(10)V99';
COMMENT ON COLUMN account.curr_cyc_debit IS 'ACCT-CURR-CYC-DEBIT PIC S9(10)V99';
COMMENT ON COLUMN account.addr_zip IS 'ACCT-ADDR-ZIP PIC X(10)';
COMMENT ON COLUMN account.group_id IS 'ACCT-GROUP-ID PIC X(10); looked up in disclosure_group.acct_group_id (fallback DEFAULT)';
COMMENT ON COLUMN account.version IS 'technical: optimistic lock';

-- ---------------------------------------------------------------------------------------------
-- 2.5 customer <- CVCUS01Y / CUSTDATA.VSAM.KSDS (500 bytes, key CUST-ID 9 @0)
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer (
    cust_id              INTEGER      NOT NULL,
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
    version              BIGINT       NOT NULL DEFAULT 0,
    CONSTRAINT pk_customer PRIMARY KEY (cust_id),
    CONSTRAINT ck_customer_cust_id CHECK (cust_id BETWEEN 0 AND 999999999),
    CONSTRAINT ck_customer_ssn CHECK (ssn ~ '^[0-9]{9}$'),
    CONSTRAINT ck_customer_pri_card_holder_ind CHECK (pri_card_holder_ind IN ('Y', 'N')),
    -- PIC 9(03) domain; COACTUPC enforces 300-850 on update only (sample data contains lower scores).
    CONSTRAINT ck_customer_fico_credit_score CHECK (fico_credit_score BETWEEN 0 AND 999)
);
COMMENT ON TABLE  customer IS 'CVCUS01Y CUSTOMER-RECORD; VSAM CUSTDATA.VSAM.KSDS (CICS file CUSTDAT)';
COMMENT ON COLUMN customer.cust_id IS 'CUST-ID PIC 9(09)';
COMMENT ON COLUMN customer.first_name IS 'CUST-FIRST-NAME PIC X(25)';
COMMENT ON COLUMN customer.middle_name IS 'CUST-MIDDLE-NAME PIC X(25)';
COMMENT ON COLUMN customer.last_name IS 'CUST-LAST-NAME PIC X(25)';
COMMENT ON COLUMN customer.addr_line_1 IS 'CUST-ADDR-LINE-1 PIC X(50)';
COMMENT ON COLUMN customer.addr_line_2 IS 'CUST-ADDR-LINE-2 PIC X(50)';
COMMENT ON COLUMN customer.addr_line_3 IS 'CUST-ADDR-LINE-3 PIC X(50)';
COMMENT ON COLUMN customer.addr_state_cd IS 'CUST-ADDR-STATE-CD PIC X(02)';
COMMENT ON COLUMN customer.addr_country_cd IS 'CUST-ADDR-COUNTRY-CD PIC X(03)';
COMMENT ON COLUMN customer.addr_zip IS 'CUST-ADDR-ZIP PIC X(10)';
COMMENT ON COLUMN customer.phone_num_1 IS 'CUST-PHONE-NUM-1 PIC X(15)';
COMMENT ON COLUMN customer.phone_num_2 IS 'CUST-PHONE-NUM-2 PIC X(15)';
COMMENT ON COLUMN customer.ssn IS 'CUST-SSN PIC 9(09), zero-padded text';
COMMENT ON COLUMN customer.govt_issued_id IS 'CUST-GOVT-ISSUED-ID PIC X(20)';
COMMENT ON COLUMN customer.dob IS 'CUST-DOB-YYYY-MM-DD PIC X(10)';
COMMENT ON COLUMN customer.eft_account_id IS 'CUST-EFT-ACCOUNT-ID PIC X(10)';
COMMENT ON COLUMN customer.pri_card_holder_ind IS 'CUST-PRI-CARD-HOLDER-IND PIC X(01) Y/N';
COMMENT ON COLUMN customer.fico_credit_score IS 'CUST-FICO-CREDIT-SCORE PIC 9(03)';
COMMENT ON COLUMN customer.version IS 'technical: optimistic lock';

-- ---------------------------------------------------------------------------------------------
-- 2.3 card <- CVACT02Y / CARDDATA.VSAM.KSDS (150 bytes, key CARD-NUM 16 @0; AIX CARDAIX 11 @16)
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS card (
    card_num         CHAR(16)      NOT NULL,
    acct_id          NUMERIC(11,0) NOT NULL,
    cvv_cd           SMALLINT      NOT NULL,
    embossed_name    VARCHAR(50)   NOT NULL,
    expiration_date  DATE,
    active_status    CHAR(1)       NOT NULL,
    version          BIGINT        NOT NULL DEFAULT 0,
    CONSTRAINT pk_card PRIMARY KEY (card_num),
    CONSTRAINT fk_card_account FOREIGN KEY (acct_id) REFERENCES account (acct_id)
        DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT ck_card_card_num CHECK (card_num ~ '^[0-9]{16}$'),
    CONSTRAINT ck_card_cvv_cd CHECK (cvv_cd BETWEEN 0 AND 999),
    CONSTRAINT ck_card_active_status CHECK (active_status IN ('Y', 'N'))
);
CREATE INDEX IF NOT EXISTS ix_card_acct_id ON card (acct_id);
COMMENT ON TABLE  card IS 'CVACT02Y CARD-RECORD; VSAM CARDDATA.VSAM.KSDS (CICS file CARDDAT)';
COMMENT ON INDEX  ix_card_acct_id IS 'AIX CARDDATA.VSAM.AIX (path CARDAIX), key CARD-ACCT-ID 11 @16, non-unique';
COMMENT ON COLUMN card.card_num IS 'CARD-NUM PIC X(16)';
COMMENT ON COLUMN card.acct_id IS 'CARD-ACCT-ID PIC 9(11)';
COMMENT ON COLUMN card.cvv_cd IS 'CARD-CVV-CD PIC 9(03)';
COMMENT ON COLUMN card.embossed_name IS 'CARD-EMBOSSED-NAME PIC X(50)';
COMMENT ON COLUMN card.expiration_date IS 'CARD-EXPIRAION-DATE PIC X(10) YYYY-MM-DD (source misspelling)';
COMMENT ON COLUMN card.active_status IS 'CARD-ACTIVE-STATUS PIC X(01) Y/N';
COMMENT ON COLUMN card.version IS 'technical: optimistic lock';

-- ---------------------------------------------------------------------------------------------
-- 2.4 card_xref <- CVACT03Y / CARDXREF.VSAM.KSDS (50 bytes, key 16 @0; AIX CXACAIX 11 @25)
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS card_xref (
    card_num  CHAR(16)      NOT NULL,
    cust_id   INTEGER       NOT NULL,
    acct_id   NUMERIC(11,0) NOT NULL,
    CONSTRAINT pk_card_xref PRIMARY KEY (card_num),
    CONSTRAINT fk_card_xref_card FOREIGN KEY (card_num) REFERENCES card (card_num)
        DEFERRABLE INITIALLY DEFERRED,
    CONSTRAINT fk_card_xref_customer FOREIGN KEY (cust_id) REFERENCES customer (cust_id)
        DEFERRABLE INITIALLY DEFERRED,
    CONSTRAINT fk_card_xref_account FOREIGN KEY (acct_id) REFERENCES account (acct_id)
        DEFERRABLE INITIALLY DEFERRED
);
CREATE INDEX IF NOT EXISTS ix_card_xref_acct_id ON card_xref (acct_id);
COMMENT ON TABLE  card_xref IS 'CVACT03Y CARD-XREF-RECORD; VSAM CARDXREF.VSAM.KSDS (CICS file CCXREF)';
COMMENT ON INDEX  ix_card_xref_acct_id IS 'AIX CARDXREF.VSAM.AIX (path CXACAIX), key XREF-ACCT-ID 11 @25, non-unique';
COMMENT ON COLUMN card_xref.card_num IS 'XREF-CARD-NUM PIC X(16)';
COMMENT ON COLUMN card_xref.cust_id IS 'XREF-CUST-ID PIC 9(09)';
COMMENT ON COLUMN card_xref.acct_id IS 'XREF-ACCT-ID PIC 9(11)';

-- ---------------------------------------------------------------------------------------------
-- 2.10 transaction_type <- CVTRA03Y / TRANTYPE.VSAM.KSDS (60 bytes, key 2 @0) and DB2 TRANSACTION_TYPE
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS transaction_type (
    type_cd      CHAR(2)     NOT NULL,
    description  VARCHAR(50) NOT NULL,
    version      BIGINT      NOT NULL DEFAULT 0,
    CONSTRAINT pk_transaction_type PRIMARY KEY (type_cd),
    CONSTRAINT ck_transaction_type_type_cd CHECK (type_cd ~ '^[0-9]{2}$')
);
COMMENT ON TABLE  transaction_type IS 'CVTRA03Y TRAN-TYPE-RECORD (VSAM TRANTYPE) = DB2 CARDDEMO.TRANSACTION_TYPE (see db2/transaction_type.sql)';
COMMENT ON COLUMN transaction_type.type_cd IS 'TRAN-TYPE PIC X(02) / DB2 TR_TYPE CHAR(2)';
COMMENT ON COLUMN transaction_type.description IS 'TRAN-TYPE-DESC PIC X(50) / DB2 TR_DESCRIPTION VARCHAR(50)';
COMMENT ON COLUMN transaction_type.version IS 'technical: optimistic lock';

-- ---------------------------------------------------------------------------------------------
-- 2.11 transaction_category <- CVTRA04Y / TRANCATG.VSAM.KSDS (60 bytes, key 6 @0) and DB2 TRANSACTION_TYPE_CATEGORY
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS transaction_category (
    type_cd      CHAR(2)     NOT NULL,
    cat_cd       SMALLINT    NOT NULL,
    description  VARCHAR(50) NOT NULL,
    CONSTRAINT pk_transaction_category PRIMARY KEY (type_cd, cat_cd),
    CONSTRAINT fk_transaction_category_type FOREIGN KEY (type_cd) REFERENCES transaction_type (type_cd)
        ON DELETE RESTRICT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT ck_transaction_category_cat_cd CHECK (cat_cd BETWEEN 0 AND 9999)
);
COMMENT ON TABLE  transaction_category IS 'CVTRA04Y TRAN-CAT-RECORD (VSAM TRANCATG) = DB2 CARDDEMO.TRANSACTION_TYPE_CATEGORY';
COMMENT ON COLUMN transaction_category.type_cd IS 'TRAN-TYPE-CD PIC X(02) / DB2 TRC_TYPE_CODE CHAR(2)';
COMMENT ON COLUMN transaction_category.cat_cd IS 'TRAN-CAT-CD PIC 9(04) / DB2 TRC_TYPE_CATEGORY CHAR(4) numeric string';
COMMENT ON COLUMN transaction_category.description IS 'TRAN-CAT-TYPE-DESC PIC X(50) / DB2 TRC_CAT_DATA VARCHAR(50)';

-- ---------------------------------------------------------------------------------------------
-- 2.9 disclosure_group <- CVTRA02Y / DISCGRP.VSAM.KSDS (50 bytes, key 16 @0)
-- No FK to transaction_category: DISCGRP carries rate rows for type/category pairs that TRANCATG does not define.
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS disclosure_group (
    acct_group_id  VARCHAR(10)  NOT NULL,
    type_cd        CHAR(2)      NOT NULL,
    cat_cd         SMALLINT     NOT NULL,
    int_rate       NUMERIC(6,2) NOT NULL,
    CONSTRAINT pk_disclosure_group PRIMARY KEY (acct_group_id, type_cd, cat_cd),
    CONSTRAINT ck_disclosure_group_cat_cd CHECK (cat_cd BETWEEN 0 AND 9999)
);
COMMENT ON TABLE  disclosure_group IS 'CVTRA02Y DIS-GROUP-RECORD; VSAM DISCGRP.VSAM.KSDS';
COMMENT ON COLUMN disclosure_group.acct_group_id IS 'DIS-ACCT-GROUP-ID PIC X(10); DEFAULT = CBACT04C fallback group';
COMMENT ON COLUMN disclosure_group.type_cd IS 'DIS-TRAN-TYPE-CD PIC X(02)';
COMMENT ON COLUMN disclosure_group.cat_cd IS 'DIS-TRAN-CAT-CD PIC 9(04)';
COMMENT ON COLUMN disclosure_group.int_rate IS 'DIS-INT-RATE PIC S9(04)V99 (annual %)';

-- ---------------------------------------------------------------------------------------------
-- 2.8 tran_cat_balance <- CVTRA01Y / TCATBALF.VSAM.KSDS (50 bytes, key 17 @0)
-- CBTRN02C creates/updates a balance only after the account READ succeeded -> FK to account.
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS tran_cat_balance (
    acct_id  NUMERIC(11,0) NOT NULL,
    type_cd  CHAR(2)       NOT NULL,
    cat_cd   SMALLINT      NOT NULL,
    balance  NUMERIC(11,2) NOT NULL,
    version  BIGINT        NOT NULL DEFAULT 0,
    CONSTRAINT pk_tran_cat_balance PRIMARY KEY (acct_id, type_cd, cat_cd),
    CONSTRAINT fk_tran_cat_balance_account FOREIGN KEY (acct_id) REFERENCES account (acct_id)
        DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT ck_tran_cat_balance_cat_cd CHECK (cat_cd BETWEEN 0 AND 9999)
);
COMMENT ON TABLE  tran_cat_balance IS 'CVTRA01Y TRAN-CAT-BAL-RECORD; VSAM TCATBALF.VSAM.KSDS';
COMMENT ON COLUMN tran_cat_balance.acct_id IS 'TRANCAT-ACCT-ID PIC 9(11)';
COMMENT ON COLUMN tran_cat_balance.type_cd IS 'TRANCAT-TYPE-CD PIC X(02)';
COMMENT ON COLUMN tran_cat_balance.cat_cd IS 'TRANCAT-CD PIC 9(04)';
COMMENT ON COLUMN tran_cat_balance.balance IS 'TRAN-CAT-BAL PIC S9(09)V99';
COMMENT ON COLUMN tran_cat_balance.version IS 'technical: optimistic lock';

-- ---------------------------------------------------------------------------------------------
-- 2.6 transaction <- CVTRA05Y / TRANSACT.VSAM.KSDS (350 bytes, key 16 @0; AIX TRAN-PROC-TS 26 @304)
-- card_num deliberately has no FK (batch may post before card load; contract 2.6).
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS transaction (
    tran_id        CHAR(16)      NOT NULL,
    type_cd        CHAR(2)       NOT NULL,
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
    CONSTRAINT pk_transaction PRIMARY KEY (tran_id),
    CONSTRAINT fk_transaction_type FOREIGN KEY (type_cd) REFERENCES transaction_type (type_cd)
        DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT fk_transaction_category FOREIGN KEY (type_cd, cat_cd) REFERENCES transaction_category (type_cd, cat_cd)
        DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT ck_transaction_merchant_id CHECK (merchant_id BETWEEN 0 AND 999999999)
);
CREATE INDEX IF NOT EXISTS ix_transaction_proc_ts ON transaction (proc_ts);
CREATE INDEX IF NOT EXISTS ix_transaction_card_num ON transaction (card_num);
COMMENT ON TABLE  transaction IS 'CVTRA05Y TRAN-RECORD; VSAM TRANSACT.VSAM.KSDS (CICS file TRANSACT)';
COMMENT ON INDEX  ix_transaction_proc_ts IS 'AIX TRANSACT.VSAM.AIX, key TRAN-PROC-TS 26 @304, non-unique';
COMMENT ON INDEX  ix_transaction_card_num IS 'statement generation (CBSTM03A TRXFL re-key by card)';
COMMENT ON COLUMN transaction.tran_id IS 'TRAN-ID PIC X(16) numeric string';
COMMENT ON COLUMN transaction.type_cd IS 'TRAN-TYPE-CD PIC X(02)';
COMMENT ON COLUMN transaction.cat_cd IS 'TRAN-CAT-CD PIC 9(04)';
COMMENT ON COLUMN transaction.source IS 'TRAN-SOURCE PIC X(10)';
COMMENT ON COLUMN transaction.description IS 'TRAN-DESC PIC X(100)';
COMMENT ON COLUMN transaction.amt IS 'TRAN-AMT PIC S9(09)V99';
COMMENT ON COLUMN transaction.merchant_id IS 'TRAN-MERCHANT-ID PIC 9(09)';
COMMENT ON COLUMN transaction.merchant_name IS 'TRAN-MERCHANT-NAME PIC X(50)';
COMMENT ON COLUMN transaction.merchant_city IS 'TRAN-MERCHANT-CITY PIC X(50)';
COMMENT ON COLUMN transaction.merchant_zip IS 'TRAN-MERCHANT-ZIP PIC X(10)';
COMMENT ON COLUMN transaction.card_num IS 'TRAN-CARD-NUM PIC X(16)';
COMMENT ON COLUMN transaction.orig_ts IS 'TRAN-ORIG-TS PIC X(26)';
COMMENT ON COLUMN transaction.proc_ts IS 'TRAN-PROC-TS PIC X(26)';

-- ---------------------------------------------------------------------------------------------
-- 2.7 daily_transaction <- CVTRA06Y / DALYTRAN.PS (FB 350). Posting staging; no FKs, no de-duplication.
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS daily_transaction (
    run_id         VARCHAR(40)   NOT NULL,
    load_seq       INTEGER       NOT NULL,
    tran_id        CHAR(16)      NOT NULL,
    type_cd        CHAR(2)       NOT NULL,
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
    post_status    CHAR(1),
    reject_reason  SMALLINT,
    CONSTRAINT pk_daily_transaction PRIMARY KEY (run_id, load_seq),
    CONSTRAINT ck_daily_transaction_load_seq CHECK (load_seq > 0),
    CONSTRAINT ck_daily_transaction_post_status CHECK (post_status IN ('P', 'R')),
    CONSTRAINT ck_daily_transaction_reject_reason CHECK (
        (post_status = 'R' AND reject_reason IN (100, 101, 102, 103, 109))
        OR (post_status IS DISTINCT FROM 'R' AND reject_reason IS NULL))
);
COMMENT ON TABLE  daily_transaction IS 'CVTRA06Y DALYTRAN-RECORD; DALYTRAN.PS staging for post-daily-transactions (CBTRN02C)';
COMMENT ON COLUMN daily_transaction.run_id IS 'technical: batch runId that staged the file';
COMMENT ON COLUMN daily_transaction.load_seq IS 'technical: 1-based record order in the input file';
COMMENT ON COLUMN daily_transaction.tran_id IS 'DALYTRAN-ID PIC X(16) (not unique)';
COMMENT ON COLUMN daily_transaction.type_cd IS 'DALYTRAN-TYPE-CD PIC X(02)';
COMMENT ON COLUMN daily_transaction.cat_cd IS 'DALYTRAN-CAT-CD PIC 9(04)';
COMMENT ON COLUMN daily_transaction.source IS 'DALYTRAN-SOURCE PIC X(10)';
COMMENT ON COLUMN daily_transaction.description IS 'DALYTRAN-DESC PIC X(100)';
COMMENT ON COLUMN daily_transaction.amt IS 'DALYTRAN-AMT PIC S9(09)V99';
COMMENT ON COLUMN daily_transaction.merchant_id IS 'DALYTRAN-MERCHANT-ID PIC 9(09)';
COMMENT ON COLUMN daily_transaction.merchant_name IS 'DALYTRAN-MERCHANT-NAME PIC X(50)';
COMMENT ON COLUMN daily_transaction.merchant_city IS 'DALYTRAN-MERCHANT-CITY PIC X(50)';
COMMENT ON COLUMN daily_transaction.merchant_zip IS 'DALYTRAN-MERCHANT-ZIP PIC X(10)';
COMMENT ON COLUMN daily_transaction.card_num IS 'DALYTRAN-CARD-NUM PIC X(16)';
COMMENT ON COLUMN daily_transaction.orig_ts IS 'DALYTRAN-ORIG-TS PIC X(26)';
COMMENT ON COLUMN daily_transaction.proc_ts IS 'DALYTRAN-PROC-TS PIC X(26)';
COMMENT ON COLUMN daily_transaction.post_status IS 'technical: P posted, R rejected, NULL not processed (restart marker)';
COMMENT ON COLUMN daily_transaction.reject_reason IS 'technical: CBTRN02C reject code 100/101/102/103/109';

-- ---------------------------------------------------------------------------------------------
-- Technical tables (no legacy record)
-- ---------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS processed_message (
    message_id    UUID        NOT NULL,
    queue         VARCHAR(80) NOT NULL,
    reply_body    JSONB,
    processed_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT pk_processed_message PRIMARY KEY (message_id)
);
CREATE INDEX IF NOT EXISTS ix_processed_message_processed_at ON processed_message (processed_at);
COMMENT ON TABLE processed_message IS 'technical: SQS consumer de-duplication (messaging.md section 4); purge rows older than 14 days';

CREATE TABLE IF NOT EXISTS batch_job_run (
    run_id         VARCHAR(40) NOT NULL,
    job_name       VARCHAR(64) NOT NULL,
    business_date  DATE,
    status         VARCHAR(16) NOT NULL,
    exit_code      SMALLINT,
    started_at     TIMESTAMPTZ NOT NULL DEFAULT now(),
    ended_at       TIMESTAMPTZ,
    counts         JSONB,
    CONSTRAINT pk_batch_job_run PRIMARY KEY (run_id, job_name)
);
COMMENT ON TABLE batch_job_run IS 'technical: batch idempotency / job-execution record (batch.md section 4)';
