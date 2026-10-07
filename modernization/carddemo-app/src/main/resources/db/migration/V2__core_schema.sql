-- CardDemo core schema: one table per VSAM KSDS / sequential record layout in app/cpy.
-- Design and the field-by-field mapping: docs/modernization/03-data-model.md (copybook-column-map.csv).
-- Type rules: PIC X(n) -> VARCHAR(n) (ADR-0003); PIC S9(n)V9(m) -> NUMERIC(n+m,m) (ADR-0004, no float);
-- PIC 9(n) -> INTEGER (n <= 9) / BIGINT (n <= 18) with a CHECK keeping the PIC width.
-- Every copybook field is NOT NULL (fixed-width records always hold a value; all-space text is '').
-- KSDS key -> PRIMARY KEY, AIX -> index (ADR-0011); level-88 values -> CHECK (ADR-0006);
-- tables rewritten online carry a version column (ADR-0010); GDG outputs -> batch_output_file (ADR-0012).

-- YYYY-MM-DD text -> DATE, NULL when the text is not a valid calendar date. IMMUTABLE so it can feed the
-- generated *_dt columns: the copybook text stays the source of truth because programs compare it as text.
CREATE FUNCTION cobol_iso_date(txt VARCHAR) RETURNS DATE
    LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
    IF txt !~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}$' OR substr(txt, 1, 4) = '0000' THEN
        RETURN NULL;
    END IF;
    RETURN make_date(substr(txt, 1, 4)::INTEGER, substr(txt, 6, 2)::INTEGER, substr(txt, 9, 2)::INTEGER);
EXCEPTION
    WHEN datetime_field_overflow OR invalid_datetime_format THEN
        RETURN NULL;
END;
$$;

-- USRSEC (CSUSR01Y, 80 bytes, KSDS KEYS(8 0))
CREATE TABLE user_security (
    usr_id     VARCHAR(8)  NOT NULL,
    first_name VARCHAR(20) NOT NULL,
    last_name  VARCHAR(20) NOT NULL,
    password   VARCHAR(8)  NOT NULL,
    usr_type   VARCHAR(1)  NOT NULL,
    version    BIGINT      NOT NULL DEFAULT 0,
    CONSTRAINT user_security_pk PRIMARY KEY (usr_id),
    -- COCOM01Y 88 CDEMO-USRTYP-ADMIN 'A' / CDEMO-USRTYP-USER 'U'
    CONSTRAINT user_security_usr_type_ck CHECK (usr_type IN ('A', 'U'))
);

-- CUSTDATA (CVCUS01Y, 500 bytes, KSDS KEYS(9 0))
CREATE TABLE customer (
    cust_id             INTEGER     NOT NULL,
    first_name          VARCHAR(25) NOT NULL,
    middle_name         VARCHAR(25) NOT NULL,
    last_name           VARCHAR(25) NOT NULL,
    addr_line_1         VARCHAR(50) NOT NULL,
    addr_line_2         VARCHAR(50) NOT NULL,
    addr_line_3         VARCHAR(50) NOT NULL,
    addr_state_cd       VARCHAR(2)  NOT NULL,
    addr_country_cd     VARCHAR(3)  NOT NULL,
    addr_zip            VARCHAR(10) NOT NULL,
    phone_num_1         VARCHAR(15) NOT NULL,
    phone_num_2         VARCHAR(15) NOT NULL,
    ssn                 INTEGER     NOT NULL,
    govt_issued_id      VARCHAR(20) NOT NULL,
    dob                 VARCHAR(10) NOT NULL,
    dob_dt              DATE GENERATED ALWAYS AS (cobol_iso_date(dob)) STORED,
    eft_account_id      VARCHAR(10) NOT NULL,
    pri_card_holder_ind VARCHAR(1)  NOT NULL,
    fico_credit_score   INTEGER     NOT NULL,
    version             BIGINT      NOT NULL DEFAULT 0,
    CONSTRAINT customer_pk PRIMARY KEY (cust_id),
    CONSTRAINT customer_cust_id_ck CHECK (cust_id BETWEEN 0 AND 999999999),
    CONSTRAINT customer_ssn_ck CHECK (ssn BETWEEN 0 AND 999999999),
    CONSTRAINT customer_fico_credit_score_ck CHECK (fico_credit_score BETWEEN 0 AND 999),
    -- COACTUPC 88 FLG-PRI-CARDHOLDER-ISVALID VALUES 'Y', 'N'
    CONSTRAINT customer_pri_card_holder_ind_ck CHECK (pri_card_holder_ind IN ('Y', 'N'))
);

-- ACCTDATA (CVACT01Y, 300 bytes, KSDS KEYS(11 0))
CREATE TABLE account (
    acct_id            BIGINT        NOT NULL,
    active_status      VARCHAR(1)    NOT NULL,
    curr_bal           NUMERIC(12,2) NOT NULL,
    credit_limit       NUMERIC(12,2) NOT NULL,
    cash_credit_limit  NUMERIC(12,2) NOT NULL,
    open_date          VARCHAR(10)   NOT NULL,
    open_date_dt       DATE GENERATED ALWAYS AS (cobol_iso_date(open_date)) STORED,
    expiration_date    VARCHAR(10)   NOT NULL,
    expiration_date_dt DATE GENERATED ALWAYS AS (cobol_iso_date(expiration_date)) STORED,
    reissue_date       VARCHAR(10)   NOT NULL,
    reissue_date_dt    DATE GENERATED ALWAYS AS (cobol_iso_date(reissue_date)) STORED,
    curr_cyc_credit    NUMERIC(12,2) NOT NULL,
    curr_cyc_debit     NUMERIC(12,2) NOT NULL,
    addr_zip           VARCHAR(10)   NOT NULL,
    group_id           VARCHAR(10)   NOT NULL,
    version            BIGINT        NOT NULL DEFAULT 0,
    CONSTRAINT account_pk PRIMARY KEY (acct_id),
    CONSTRAINT account_acct_id_ck CHECK (acct_id BETWEEN 0 AND 99999999999),
    -- COACTUPC 88 FLG-ACCT-STATUS-ISVALID VALUES 'Y', 'N'
    CONSTRAINT account_active_status_ck CHECK (active_status IN ('Y', 'N'))
);

-- CARDDATA (CVACT02Y, 150 bytes, KSDS KEYS(16 0); AIX CARDAIX KEYS(11 16) NONUNIQUEKEY)
CREATE TABLE card (
    card_num           VARCHAR(16) NOT NULL,
    acct_id            BIGINT      NOT NULL,
    cvv_cd             INTEGER     NOT NULL,
    embossed_name      VARCHAR(50) NOT NULL,
    expiration_date    VARCHAR(10) NOT NULL,
    expiration_date_dt DATE GENERATED ALWAYS AS (cobol_iso_date(expiration_date)) STORED,
    active_status      VARCHAR(1)  NOT NULL,
    version            BIGINT      NOT NULL DEFAULT 0,
    CONSTRAINT card_pk PRIMARY KEY (card_num),
    CONSTRAINT card_acct_id_ck CHECK (acct_id BETWEEN 0 AND 99999999999),
    CONSTRAINT card_cvv_cd_ck CHECK (cvv_cd BETWEEN 0 AND 999),
    -- COCRDUPC 88 CARD-STATUS-MUST-BE-YES-NO: 'Y' / 'N'
    CONSTRAINT card_active_status_ck CHECK (active_status IN ('Y', 'N'))
);
CREATE INDEX card_acct_id_ix ON card (acct_id, card_num);

-- CARDXREF / CCXREF (CVACT03Y, 50 bytes, KSDS KEYS(16 0); AIX CXACAIX KEYS(11 25)): card <-> customer <-> account.
-- FKs are DEFERRABLE INITIALLY DEFERRED so a dataset refresh (delete + reload of card/customer/account) runs in one
-- transaction and the references are checked at commit.
CREATE TABLE card_xref (
    card_num VARCHAR(16) NOT NULL,
    cust_id  INTEGER     NOT NULL,
    acct_id  BIGINT      NOT NULL,
    CONSTRAINT card_xref_pk PRIMARY KEY (card_num),
    CONSTRAINT card_xref_cust_id_ck CHECK (cust_id BETWEEN 0 AND 999999999),
    CONSTRAINT card_xref_acct_id_ck CHECK (acct_id BETWEEN 0 AND 99999999999),
    CONSTRAINT card_xref_card_fk FOREIGN KEY (card_num) REFERENCES card (card_num)
        DEFERRABLE INITIALLY DEFERRED,
    CONSTRAINT card_xref_customer_fk FOREIGN KEY (cust_id) REFERENCES customer (cust_id)
        DEFERRABLE INITIALLY DEFERRED,
    CONSTRAINT card_xref_account_fk FOREIGN KEY (acct_id) REFERENCES account (acct_id)
        DEFERRABLE INITIALLY DEFERRED
);
CREATE INDEX card_xref_acct_id_ix ON card_xref (acct_id, card_num);
CREATE INDEX card_xref_cust_id_ix ON card_xref (cust_id);

-- TRANTYPE (CVTRA03Y, 60 bytes, KSDS KEYS(2 0))
CREATE TABLE transaction_type (
    tran_type_cd VARCHAR(2)  NOT NULL,
    description  VARCHAR(50) NOT NULL,
    CONSTRAINT transaction_type_pk PRIMARY KEY (tran_type_cd)
);

-- TRANCATG (CVTRA04Y, 60 bytes, KSDS KEYS(6 0) = type + category)
CREATE TABLE transaction_category (
    tran_type_cd VARCHAR(2)  NOT NULL,
    tran_cat_cd  INTEGER     NOT NULL,
    description  VARCHAR(50) NOT NULL,
    CONSTRAINT transaction_category_pk PRIMARY KEY (tran_type_cd, tran_cat_cd),
    CONSTRAINT transaction_category_tran_cat_cd_ck CHECK (tran_cat_cd BETWEEN 0 AND 9999),
    CONSTRAINT transaction_category_type_fk FOREIGN KEY (tran_type_cd) REFERENCES transaction_type (tran_type_cd)
        DEFERRABLE INITIALLY DEFERRED
);

-- DISCGRP (CVTRA02Y, 50 bytes, KSDS KEYS(16 0) = group + type + category). No FK from account.group_id:
-- CBACT04C falls back to group 'DEFAULT' when the account's group has no row.
CREATE TABLE disclosure_group (
    acct_group_id VARCHAR(10)  NOT NULL,
    tran_type_cd  VARCHAR(2)   NOT NULL,
    tran_cat_cd   INTEGER      NOT NULL,
    int_rate      NUMERIC(6,2) NOT NULL,
    CONSTRAINT disclosure_group_pk PRIMARY KEY (acct_group_id, tran_type_cd, tran_cat_cd),
    CONSTRAINT disclosure_group_tran_cat_cd_ck CHECK (tran_cat_cd BETWEEN 0 AND 9999)
);

-- TCATBALF (CVTRA01Y, 50 bytes, KSDS KEYS(17 0) = account + type + category). No FK to account: TCATBALF and
-- ACCTDATA are refreshed by separate jobs, and CBACT04C reads TCATBALF sequentially without requiring the account.
CREATE TABLE tran_cat_balance (
    acct_id      BIGINT        NOT NULL,
    tran_type_cd VARCHAR(2)    NOT NULL,
    tran_cat_cd  INTEGER       NOT NULL,
    balance      NUMERIC(11,2) NOT NULL,
    CONSTRAINT tran_cat_balance_pk PRIMARY KEY (acct_id, tran_type_cd, tran_cat_cd),
    CONSTRAINT tran_cat_balance_acct_id_ck CHECK (acct_id BETWEEN 0 AND 99999999999),
    CONSTRAINT tran_cat_balance_tran_cat_cd_ck CHECK (tran_cat_cd BETWEEN 0 AND 9999)
);

-- TRANSACT (CVTRA05Y, 350 bytes, KSDS KEYS(16 0); AIX KEYS(26 304) NONUNIQUEKEY on TRAN-PROC-TS).
-- Insert-only online (COTRN02C, COBIL00C), so no version column. No FK on card_num / type / category:
-- COTRN02C and CBTRN02C write them without checking TRANCATG.
CREATE TABLE transaction (
    tran_id       VARCHAR(16)   NOT NULL,
    tran_type_cd  VARCHAR(2)    NOT NULL,
    tran_cat_cd   INTEGER       NOT NULL,
    source        VARCHAR(10)   NOT NULL,
    description   VARCHAR(100)  NOT NULL,
    amount        NUMERIC(11,2) NOT NULL,
    merchant_id   INTEGER       NOT NULL,
    merchant_name VARCHAR(50)   NOT NULL,
    merchant_city VARCHAR(50)   NOT NULL,
    merchant_zip  VARCHAR(10)   NOT NULL,
    card_num      VARCHAR(16)   NOT NULL,
    orig_ts       VARCHAR(26)   NOT NULL,
    proc_ts       VARCHAR(26)   NOT NULL,
    CONSTRAINT transaction_pk PRIMARY KEY (tran_id),
    CONSTRAINT transaction_tran_cat_cd_ck CHECK (tran_cat_cd BETWEEN 0 AND 9999),
    CONSTRAINT transaction_merchant_id_ck CHECK (merchant_id BETWEEN 0 AND 999999999)
);
CREATE INDEX transaction_proc_ts_ix ON transaction (proc_ts, tran_id);

-- DALYTRAN (CVTRA06Y, 350 bytes, sequential input of POSTTRAN). Staging copy of the current daily file
-- (ADR-0011: the batch job still reads the file); no FKs, invalid rows are POSTTRAN rejects, not load errors.
-- A sequential file has no unique key, so rows are keyed by their 1-based position in the file (record_seq).
CREATE TABLE daily_transaction (
    record_seq    INTEGER       NOT NULL,
    tran_id       VARCHAR(16)   NOT NULL,
    tran_type_cd  VARCHAR(2)    NOT NULL,
    tran_cat_cd   INTEGER       NOT NULL,
    source        VARCHAR(10)   NOT NULL,
    description   VARCHAR(100)  NOT NULL,
    amount        NUMERIC(11,2) NOT NULL,
    merchant_id   INTEGER       NOT NULL,
    merchant_name VARCHAR(50)   NOT NULL,
    merchant_city VARCHAR(50)   NOT NULL,
    merchant_zip  VARCHAR(10)   NOT NULL,
    card_num      VARCHAR(16)   NOT NULL,
    orig_ts       VARCHAR(26)   NOT NULL,
    proc_ts       VARCHAR(26)   NOT NULL,
    CONSTRAINT daily_transaction_pk PRIMARY KEY (record_seq),
    CONSTRAINT daily_transaction_record_seq_ck CHECK (record_seq > 0),
    CONSTRAINT daily_transaction_tran_cat_cd_ck CHECK (tran_cat_cd BETWEEN 0 AND 9999),
    CONSTRAINT daily_transaction_merchant_id_ck CHECK (merchant_id BETWEEN 0 AND 999999999)
);
CREATE INDEX daily_transaction_tran_id_ix ON daily_transaction (tran_id);

-- GDG replacement (ADR-0012): every (+1) generation is a dated file under carddemo.batch.output-dir,
-- catalogued here so (0) / (-1) resolve by query and a restart re-reads the generation it recorded.
CREATE TABLE batch_output_file (
    output_file_id   BIGINT GENERATED ALWAYS AS IDENTITY,
    gdg_base         VARCHAR(44)   NOT NULL,
    business_date    DATE          NOT NULL,
    job_execution_id BIGINT        NOT NULL,
    file_path        VARCHAR(1024) NOT NULL,
    record_count     BIGINT        NOT NULL,
    sha256           VARCHAR(64)   NOT NULL,
    created_at       TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT batch_output_file_pk PRIMARY KEY (output_file_id),
    CONSTRAINT batch_output_file_generation_uk UNIQUE (gdg_base, business_date, job_execution_id),
    CONSTRAINT batch_output_file_job_execution_fk FOREIGN KEY (job_execution_id)
        REFERENCES BATCH_JOB_EXECUTION (JOB_EXECUTION_ID),
    CONSTRAINT batch_output_file_record_count_ck CHECK (record_count >= 0)
);
CREATE INDEX batch_output_file_generation_ix
    ON batch_output_file (gdg_base, business_date DESC, job_execution_id DESC);

COMMENT ON FUNCTION cobol_iso_date(VARCHAR) IS 'YYYY-MM-DD text to DATE; NULL when not a valid date';
COMMENT ON TABLE batch_output_file IS 'GDG generations written as dated files (ADR-0012)';
COMMENT ON COLUMN batch_output_file.gdg_base IS 'GDG base without the AWS.M2.CARDDEMO. prefix';
COMMENT ON COLUMN batch_output_file.file_path IS '<output-dir>/<gdg_base>/<gdg_base>.<business_date>.<job_execution_id>';

-- Copybook names (ADR-0011): generated from db/copybook-column-map.csv, checked by CoreSchemaIT.
COMMENT ON TABLE user_security IS 'USRSEC (CSUSR01Y)';
COMMENT ON TABLE customer IS 'CUSTDATA (CVCUS01Y)';
COMMENT ON TABLE account IS 'ACCTDATA (CVACT01Y)';
COMMENT ON TABLE card IS 'CARDDATA (CVACT02Y)';
COMMENT ON TABLE card_xref IS 'CARDXREF (CVACT03Y)';
COMMENT ON TABLE transaction IS 'TRANSACT (CVTRA05Y)';
COMMENT ON TABLE daily_transaction IS 'DALYTRAN (CVTRA06Y)';
COMMENT ON TABLE tran_cat_balance IS 'TCATBALF (CVTRA01Y)';
COMMENT ON TABLE disclosure_group IS 'DISCGRP (CVTRA02Y)';
COMMENT ON TABLE transaction_type IS 'TRANTYPE (CVTRA03Y)';
COMMENT ON TABLE transaction_category IS 'TRANCATG (CVTRA04Y)';
COMMENT ON COLUMN user_security.usr_id IS 'SEC-USR-ID';
COMMENT ON COLUMN user_security.first_name IS 'SEC-USR-FNAME';
COMMENT ON COLUMN user_security.last_name IS 'SEC-USR-LNAME';
COMMENT ON COLUMN user_security.password IS 'SEC-USR-PWD';
COMMENT ON COLUMN user_security.usr_type IS 'SEC-USR-TYPE';
COMMENT ON COLUMN customer.cust_id IS 'CUST-ID';
COMMENT ON COLUMN customer.first_name IS 'CUST-FIRST-NAME';
COMMENT ON COLUMN customer.middle_name IS 'CUST-MIDDLE-NAME';
COMMENT ON COLUMN customer.last_name IS 'CUST-LAST-NAME';
COMMENT ON COLUMN customer.addr_line_1 IS 'CUST-ADDR-LINE-1';
COMMENT ON COLUMN customer.addr_line_2 IS 'CUST-ADDR-LINE-2';
COMMENT ON COLUMN customer.addr_line_3 IS 'CUST-ADDR-LINE-3';
COMMENT ON COLUMN customer.addr_state_cd IS 'CUST-ADDR-STATE-CD';
COMMENT ON COLUMN customer.addr_country_cd IS 'CUST-ADDR-COUNTRY-CD';
COMMENT ON COLUMN customer.addr_zip IS 'CUST-ADDR-ZIP';
COMMENT ON COLUMN customer.phone_num_1 IS 'CUST-PHONE-NUM-1';
COMMENT ON COLUMN customer.phone_num_2 IS 'CUST-PHONE-NUM-2';
COMMENT ON COLUMN customer.ssn IS 'CUST-SSN';
COMMENT ON COLUMN customer.govt_issued_id IS 'CUST-GOVT-ISSUED-ID';
COMMENT ON COLUMN customer.dob IS 'CUST-DOB-YYYY-MM-DD';
COMMENT ON COLUMN customer.eft_account_id IS 'CUST-EFT-ACCOUNT-ID';
COMMENT ON COLUMN customer.pri_card_holder_ind IS 'CUST-PRI-CARD-HOLDER-IND';
COMMENT ON COLUMN customer.fico_credit_score IS 'CUST-FICO-CREDIT-SCORE';
COMMENT ON COLUMN account.acct_id IS 'ACCT-ID';
COMMENT ON COLUMN account.active_status IS 'ACCT-ACTIVE-STATUS';
COMMENT ON COLUMN account.curr_bal IS 'ACCT-CURR-BAL';
COMMENT ON COLUMN account.credit_limit IS 'ACCT-CREDIT-LIMIT';
COMMENT ON COLUMN account.cash_credit_limit IS 'ACCT-CASH-CREDIT-LIMIT';
COMMENT ON COLUMN account.open_date IS 'ACCT-OPEN-DATE';
COMMENT ON COLUMN account.expiration_date IS 'ACCT-EXPIRAION-DATE';
COMMENT ON COLUMN account.reissue_date IS 'ACCT-REISSUE-DATE';
COMMENT ON COLUMN account.curr_cyc_credit IS 'ACCT-CURR-CYC-CREDIT';
COMMENT ON COLUMN account.curr_cyc_debit IS 'ACCT-CURR-CYC-DEBIT';
COMMENT ON COLUMN account.addr_zip IS 'ACCT-ADDR-ZIP';
COMMENT ON COLUMN account.group_id IS 'ACCT-GROUP-ID';
COMMENT ON COLUMN card.card_num IS 'CARD-NUM';
COMMENT ON COLUMN card.acct_id IS 'CARD-ACCT-ID';
COMMENT ON COLUMN card.cvv_cd IS 'CARD-CVV-CD';
COMMENT ON COLUMN card.embossed_name IS 'CARD-EMBOSSED-NAME';
COMMENT ON COLUMN card.expiration_date IS 'CARD-EXPIRAION-DATE';
COMMENT ON COLUMN card.active_status IS 'CARD-ACTIVE-STATUS';
COMMENT ON COLUMN card_xref.card_num IS 'XREF-CARD-NUM';
COMMENT ON COLUMN card_xref.cust_id IS 'XREF-CUST-ID';
COMMENT ON COLUMN card_xref.acct_id IS 'XREF-ACCT-ID';
COMMENT ON COLUMN transaction.tran_id IS 'TRAN-ID';
COMMENT ON COLUMN transaction.tran_type_cd IS 'TRAN-TYPE-CD';
COMMENT ON COLUMN transaction.tran_cat_cd IS 'TRAN-CAT-CD';
COMMENT ON COLUMN transaction.source IS 'TRAN-SOURCE';
COMMENT ON COLUMN transaction.description IS 'TRAN-DESC';
COMMENT ON COLUMN transaction.amount IS 'TRAN-AMT';
COMMENT ON COLUMN transaction.merchant_id IS 'TRAN-MERCHANT-ID';
COMMENT ON COLUMN transaction.merchant_name IS 'TRAN-MERCHANT-NAME';
COMMENT ON COLUMN transaction.merchant_city IS 'TRAN-MERCHANT-CITY';
COMMENT ON COLUMN transaction.merchant_zip IS 'TRAN-MERCHANT-ZIP';
COMMENT ON COLUMN transaction.card_num IS 'TRAN-CARD-NUM';
COMMENT ON COLUMN transaction.orig_ts IS 'TRAN-ORIG-TS';
COMMENT ON COLUMN transaction.proc_ts IS 'TRAN-PROC-TS';
COMMENT ON COLUMN daily_transaction.tran_id IS 'DALYTRAN-ID';
COMMENT ON COLUMN daily_transaction.tran_type_cd IS 'DALYTRAN-TYPE-CD';
COMMENT ON COLUMN daily_transaction.tran_cat_cd IS 'DALYTRAN-CAT-CD';
COMMENT ON COLUMN daily_transaction.source IS 'DALYTRAN-SOURCE';
COMMENT ON COLUMN daily_transaction.description IS 'DALYTRAN-DESC';
COMMENT ON COLUMN daily_transaction.amount IS 'DALYTRAN-AMT';
COMMENT ON COLUMN daily_transaction.merchant_id IS 'DALYTRAN-MERCHANT-ID';
COMMENT ON COLUMN daily_transaction.merchant_name IS 'DALYTRAN-MERCHANT-NAME';
COMMENT ON COLUMN daily_transaction.merchant_city IS 'DALYTRAN-MERCHANT-CITY';
COMMENT ON COLUMN daily_transaction.merchant_zip IS 'DALYTRAN-MERCHANT-ZIP';
COMMENT ON COLUMN daily_transaction.card_num IS 'DALYTRAN-CARD-NUM';
COMMENT ON COLUMN daily_transaction.orig_ts IS 'DALYTRAN-ORIG-TS';
COMMENT ON COLUMN daily_transaction.proc_ts IS 'DALYTRAN-PROC-TS';
COMMENT ON COLUMN tran_cat_balance.acct_id IS 'TRANCAT-ACCT-ID';
COMMENT ON COLUMN tran_cat_balance.tran_type_cd IS 'TRANCAT-TYPE-CD';
COMMENT ON COLUMN tran_cat_balance.tran_cat_cd IS 'TRANCAT-CD';
COMMENT ON COLUMN tran_cat_balance.balance IS 'TRAN-CAT-BAL';
COMMENT ON COLUMN disclosure_group.acct_group_id IS 'DIS-ACCT-GROUP-ID';
COMMENT ON COLUMN disclosure_group.tran_type_cd IS 'DIS-TRAN-TYPE-CD';
COMMENT ON COLUMN disclosure_group.tran_cat_cd IS 'DIS-TRAN-CAT-CD';
COMMENT ON COLUMN disclosure_group.int_rate IS 'DIS-INT-RATE';
COMMENT ON COLUMN transaction_type.tran_type_cd IS 'TRAN-TYPE';
COMMENT ON COLUMN transaction_type.description IS 'TRAN-TYPE-DESC';
COMMENT ON COLUMN transaction_category.tran_type_cd IS 'TRAN-TYPE-CD';
COMMENT ON COLUMN transaction_category.tran_cat_cd IS 'TRAN-CAT-CD';
COMMENT ON COLUMN transaction_category.description IS 'TRAN-CAT-TYPE-DESC';

-- Derived columns (no copybook field of their own)
COMMENT ON COLUMN customer.dob_dt IS 'cobol_iso_date(CUST-DOB-YYYY-MM-DD)';
COMMENT ON COLUMN account.open_date_dt IS 'cobol_iso_date(ACCT-OPEN-DATE)';
COMMENT ON COLUMN account.expiration_date_dt IS 'cobol_iso_date(ACCT-EXPIRAION-DATE)';
COMMENT ON COLUMN account.reissue_date_dt IS 'cobol_iso_date(ACCT-REISSUE-DATE)';
COMMENT ON COLUMN daily_transaction.record_seq IS '1-based record position in the DALYTRAN file';
COMMENT ON COLUMN card.expiration_date_dt IS 'cobol_iso_date(CARD-EXPIRAION-DATE)';
COMMENT ON COLUMN user_security.version IS 'optimistic lock (ADR-0010): COUSR02C/COUSR03C';
COMMENT ON COLUMN customer.version IS 'optimistic lock (ADR-0010): COACTUPC';
COMMENT ON COLUMN account.version IS 'optimistic lock (ADR-0010): COACTUPC, COBIL00C';
COMMENT ON COLUMN card.version IS 'optimistic lock (ADR-0010): COCRDUPC';
