-- IMS HIDAM database DBPAUTP0 (app/app-authorization-ims-db2-mq/ims/DBPAUTP0.dbd) -> relational tables
-- (contract data-model.md 3.2 / 3.3). REPLATFORM CANDIDATE (migration-inventory.md section 9): the programs
-- that navigate this database (COPAUA0C, COPAUS0C/1C, CBPAUP0C) stay on the M2 runtime unless the
-- online-services session refactors them; these tables are the fixed target if they do.
--
-- Hierarchy mapping:
--   root  PAUTSUM0 (100 bytes, CIPAUSMY, key ACCNTID = PA-ACCT-ID S9(11) COMP-3)  -> pending_auth_summary
--   child PAUTDTL1 (200 bytes, CIPAUDTY, key PAUT9CTS = PA-AUTH-DATE-9C + PA-AUTH-TIME-9C, COMP-3)
--                                                                                 -> pending_auth_detail
--   parentage (child under root)     -> pending_auth_detail.acct_id FK ON DELETE CASCADE
--                                       (DL/I DLET of a root deletes its dependents)
--   index DB DBPAUTX0 (PAUTINDX on ACCNTID) -> covered by pk_pending_auth_summary
--   PA-ACCOUNT-STATUS X(02) OCCURS 5 -> CHAR(2)[] (max 5 elements)
--   PAUT9CTS descending-order trick (9's complement of date/time) is kept verbatim so GU/GNP order
--   (newest first) == ORDER BY auth_date_9c, auth_time_9c.
-- Idempotent. Requires schema.sql.

CREATE TABLE IF NOT EXISTS carddemo.pending_auth_summary (
    acct_id            NUMERIC(11,0) NOT NULL,
    cust_id            INTEGER       NOT NULL,
    auth_status        CHAR(1),
    account_status     CHAR(2)[],
    credit_limit       NUMERIC(11,2) NOT NULL,
    cash_limit         NUMERIC(11,2) NOT NULL,
    credit_balance     NUMERIC(11,2) NOT NULL,
    cash_balance       NUMERIC(11,2) NOT NULL,
    approved_auth_cnt  SMALLINT      NOT NULL,
    declined_auth_cnt  SMALLINT      NOT NULL,
    approved_auth_amt  NUMERIC(11,2) NOT NULL,
    declined_auth_amt  NUMERIC(11,2) NOT NULL,
    version            BIGINT        NOT NULL DEFAULT 0,
    CONSTRAINT pk_pending_auth_summary PRIMARY KEY (acct_id),
    CONSTRAINT ck_pending_auth_summary_account_status CHECK (cardinality(account_status) <= 5)
);
COMMENT ON TABLE  carddemo.pending_auth_summary IS 'IMS DBPAUTP0 root segment PAUTSUM0 (CIPAUSMY); replatform candidate';
COMMENT ON COLUMN carddemo.pending_auth_summary.acct_id IS 'PA-ACCT-ID PIC S9(11) COMP-3 (IMS key ACCNTID)';
COMMENT ON COLUMN carddemo.pending_auth_summary.cust_id IS 'PA-CUST-ID PIC 9(09)';
COMMENT ON COLUMN carddemo.pending_auth_summary.auth_status IS 'PA-AUTH-STATUS PIC X(01)';
COMMENT ON COLUMN carddemo.pending_auth_summary.account_status IS 'PA-ACCOUNT-STATUS PIC X(02) OCCURS 5 (blank element -> NULL)';
COMMENT ON COLUMN carddemo.pending_auth_summary.credit_limit IS 'PA-CREDIT-LIMIT PIC S9(09)V99 COMP-3';
COMMENT ON COLUMN carddemo.pending_auth_summary.cash_limit IS 'PA-CASH-LIMIT PIC S9(09)V99 COMP-3';
COMMENT ON COLUMN carddemo.pending_auth_summary.credit_balance IS 'PA-CREDIT-BALANCE PIC S9(09)V99 COMP-3';
COMMENT ON COLUMN carddemo.pending_auth_summary.cash_balance IS 'PA-CASH-BALANCE PIC S9(09)V99 COMP-3';
COMMENT ON COLUMN carddemo.pending_auth_summary.approved_auth_cnt IS 'PA-APPROVED-AUTH-CNT PIC S9(04) COMP';
COMMENT ON COLUMN carddemo.pending_auth_summary.declined_auth_cnt IS 'PA-DECLINED-AUTH-CNT PIC S9(04) COMP';
COMMENT ON COLUMN carddemo.pending_auth_summary.approved_auth_amt IS 'PA-APPROVED-AUTH-AMT PIC S9(09)V99 COMP-3';
COMMENT ON COLUMN carddemo.pending_auth_summary.declined_auth_amt IS 'PA-DECLINED-AUTH-AMT PIC S9(09)V99 COMP-3';
COMMENT ON COLUMN carddemo.pending_auth_summary.version IS 'technical: optimistic lock';

CREATE TABLE IF NOT EXISTS carddemo.pending_auth_detail (
    acct_id                 NUMERIC(11,0) NOT NULL,
    auth_date_9c            INTEGER       NOT NULL,
    auth_time_9c            BIGINT        NOT NULL,
    auth_orig_date          CHAR(6),
    auth_orig_time          CHAR(6),
    card_num                CHAR(16)      NOT NULL,
    auth_type               CHAR(4),
    card_expiry_date        CHAR(4),
    message_type            CHAR(6),
    message_source          CHAR(6),
    auth_id_code            CHAR(6),
    auth_resp_code          CHAR(2),
    auth_resp_reason        CHAR(4),
    processing_code         INTEGER,
    transaction_amt         NUMERIC(12,2) NOT NULL,
    approved_amt            NUMERIC(12,2) NOT NULL,
    merchant_category_code  CHAR(4),
    acqr_country_code       CHAR(3),
    pos_entry_mode          SMALLINT,
    merchant_id             VARCHAR(15),
    merchant_name           VARCHAR(22),
    merchant_city           VARCHAR(13),
    merchant_state          CHAR(2),
    merchant_zip            VARCHAR(9),
    transaction_id          VARCHAR(15),
    match_status            CHAR(1),
    auth_fraud              CHAR(1),
    fraud_rpt_date          CHAR(8),
    CONSTRAINT pk_pending_auth_detail PRIMARY KEY (acct_id, auth_date_9c, auth_time_9c),
    CONSTRAINT fk_pending_auth_detail_summary FOREIGN KEY (acct_id)
        REFERENCES carddemo.pending_auth_summary (acct_id) ON DELETE CASCADE DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT ck_pending_auth_detail_match_status CHECK (match_status IN ('P', 'D', 'E', 'M')),
    CONSTRAINT ck_pending_auth_detail_auth_fraud CHECK (auth_fraud IN ('F', 'R'))
);
CREATE INDEX IF NOT EXISTS ix_pending_auth_detail_card ON carddemo.pending_auth_detail (card_num);
COMMENT ON TABLE  carddemo.pending_auth_detail IS 'IMS DBPAUTP0 child segment PAUTDTL1 (CIPAUDTY); replatform candidate';
COMMENT ON COLUMN carddemo.pending_auth_detail.acct_id IS 'parent PAUTSUM0 key PA-ACCT-ID';
COMMENT ON COLUMN carddemo.pending_auth_detail.auth_date_9c IS 'PA-AUTH-DATE-9C PIC S9(05) COMP-3 (99999 - YYDDD)';
COMMENT ON COLUMN carddemo.pending_auth_detail.auth_time_9c IS 'PA-AUTH-TIME-9C PIC S9(09) COMP-3 (999999999 - HHMMSSmmm)';
COMMENT ON COLUMN carddemo.pending_auth_detail.auth_orig_date IS 'PA-AUTH-ORIG-DATE PIC X(06) YYMMDD';
COMMENT ON COLUMN carddemo.pending_auth_detail.auth_orig_time IS 'PA-AUTH-ORIG-TIME PIC X(06) HHMMSS';
COMMENT ON COLUMN carddemo.pending_auth_detail.card_num IS 'PA-CARD-NUM PIC X(16)';
COMMENT ON COLUMN carddemo.pending_auth_detail.auth_type IS 'PA-AUTH-TYPE PIC X(04)';
COMMENT ON COLUMN carddemo.pending_auth_detail.card_expiry_date IS 'PA-CARD-EXPIRY-DATE PIC X(04) MMYY';
COMMENT ON COLUMN carddemo.pending_auth_detail.message_type IS 'PA-MESSAGE-TYPE PIC X(06)';
COMMENT ON COLUMN carddemo.pending_auth_detail.message_source IS 'PA-MESSAGE-SOURCE PIC X(06)';
COMMENT ON COLUMN carddemo.pending_auth_detail.auth_id_code IS 'PA-AUTH-ID-CODE PIC X(06)';
COMMENT ON COLUMN carddemo.pending_auth_detail.auth_resp_code IS 'PA-AUTH-RESP-CODE PIC X(02): 00 approved, 05 declined';
COMMENT ON COLUMN carddemo.pending_auth_detail.auth_resp_reason IS 'PA-AUTH-RESP-REASON PIC X(04)';
COMMENT ON COLUMN carddemo.pending_auth_detail.processing_code IS 'PA-PROCESSING-CODE PIC 9(06)';
COMMENT ON COLUMN carddemo.pending_auth_detail.transaction_amt IS 'PA-TRANSACTION-AMT PIC S9(10)V99 COMP-3';
COMMENT ON COLUMN carddemo.pending_auth_detail.approved_amt IS 'PA-APPROVED-AMT PIC S9(10)V99 COMP-3';
COMMENT ON COLUMN carddemo.pending_auth_detail.merchant_category_code IS 'PA-MERCHANT-CATAGORY-CODE PIC X(04) (source misspelling)';
COMMENT ON COLUMN carddemo.pending_auth_detail.acqr_country_code IS 'PA-ACQR-COUNTRY-CODE PIC X(03)';
COMMENT ON COLUMN carddemo.pending_auth_detail.pos_entry_mode IS 'PA-POS-ENTRY-MODE PIC 9(02)';
COMMENT ON COLUMN carddemo.pending_auth_detail.merchant_id IS 'PA-MERCHANT-ID PIC X(15)';
COMMENT ON COLUMN carddemo.pending_auth_detail.merchant_name IS 'PA-MERCHANT-NAME PIC X(22)';
COMMENT ON COLUMN carddemo.pending_auth_detail.merchant_city IS 'PA-MERCHANT-CITY PIC X(13)';
COMMENT ON COLUMN carddemo.pending_auth_detail.merchant_state IS 'PA-MERCHANT-STATE PIC X(02)';
COMMENT ON COLUMN carddemo.pending_auth_detail.merchant_zip IS 'PA-MERCHANT-ZIP PIC X(09)';
COMMENT ON COLUMN carddemo.pending_auth_detail.transaction_id IS 'PA-TRANSACTION-ID PIC X(15)';
COMMENT ON COLUMN carddemo.pending_auth_detail.match_status IS 'PA-MATCH-STATUS PIC X(01): P/D/E/M';
COMMENT ON COLUMN carddemo.pending_auth_detail.auth_fraud IS 'PA-AUTH-FRAUD PIC X(01): F/R';
COMMENT ON COLUMN carddemo.pending_auth_detail.fraud_rpt_date IS 'PA-FRAUD-RPT-DATE PIC X(08)';
