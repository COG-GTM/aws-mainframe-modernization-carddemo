-- Optional module app-authorization-ims-db2-mq: IMS HIDAM database DBPAUTP0
-- (root PAUTSUM0 = CIPAUSMY, child PAUTDTL1 = CIPAUDTY). COMP-3 amounts become NUMERIC.

CREATE TABLE pending_auth_summary (
    acct_id            BIGINT        NOT NULL PRIMARY KEY,  -- PA-ACCT-ID S9(11) COMP-3
    cust_id            BIGINT,                              -- PA-CUST-ID 9(09)
    auth_status        VARCHAR(1),                          -- PA-AUTH-STATUS
    credit_limit       NUMERIC(11,2),                       -- PA-CREDIT-LIMIT S9(09)V99 COMP-3
    cash_limit         NUMERIC(11,2),                       -- PA-CASH-LIMIT
    credit_balance     NUMERIC(11,2),                       -- PA-CREDIT-BALANCE
    cash_balance       NUMERIC(11,2),                       -- PA-CASH-BALANCE
    approved_auth_cnt  INTEGER,                             -- PA-APPROVED-AUTH-CNT S9(04) COMP
    declined_auth_cnt  INTEGER,                             -- PA-DECLINED-AUTH-CNT S9(04) COMP
    approved_auth_amt  NUMERIC(11,2),                       -- PA-APPROVED-AUTH-AMT
    declined_auth_amt  NUMERIC(11,2)                        -- PA-DECLINED-AUTH-AMT
);

-- PA-ACCOUNT-STATUS PIC X(02) OCCURS 5 TIMES
CREATE TABLE pending_auth_summary_status (
    acct_id         BIGINT      NOT NULL REFERENCES pending_auth_summary (acct_id) ON DELETE CASCADE,
    status_index    INTEGER     NOT NULL,
    account_status  VARCHAR(2),
    PRIMARY KEY (acct_id, status_index)
);

CREATE TABLE pending_auth_detail (
    acct_id                 BIGINT        NOT NULL REFERENCES pending_auth_summary (acct_id) ON DELETE CASCADE,
    auth_date_key           INTEGER       NOT NULL,  -- PA-AUTH-DATE-9C S9(05) COMP-3
    auth_time_key           BIGINT        NOT NULL,  -- PA-AUTH-TIME-9C S9(09) COMP-3
    auth_orig_date          VARCHAR(6),
    auth_orig_time          VARCHAR(6),
    card_num                VARCHAR(16),
    auth_type               VARCHAR(4),
    card_expiry_date        VARCHAR(4),
    message_type            VARCHAR(6),
    message_source          VARCHAR(6),
    auth_id_code            VARCHAR(6),
    auth_resp_code          VARCHAR(2),
    auth_resp_reason        VARCHAR(4),
    processing_code         INTEGER,                 -- PA-PROCESSING-CODE 9(06)
    transaction_amt         NUMERIC(12,2),           -- PA-TRANSACTION-AMT S9(10)V99 COMP-3
    approved_amt            NUMERIC(12,2),           -- PA-APPROVED-AMT S9(10)V99 COMP-3
    merchant_category_code  VARCHAR(4),
    acqr_country_code       VARCHAR(3),
    pos_entry_mode          INTEGER,                 -- PA-POS-ENTRY-MODE 9(02)
    merchant_id             VARCHAR(15),
    merchant_name           VARCHAR(22),
    merchant_city           VARCHAR(13),
    merchant_state          VARCHAR(2),
    merchant_zip            VARCHAR(9),
    transaction_id          VARCHAR(15),
    match_status            VARCHAR(1),
    auth_fraud              VARCHAR(1),
    fraud_rpt_date          VARCHAR(8),
    PRIMARY KEY (acct_id, auth_date_key, auth_time_key)
);
CREATE INDEX ix_pending_auth_detail_card_num ON pending_auth_detail (card_num);
