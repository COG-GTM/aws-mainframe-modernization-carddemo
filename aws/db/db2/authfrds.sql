-- DB2 CARDDEMO.AUTHFRDS (app/app-authorization-ims-db2-mq/ddl/AUTHFRDS.ddl) + index XAUTHFRD (XAUTHFRD.ddl)
-- -> carddemo.authfrds (contract data-model.md 3.1). Written by COPAUS2C (INSERT, UPDATE on duplicate key).
-- Idempotent. Requires schema.sql (schema carddemo).
--
-- DB2 -> PostgreSQL conversion:
--   TIMESTAMP (DB2, microseconds)   -> TIMESTAMP(6)
--   DECIMAL(p,s)                    -> NUMERIC(p,s)
--   DECIMAL(11) / DECIMAL(9)        -> NUMERIC(11,0) / NUMERIC(9,0)
--   CHAR / VARCHAR / SMALLINT / DATE unchanged
--   MERCHANT_CATAGORY_CODE          -> merchant_category_code (misspelling corrected, contract 1.3)
--   CREATE UNIQUE INDEX XAUTHFRD (CARD_NUM ASC, AUTH_TS DESC) COPY YES
--                                   -> ix_authfrds_card_ts (card_num, auth_ts DESC); uniqueness already
--                                      guaranteed by the primary key on the same columns, so non-unique.
--   ERASE/CLOSE/COPY storage clauses have no PostgreSQL equivalent and are dropped.

CREATE TABLE IF NOT EXISTS carddemo.authfrds (
    card_num                CHAR(16)      NOT NULL,
    auth_ts                 TIMESTAMP(6)  NOT NULL,
    auth_type               CHAR(4),
    card_expiry_date        CHAR(4),
    message_type            CHAR(6),
    message_source          CHAR(6),
    auth_id_code            CHAR(6),
    auth_resp_code          CHAR(2),
    auth_resp_reason        CHAR(4),
    processing_code         CHAR(6),
    transaction_amt         NUMERIC(12,2),
    approved_amt            NUMERIC(12,2),
    merchant_category_code  CHAR(4),
    acqr_country_code       CHAR(3),
    pos_entry_mode          SMALLINT,
    merchant_id             CHAR(15),
    merchant_name           VARCHAR(22),
    merchant_city           CHAR(13),
    merchant_state          CHAR(2),
    merchant_zip            CHAR(9),
    transaction_id          CHAR(15),
    match_status            CHAR(1),
    auth_fraud              CHAR(1),
    fraud_rpt_date          DATE,
    acct_id                 NUMERIC(11,0),
    cust_id                 NUMERIC(9,0),
    CONSTRAINT pk_authfrds PRIMARY KEY (card_num, auth_ts),
    CONSTRAINT ck_authfrds_match_status CHECK (match_status IN ('P', 'D', 'E', 'M')),
    CONSTRAINT ck_authfrds_auth_fraud CHECK (auth_fraud IN ('F', 'R'))
);
CREATE INDEX IF NOT EXISTS ix_authfrds_card_ts ON carddemo.authfrds (card_num, auth_ts DESC);

COMMENT ON TABLE  carddemo.authfrds IS 'DB2 CARDDEMO.AUTHFRDS (DCLGEN AUTHFRDS.dcl); fraud-flagged authorizations written by COPAUS2C';
COMMENT ON INDEX  carddemo.ix_authfrds_card_ts IS 'DB2 index CARDDEMO.XAUTHFRD (CARD_NUM ASC, AUTH_TS DESC)';
COMMENT ON COLUMN carddemo.authfrds.card_num IS 'CARD_NUM CHAR(16) NOT NULL';
COMMENT ON COLUMN carddemo.authfrds.auth_ts IS 'AUTH_TS TIMESTAMP NOT NULL (COPAUS2C builds it from PA-AUTH-ORIG-DATE/TIME)';
COMMENT ON COLUMN carddemo.authfrds.merchant_category_code IS 'MERCHANT_CATAGORY_CODE CHAR(4) (source misspelling)';
COMMENT ON COLUMN carddemo.authfrds.match_status IS 'MATCH_STATUS CHAR(1): P pending, D declined, E expired, M matched (CIPAUDTY)';
COMMENT ON COLUMN carddemo.authfrds.auth_fraud IS 'AUTH_FRAUD CHAR(1): F confirmed fraud, R fraud removed';
COMMENT ON COLUMN carddemo.authfrds.fraud_rpt_date IS 'FRAUD_RPT_DATE DATE';
COMMENT ON COLUMN carddemo.authfrds.acct_id IS 'ACCT_ID DECIMAL(11)';
COMMENT ON COLUMN carddemo.authfrds.cust_id IS 'CUST_ID DECIMAL(9)';
