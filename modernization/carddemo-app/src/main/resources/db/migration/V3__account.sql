-- CVACT01Y ACCOUNT-RECORD (300 bytes). Money S9(10)V99 -> NUMERIC(12,2); dates X(10) -> DATE (D-013).
CREATE TABLE account.account (
    acct_id            BIGINT        PRIMARY KEY CHECK (acct_id BETWEEN 0 AND 99999999999),
    active_status      VARCHAR(1)    NOT NULL,
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
