-- ACCTDATA VSAM KSDS (copybook CVACT01Y, record length 300), key ACCT-ID.
CREATE TABLE accounts (
    acct_id           NUMERIC(11, 0) NOT NULL,
    active_status     CHAR(1)        NOT NULL DEFAULT 'Y',
    curr_bal          NUMERIC(12, 2) NOT NULL DEFAULT 0,
    credit_limit      NUMERIC(12, 2) NOT NULL DEFAULT 0,
    cash_credit_limit NUMERIC(12, 2) NOT NULL DEFAULT 0,
    open_date         DATE,
    expiration_date   DATE,
    reissue_date      DATE,
    curr_cyc_credit   NUMERIC(12, 2) NOT NULL DEFAULT 0,
    curr_cyc_debit    NUMERIC(12, 2) NOT NULL DEFAULT 0,
    addr_zip          VARCHAR(10),
    group_id          VARCHAR(10),
    version           BIGINT         NOT NULL DEFAULT 0,
    CONSTRAINT pk_accounts PRIMARY KEY (acct_id),
    CONSTRAINT ck_accounts_active_status CHECK (active_status IN ('Y', 'N'))
);

-- CBACT04C reads the disclosure group by ACCT-GROUP-ID for every category balance.
CREATE INDEX ix_accounts_group_id ON accounts (group_id);
