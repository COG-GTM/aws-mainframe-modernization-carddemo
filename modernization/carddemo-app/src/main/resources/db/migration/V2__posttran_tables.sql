-- Phase 1: the four KSDS of POSTTRAN as tables (plan §1, boundary B-03..B-06). One column per copybook field,
-- FILLER not stored (D-020). PIC 9(n) -> BIGINT/INTEGER with range CHECK; PIC S9(i)V99 -> NUMERIC(i+2,2); PIC X(n) ->
-- VARCHAR(n) without trailing spaces; text dates/timestamps stay VARCHAR (D-004). Key text columns use COLLATE "C" so
-- ORDER BY key = KSDS key (byte) order for unloads (D-037).

-- ACCTDATA, app/cpy/CVACT01Y.cpy (RECL 300, key ACCT-ID)
CREATE TABLE account.accounts (
    acct_id            BIGINT        NOT NULL PRIMARY KEY CHECK (acct_id BETWEEN 0 AND 99999999999),
    active_status      VARCHAR(1)    NOT NULL,
    curr_bal           NUMERIC(12,2) NOT NULL,
    credit_limit       NUMERIC(12,2) NOT NULL,
    cash_credit_limit  NUMERIC(12,2) NOT NULL,
    open_date          VARCHAR(10)   NOT NULL,
    expiration_date    VARCHAR(10)   NOT NULL,
    reissue_date       VARCHAR(10)   NOT NULL,
    curr_cyc_credit    NUMERIC(12,2) NOT NULL,
    curr_cyc_debit     NUMERIC(12,2) NOT NULL,
    addr_zip           VARCHAR(10)   NOT NULL,
    group_id           VARCHAR(10)   NOT NULL,
    version            BIGINT        NOT NULL DEFAULT 0
);

-- CARDXREF, app/cpy/CVACT03Y.cpy (RECL 50, key XREF-CARD-NUM)
CREATE TABLE card.card_xref (
    card_num  VARCHAR(16) COLLATE "C" NOT NULL PRIMARY KEY,
    cust_id   BIGINT      NOT NULL CHECK (cust_id BETWEEN 0 AND 999999999),
    acct_id   BIGINT      NOT NULL CHECK (acct_id BETWEEN 0 AND 99999999999)
);

-- TCATBALF, app/cpy/CVTRA01Y.cpy (RECL 50, key TRAN-CAT-KEY = TRANCAT-ACCT-ID + TRANCAT-TYPE-CD + TRANCAT-CD)
CREATE TABLE reference.tran_cat_balances (
    acct_id  BIGINT        NOT NULL CHECK (acct_id BETWEEN 0 AND 99999999999),
    type_cd  VARCHAR(2) COLLATE "C" NOT NULL,
    cat_cd   INTEGER       NOT NULL CHECK (cat_cd BETWEEN 0 AND 9999),
    balance  NUMERIC(11,2) NOT NULL,
    version  BIGINT        NOT NULL DEFAULT 0,
    PRIMARY KEY (acct_id, type_cd, cat_cd)
);

-- TRANSACT, app/cpy/CVTRA05Y.cpy (RECL 350, key TRAN-ID). Written once per posted record; emptied at a new POSTTRAN
-- instance (D-024).
CREATE TABLE transaction.transactions (
    tran_id        VARCHAR(16) COLLATE "C" NOT NULL PRIMARY KEY,
    type_cd        VARCHAR(2)    NOT NULL,
    cat_cd         INTEGER       NOT NULL CHECK (cat_cd BETWEEN 0 AND 9999),
    source         VARCHAR(10)   NOT NULL,
    description    VARCHAR(100)  NOT NULL,
    amount         NUMERIC(11,2) NOT NULL,
    merchant_id    BIGINT        NOT NULL CHECK (merchant_id BETWEEN 0 AND 999999999),
    merchant_name  VARCHAR(50)   NOT NULL,
    merchant_city  VARCHAR(50)   NOT NULL,
    merchant_zip   VARCHAR(10)   NOT NULL,
    card_num       VARCHAR(16)   NOT NULL,
    orig_ts        VARCHAR(26)   NOT NULL,
    proc_ts        VARCHAR(26)   NOT NULL
);
