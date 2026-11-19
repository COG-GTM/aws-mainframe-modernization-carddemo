-- CVTRA01Y TRAN-CAT-BAL-RECORD (50 bytes). Key 9(11) + X(02) + 9(04); balance S9(09)V99 -> NUMERIC(11,2).
CREATE TABLE reference.tran_cat_balance (
    acct_id  BIGINT        NOT NULL CHECK (acct_id BETWEEN 0 AND 99999999999),
    type_cd  VARCHAR(2)    NOT NULL,
    cat_cd   BIGINT        NOT NULL CHECK (cat_cd BETWEEN 0 AND 9999),
    balance  NUMERIC(11,2) NOT NULL,
    version  BIGINT        NOT NULL DEFAULT 0,
    PRIMARY KEY (acct_id, type_cd, cat_cd)
);
