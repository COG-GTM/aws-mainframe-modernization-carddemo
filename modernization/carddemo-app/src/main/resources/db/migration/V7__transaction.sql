-- CVTRA05Y TRAN-RECORD (350 bytes). Amount S9(09)V99 -> NUMERIC(11,2). FILLER X(20) is not stored (always spaces).
-- D-015: ORIG-TS/PROC-TS keep their 26-byte text, written back byte for byte; the TIMESTAMP(6) beside each is parsed
-- from either known format and is NULL when the text does not parse (no abend).
CREATE TABLE transaction.transaction (
    tran_id        VARCHAR(16)   PRIMARY KEY,
    type_cd        VARCHAR(2)    NOT NULL,
    cat_cd         BIGINT        NOT NULL CHECK (cat_cd BETWEEN 0 AND 9999),
    source         VARCHAR(10)   NOT NULL,
    description    VARCHAR(100)  NOT NULL,
    amount         NUMERIC(11,2) NOT NULL,
    merchant_id    BIGINT        NOT NULL CHECK (merchant_id BETWEEN 0 AND 999999999),
    merchant_name  VARCHAR(50)   NOT NULL,
    merchant_city  VARCHAR(50)   NOT NULL,
    merchant_zip   VARCHAR(10)   NOT NULL,
    card_num       VARCHAR(16)   NOT NULL,
    orig_ts        TIMESTAMP(6),
    orig_ts_text   VARCHAR(26)   NOT NULL CHECK (char_length(orig_ts_text) = 26),
    proc_ts        TIMESTAMP(6),
    proc_ts_text   VARCHAR(26)   NOT NULL CHECK (char_length(proc_ts_text) = 26)
);

-- AIX TRANSACT.VSAM.AIX KEYS(26 304) = TRAN-PROC-TS
CREATE INDEX transaction_proc_ts_idx ON transaction.transaction (proc_ts);
