-- Copybook audit (docs/migration/schema-mapping.md).

-- DALYREJS (CBTRN02C REJECT-RECORD): REJECT-TRAN-DATA X(350) is the whole DALYTRAN-RECORD
-- (CVTRA06Y, moved in 2500-WRITE-REJECT-REC) and VALIDATION-TRAILER X(80) is
-- WS-VALIDATION-FAIL-REASON 9(04) + WS-VALIDATION-FAIL-REASON-DESC X(76). The table kept only the
-- id, card and reason; the remaining transaction fields are added so nothing of the record is lost.
ALTER TABLE transaction_rejects
    ADD COLUMN type_cd       VARCHAR(2),
    ADD COLUMN cat_cd        INTEGER,
    ADD COLUMN source        VARCHAR(10),
    ADD COLUMN description   VARCHAR(100),
    ADD COLUMN amount        NUMERIC(11, 2),
    ADD COLUMN merchant_id   BIGINT,
    ADD COLUMN merchant_name VARCHAR(50),
    ADD COLUMN merchant_city VARCHAR(50),
    ADD COLUMN merchant_zip  VARCHAR(10),
    ADD COLUMN orig_ts       TIMESTAMP,
    ADD COLUMN proc_ts       TIMESTAMP,
    ALTER COLUMN reason_desc TYPE VARCHAR(76),
    ADD CONSTRAINT ck_transaction_rejects_reason CHECK (reason_code BETWEEN 0 AND 9999),
    ADD CONSTRAINT ck_transaction_rejects_cat_cd CHECK (cat_cd IS NULL OR cat_cd BETWEEN 0 AND 9999),
    ADD CONSTRAINT ck_transaction_rejects_merchant_id CHECK (merchant_id IS NULL OR merchant_id BETWEEN 0 AND 999999999);

-- TRAN-CAT-CD / DALYTRAN-CAT-CD / TRANCAT-CD / DIS-TRAN-CAT-CD are PIC 9(04).
ALTER TABLE transactions ADD CONSTRAINT ck_transactions_cat_cd CHECK (cat_cd BETWEEN 0 AND 9999);
ALTER TABLE daily_transactions ADD CONSTRAINT ck_daily_transactions_cat_cd CHECK (cat_cd BETWEEN 0 AND 9999);
ALTER TABLE transaction_categories ADD CONSTRAINT ck_transaction_categories_cat_cd CHECK (cat_cd BETWEEN 0 AND 9999);
ALTER TABLE disclosure_groups ADD CONSTRAINT ck_disclosure_groups_cat_cd CHECK (cat_cd BETWEEN 0 AND 9999);
ALTER TABLE category_balances ADD CONSTRAINT ck_category_balances_cat_cd CHECK (cat_cd BETWEEN 0 AND 9999);

-- TRANSACT alternate index (TRANFILE.jcl / TRANIDX.jcl, KEYS(26 304) = TRAN-PROC-TS, non-unique).
-- TRANREPT selects TRANSACT records by the date part of TRAN-PROC-TS.
CREATE INDEX ix_transactions_proc_ts ON transactions (proc_ts);
