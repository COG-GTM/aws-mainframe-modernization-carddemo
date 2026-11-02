-- TRAN-MERCHANT-ID / DALYTRAN-MERCHANT-ID are PIC 9(09): unsigned, at most nine digits.
ALTER TABLE transactions
    ADD CONSTRAINT ck_transactions_merchant_id CHECK (merchant_id BETWEEN 0 AND 999999999);
ALTER TABLE daily_transactions
    ADD CONSTRAINT ck_daily_transactions_merchant_id CHECK (merchant_id BETWEEN 0 AND 999999999);
