-- The ledger also records which COBOL paragraph a posting followed (CBTRN02C daily transaction or
-- COBIL00C bill payment): they update different account fields, so a request of one type must never
-- be answered as a replay of the other. Rows written before this column existed were all CBTRN02C.
ALTER TABLE account_postings
    ADD COLUMN posting_type VARCHAR(17) NOT NULL DEFAULT 'DAILY_TRANSACTION',
    ADD CONSTRAINT ck_account_postings_type CHECK (posting_type IN ('DAILY_TRANSACTION', 'BILL_PAYMENT'));
