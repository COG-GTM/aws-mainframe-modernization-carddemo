-- Idempotency ledger of the posting endpoint (no COBOL counterpart). One row per posted
-- transaction id, written in the same database transaction as the account update, so a retry of
-- a posting whose account update already committed returns the recorded outcome instead of
-- applying the amount again.
CREATE TABLE account_postings (
    tran_id             VARCHAR(16)     NOT NULL,
    acct_id             BIGINT          NOT NULL,
    amount              NUMERIC(12, 2)  NOT NULL,
    transaction_date    DATE            NOT NULL,
    curr_bal            NUMERIC(12, 2)  NOT NULL,
    curr_cyc_credit     NUMERIC(12, 2)  NOT NULL,
    curr_cyc_debit      NUMERIC(12, 2)  NOT NULL,
    posted_at           TIMESTAMP       NOT NULL,
    CONSTRAINT pk_account_postings PRIMARY KEY (tran_id),
    CONSTRAINT fk_account_postings_account FOREIGN KEY (acct_id) REFERENCES accounts (acct_id)
);
