-- Wave 1 (D-052): PostgreSQL NUMERIC has no negative zero, but a COBOL signed DISPLAY field does (overpunch '}' on a zero
-- value). CBTRN02C keeps it on MOVE DALYTRAN-AMT TO TRAN-AMT (cbl:430) and produces it when a negative ADD result is
-- truncated to zero (cbl:509,527,547-552). One flag per signed copybook field of the POSTTRAN tables keeps that sign; it may
-- be true only when the value is zero.
ALTER TABLE account.accounts
    ADD COLUMN curr_bal_negative_zero          BOOLEAN NOT NULL DEFAULT FALSE CHECK (NOT curr_bal_negative_zero OR curr_bal = 0),
    ADD COLUMN credit_limit_negative_zero      BOOLEAN NOT NULL DEFAULT FALSE CHECK (NOT credit_limit_negative_zero OR credit_limit = 0),
    ADD COLUMN cash_credit_limit_negative_zero BOOLEAN NOT NULL DEFAULT FALSE CHECK (NOT cash_credit_limit_negative_zero OR cash_credit_limit = 0),
    ADD COLUMN curr_cyc_credit_negative_zero   BOOLEAN NOT NULL DEFAULT FALSE CHECK (NOT curr_cyc_credit_negative_zero OR curr_cyc_credit = 0),
    ADD COLUMN curr_cyc_debit_negative_zero    BOOLEAN NOT NULL DEFAULT FALSE CHECK (NOT curr_cyc_debit_negative_zero OR curr_cyc_debit = 0);

ALTER TABLE reference.tran_cat_balances
    ADD COLUMN balance_negative_zero BOOLEAN NOT NULL DEFAULT FALSE CHECK (NOT balance_negative_zero OR balance = 0);

ALTER TABLE transaction.transactions
    ADD COLUMN amount_negative_zero BOOLEAN NOT NULL DEFAULT FALSE CHECK (NOT amount_negative_zero OR amount = 0);
