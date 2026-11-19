-- CVACT03Y CARD-XREF-RECORD (50 bytes). No FK to account.account: CBTRN02C tolerates a missing account (reason 101, FR-07).
CREATE TABLE card.card_xref (
    card_num  VARCHAR(16) PRIMARY KEY,
    cust_id   BIGINT      NOT NULL CHECK (cust_id BETWEEN 0 AND 999999999),
    acct_id   BIGINT      NOT NULL CHECK (acct_id BETWEEN 0 AND 99999999999)
);
-- AIX CARDXREF KEYS(11,25)
CREATE INDEX card_xref_acct_id_idx ON card.card_xref (acct_id);
