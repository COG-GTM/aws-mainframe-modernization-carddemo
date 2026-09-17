-- CARDDATA VSAM KSDS (copybook CVACT02Y, record length 150), key CARD-NUM,
-- alternate index CARDAIX on CARD-ACCT-ID.
CREATE TABLE cards (
    card_num        CHAR(16)       NOT NULL,
    acct_id         NUMERIC(11, 0) NOT NULL,
    cvv_cd          SMALLINT,
    embossed_name   VARCHAR(50),
    expiration_date DATE,
    active_status   CHAR(1)        NOT NULL DEFAULT 'Y',
    version         BIGINT         NOT NULL DEFAULT 0,
    CONSTRAINT pk_cards PRIMARY KEY (card_num),
    CONSTRAINT ck_cards_active_status CHECK (active_status IN ('Y', 'N')),
    CONSTRAINT ck_cards_cvv CHECK (cvv_cd IS NULL OR cvv_cd BETWEEN 0 AND 999)
);

CREATE INDEX ix_cards_acct_id ON cards (acct_id);

-- CARDXREF VSAM KSDS (copybook CVACT03Y, record length 50), key XREF-CARD-NUM,
-- alternate index XREFAIX on XREF-ACCT-ID.
CREATE TABLE card_xref (
    card_num CHAR(16)       NOT NULL,
    cust_id  NUMERIC(9, 0)  NOT NULL,
    acct_id  NUMERIC(11, 0) NOT NULL,
    CONSTRAINT pk_card_xref PRIMARY KEY (card_num),
    CONSTRAINT fk_card_xref_card FOREIGN KEY (card_num) REFERENCES cards (card_num)
);

CREATE INDEX ix_card_xref_acct_id ON card_xref (acct_id);
CREATE INDEX ix_card_xref_cust_id ON card_xref (cust_id);
