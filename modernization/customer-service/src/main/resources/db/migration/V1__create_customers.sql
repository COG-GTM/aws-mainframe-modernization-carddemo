-- CUSTDATA VSAM KSDS (copybook CVCUS01Y, record length 500), key CUST-ID.
CREATE TABLE customers (
    cust_id             NUMERIC(9, 0) NOT NULL,
    first_name          VARCHAR(25),
    middle_name         VARCHAR(25),
    last_name           VARCHAR(25),
    addr_line_1         VARCHAR(50),
    addr_line_2         VARCHAR(50),
    addr_line_3         VARCHAR(50),
    addr_state_cd       CHAR(2),
    addr_country_cd     CHAR(3),
    addr_zip            VARCHAR(10),
    phone_num_1         VARCHAR(15),
    phone_num_2         VARCHAR(15),
    ssn                 NUMERIC(9, 0),
    govt_issued_id      VARCHAR(20),
    date_of_birth       DATE,
    eft_account_id      VARCHAR(10),
    pri_card_holder_ind CHAR(1),
    fico_credit_score   SMALLINT,
    version             BIGINT        NOT NULL DEFAULT 0,
    CONSTRAINT pk_customers PRIMARY KEY (cust_id),
    CONSTRAINT ck_customers_fico CHECK (fico_credit_score IS NULL OR fico_credit_score BETWEEN 300 AND 850)
);
