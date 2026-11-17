-- Core CardDemo datasets (VSAM KSDS files and the DALYTRAN input) as PostgreSQL tables.
--
-- Mapping rules, applied to every copybook field:
--   PIC X(n)        -> varchar(n), stored without the COBOL trailing space padding
--   PIC 9(n)        -> smallint / integer / bigint with a CHECK for the PIC range
--   PIC S9(n)V99    -> numeric(n+2, 2)
--   X(10) dates     -> date            (YYYY-MM-DD; blank -> NULL)
--   X(26) stamps    -> timestamp(6)    (YYYY-MM-DD HH:MM:SS.ffffff; blank -> NULL)
--   FILLER          -> dropped
-- VSAM prime keys become primary keys and VSAM alternate indexes (AIX) become secondary indexes.
-- VSAM had no referential integrity and the COBOL programs check existence themselves (with their
-- own error messages), so there are no foreign keys: those error paths must stay reachable.

-- ACCTDATA / ACCTDAT, copybook CVACT01Y, RECLN 300, KEYS(11 0)
CREATE TABLE account (
    acct_id               bigint        NOT NULL,  -- ACCT-ID                PIC 9(11)
    active_status         varchar(1)    NOT NULL,  -- ACCT-ACTIVE-STATUS     PIC X(01)
    curr_bal              numeric(12,2) NOT NULL,  -- ACCT-CURR-BAL          PIC S9(10)V99
    credit_limit          numeric(12,2) NOT NULL,  -- ACCT-CREDIT-LIMIT      PIC S9(10)V99
    cash_credit_limit     numeric(12,2) NOT NULL,  -- ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
    open_date             date,                    -- ACCT-OPEN-DATE         PIC X(10)
    expiration_date       date,                    -- ACCT-EXPIRAION-DATE    PIC X(10)
    reissue_date          date,                    -- ACCT-REISSUE-DATE      PIC X(10)
    curr_cyc_credit       numeric(12,2) NOT NULL,  -- ACCT-CURR-CYC-CREDIT   PIC S9(10)V99
    curr_cyc_debit        numeric(12,2) NOT NULL,  -- ACCT-CURR-CYC-DEBIT    PIC S9(10)V99
    addr_zip              varchar(10)   NOT NULL,  -- ACCT-ADDR-ZIP          PIC X(10)
    group_id              varchar(10)   NOT NULL,  -- ACCT-GROUP-ID          PIC X(10)
    CONSTRAINT account_pk PRIMARY KEY (acct_id),
    CONSTRAINT account_acct_id_pic CHECK (acct_id BETWEEN 0 AND 99999999999)
);
COMMENT ON TABLE account IS 'ACCTDATA (CVACT01Y), VSAM KSDS key ACCT-ID';

-- CARDDATA / CARDDAT, copybook CVACT02Y, RECLN 150, KEYS(16 0)
CREATE TABLE card (
    card_num              varchar(16)   NOT NULL,  -- CARD-NUM               PIC X(16)
    acct_id               bigint        NOT NULL,  -- CARD-ACCT-ID           PIC 9(11)
    cvv_cd                smallint      NOT NULL,  -- CARD-CVV-CD            PIC 9(03)
    embossed_name         varchar(50)   NOT NULL,  -- CARD-EMBOSSED-NAME     PIC X(50)
    expiration_date       date,                    -- CARD-EXPIRAION-DATE    PIC X(10)
    active_status         varchar(1)    NOT NULL,  -- CARD-ACTIVE-STATUS     PIC X(01)
    CONSTRAINT card_pk PRIMARY KEY (card_num),
    CONSTRAINT card_acct_id_pic CHECK (acct_id BETWEEN 0 AND 99999999999),
    CONSTRAINT card_cvv_cd_pic CHECK (cvv_cd BETWEEN 0 AND 999)
);
COMMENT ON TABLE card IS 'CARDDATA (CVACT02Y), VSAM KSDS key CARD-NUM';
-- CARDDATA.VSAM.AIX: KEYS(11 16) NONUNIQUEKEY on CARD-ACCT-ID (CICS file CARDAIX)
CREATE INDEX card_acct_id_aix ON card (acct_id);

-- CARDXREF / CCXREF, copybook CVACT03Y, RECLN 50, KEYS(16 0)
CREATE TABLE card_xref (
    card_num              varchar(16)   NOT NULL,  -- XREF-CARD-NUM          PIC X(16)
    cust_id               integer       NOT NULL,  -- XREF-CUST-ID           PIC 9(09)
    acct_id               bigint        NOT NULL,  -- XREF-ACCT-ID           PIC 9(11)
    CONSTRAINT card_xref_pk PRIMARY KEY (card_num),
    CONSTRAINT card_xref_cust_id_pic CHECK (cust_id BETWEEN 0 AND 999999999),
    CONSTRAINT card_xref_acct_id_pic CHECK (acct_id BETWEEN 0 AND 99999999999)
);
COMMENT ON TABLE card_xref IS 'CARDXREF (CVACT03Y), VSAM KSDS key XREF-CARD-NUM';
-- CARDXREF.VSAM.AIX: KEYS(11,25) NONUNIQUEKEY on XREF-ACCT-ID (CICS file CXACAIX)
CREATE INDEX card_xref_acct_id_aix ON card_xref (acct_id);

-- CUSTDATA / CUSTDAT, copybook CVCUS01Y, RECLN 500, KEYS(9 0)
CREATE TABLE customer (
    cust_id               integer       NOT NULL,  -- CUST-ID                  PIC 9(09)
    first_name            varchar(25)   NOT NULL,  -- CUST-FIRST-NAME          PIC X(25)
    middle_name           varchar(25)   NOT NULL,  -- CUST-MIDDLE-NAME         PIC X(25)
    last_name             varchar(25)   NOT NULL,  -- CUST-LAST-NAME           PIC X(25)
    addr_line_1           varchar(50)   NOT NULL,  -- CUST-ADDR-LINE-1         PIC X(50)
    addr_line_2           varchar(50)   NOT NULL,  -- CUST-ADDR-LINE-2         PIC X(50)
    addr_line_3           varchar(50)   NOT NULL,  -- CUST-ADDR-LINE-3         PIC X(50)
    addr_state_cd         varchar(2)    NOT NULL,  -- CUST-ADDR-STATE-CD       PIC X(02)
    addr_country_cd       varchar(3)    NOT NULL,  -- CUST-ADDR-COUNTRY-CD     PIC X(03)
    addr_zip              varchar(10)   NOT NULL,  -- CUST-ADDR-ZIP            PIC X(10)
    phone_num_1           varchar(15)   NOT NULL,  -- CUST-PHONE-NUM-1         PIC X(15)
    phone_num_2           varchar(15)   NOT NULL,  -- CUST-PHONE-NUM-2         PIC X(15)
    ssn                   integer       NOT NULL,  -- CUST-SSN                 PIC 9(09)
    govt_issued_id        varchar(20)   NOT NULL,  -- CUST-GOVT-ISSUED-ID      PIC X(20)
    dob                   date,                    -- CUST-DOB-YYYY-MM-DD      PIC X(10)
    eft_account_id        varchar(10)   NOT NULL,  -- CUST-EFT-ACCOUNT-ID      PIC X(10)
    pri_card_holder_ind   varchar(1)    NOT NULL,  -- CUST-PRI-CARD-HOLDER-IND PIC X(01)
    fico_credit_score     smallint      NOT NULL,  -- CUST-FICO-CREDIT-SCORE   PIC 9(03)
    CONSTRAINT customer_pk PRIMARY KEY (cust_id),
    CONSTRAINT customer_cust_id_pic CHECK (cust_id BETWEEN 0 AND 999999999),
    CONSTRAINT customer_ssn_pic CHECK (ssn BETWEEN 0 AND 999999999),
    CONSTRAINT customer_fico_credit_score_pic CHECK (fico_credit_score BETWEEN 0 AND 999)
);
COMMENT ON TABLE customer IS 'CUSTDATA (CVCUS01Y), VSAM KSDS key CUST-ID';

-- TRANSACT, copybook CVTRA05Y, RECLN 350, KEYS(16 0)
CREATE TABLE transaction (
    tran_id               varchar(16)   NOT NULL,  -- TRAN-ID             PIC X(16)
    type_cd               varchar(2)    NOT NULL,  -- TRAN-TYPE-CD        PIC X(02)
    cat_cd                smallint      NOT NULL,  -- TRAN-CAT-CD         PIC 9(04)
    source                varchar(10)   NOT NULL,  -- TRAN-SOURCE         PIC X(10)
    description           varchar(100)  NOT NULL,  -- TRAN-DESC           PIC X(100)
    amt                   numeric(11,2) NOT NULL,  -- TRAN-AMT            PIC S9(09)V99
    merchant_id           integer       NOT NULL,  -- TRAN-MERCHANT-ID    PIC 9(09)
    merchant_name         varchar(50)   NOT NULL,  -- TRAN-MERCHANT-NAME  PIC X(50)
    merchant_city         varchar(50)   NOT NULL,  -- TRAN-MERCHANT-CITY  PIC X(50)
    merchant_zip          varchar(10)   NOT NULL,  -- TRAN-MERCHANT-ZIP   PIC X(10)
    card_num              varchar(16)   NOT NULL,  -- TRAN-CARD-NUM       PIC X(16)
    orig_ts               timestamp(6),            -- TRAN-ORIG-TS        PIC X(26)
    proc_ts               timestamp(6),            -- TRAN-PROC-TS        PIC X(26)
    CONSTRAINT transaction_pk PRIMARY KEY (tran_id),
    CONSTRAINT transaction_cat_cd_pic CHECK (cat_cd BETWEEN 0 AND 9999),
    CONSTRAINT transaction_merchant_id_pic CHECK (merchant_id BETWEEN 0 AND 999999999)
);
COMMENT ON TABLE transaction IS 'TRANSACT (CVTRA05Y), VSAM KSDS key TRAN-ID';
-- TRANSACT.VSAM.AIX: KEYS(26 304) NONUNIQUEKEY on TRAN-PROC-TS (TRANFILE / TRANIDX)
CREATE INDEX transaction_proc_ts_aix ON transaction (proc_ts);

-- DALYTRAN, copybook CVTRA06Y, RECLN 350: sequential POSTTRAN input, staged in file order
CREATE TABLE daily_transaction (
    seq_no                bigint GENERATED ALWAYS AS IDENTITY,  -- record number in the input file
    tran_id               varchar(16)   NOT NULL,  -- DALYTRAN-ID             PIC X(16)
    type_cd               varchar(2)    NOT NULL,  -- DALYTRAN-TYPE-CD        PIC X(02)
    cat_cd                smallint      NOT NULL,  -- DALYTRAN-CAT-CD         PIC 9(04)
    source                varchar(10)   NOT NULL,  -- DALYTRAN-SOURCE         PIC X(10)
    description           varchar(100)  NOT NULL,  -- DALYTRAN-DESC           PIC X(100)
    amt                   numeric(11,2) NOT NULL,  -- DALYTRAN-AMT            PIC S9(09)V99
    merchant_id           integer       NOT NULL,  -- DALYTRAN-MERCHANT-ID    PIC 9(09)
    merchant_name         varchar(50)   NOT NULL,  -- DALYTRAN-MERCHANT-NAME  PIC X(50)
    merchant_city         varchar(50)   NOT NULL,  -- DALYTRAN-MERCHANT-CITY  PIC X(50)
    merchant_zip          varchar(10)   NOT NULL,  -- DALYTRAN-MERCHANT-ZIP   PIC X(10)
    card_num              varchar(16)   NOT NULL,  -- DALYTRAN-CARD-NUM       PIC X(16)
    orig_ts               timestamp(6),            -- DALYTRAN-ORIG-TS        PIC X(26)
    proc_ts               timestamp(6),            -- DALYTRAN-PROC-TS        PIC X(26)
    CONSTRAINT daily_transaction_pk PRIMARY KEY (seq_no),
    CONSTRAINT daily_transaction_cat_cd_pic CHECK (cat_cd BETWEEN 0 AND 9999),
    CONSTRAINT daily_transaction_merchant_id_pic CHECK (merchant_id BETWEEN 0 AND 999999999)
);
COMMENT ON TABLE daily_transaction IS 'DALYTRAN (CVTRA06Y), sequential input; seq_no keeps file order';

-- TCATBALF, copybook CVTRA01Y, RECLN 50, KEYS(17 0)
CREATE TABLE tran_cat_balance (
    acct_id               bigint        NOT NULL,  -- TRANCAT-ACCT-ID  PIC 9(11)
    type_cd               varchar(2)    NOT NULL,  -- TRANCAT-TYPE-CD  PIC X(02)
    cat_cd                smallint      NOT NULL,  -- TRANCAT-CD       PIC 9(04)
    balance               numeric(11,2) NOT NULL,  -- TRAN-CAT-BAL     PIC S9(09)V99
    CONSTRAINT tran_cat_balance_pk PRIMARY KEY (acct_id, type_cd, cat_cd),
    CONSTRAINT tran_cat_balance_acct_id_pic CHECK (acct_id BETWEEN 0 AND 99999999999),
    CONSTRAINT tran_cat_balance_cat_cd_pic CHECK (cat_cd BETWEEN 0 AND 9999)
);
COMMENT ON TABLE tran_cat_balance IS 'TCATBALF (CVTRA01Y), VSAM KSDS key TRAN-CAT-KEY';

-- DISCGRP, copybook CVTRA02Y, RECLN 50, KEYS(16 0)
CREATE TABLE disclosure_group (
    acct_group_id         varchar(10)   NOT NULL,  -- DIS-ACCT-GROUP-ID  PIC X(10)
    tran_type_cd          varchar(2)    NOT NULL,  -- DIS-TRAN-TYPE-CD   PIC X(02)
    tran_cat_cd           smallint      NOT NULL,  -- DIS-TRAN-CAT-CD    PIC 9(04)
    int_rate              numeric(6,2)  NOT NULL,  -- DIS-INT-RATE       PIC S9(04)V99
    CONSTRAINT disclosure_group_pk PRIMARY KEY (acct_group_id, tran_type_cd, tran_cat_cd),
    CONSTRAINT disclosure_group_tran_cat_cd_pic CHECK (tran_cat_cd BETWEEN 0 AND 9999)
);
COMMENT ON TABLE disclosure_group IS 'DISCGRP (CVTRA02Y), VSAM KSDS key DIS-GROUP-KEY';

-- TRANTYPE, copybook CVTRA03Y, RECLN 60, KEYS(2 0)
CREATE TABLE tran_type (
    type_cd               varchar(2)    NOT NULL,  -- TRAN-TYPE       PIC X(02)
    description           varchar(50)   NOT NULL,  -- TRAN-TYPE-DESC  PIC X(50)
    CONSTRAINT tran_type_pk PRIMARY KEY (type_cd)
);
COMMENT ON TABLE tran_type IS 'TRANTYPE (CVTRA03Y), VSAM KSDS key TRAN-TYPE';

-- TRANCATG, copybook CVTRA04Y, RECLN 60, KEYS(6 0)
CREATE TABLE tran_category (
    type_cd               varchar(2)    NOT NULL,  -- TRAN-TYPE-CD        PIC X(02)
    cat_cd                smallint      NOT NULL,  -- TRAN-CAT-CD         PIC 9(04)
    description           varchar(50)   NOT NULL,  -- TRAN-CAT-TYPE-DESC  PIC X(50)
    CONSTRAINT tran_category_pk PRIMARY KEY (type_cd, cat_cd),
    CONSTRAINT tran_category_cat_cd_pic CHECK (cat_cd BETWEEN 0 AND 9999)
);
COMMENT ON TABLE tran_category IS 'TRANCATG (CVTRA04Y), VSAM KSDS key TRAN-CAT-KEY';

-- USRSEC, copybook CSUSR01Y, RECLN 80, KEYS(8 0)
CREATE TABLE app_user (
    user_id               varchar(8)    NOT NULL,  -- SEC-USR-ID     PIC X(08)
    first_name            varchar(20)   NOT NULL,  -- SEC-USR-FNAME  PIC X(20)
    last_name             varchar(20)   NOT NULL,  -- SEC-USR-LNAME  PIC X(20)
    password_hash         varchar(100)  NOT NULL,  -- SEC-USR-PWD    PIC X(08), stored hashed, never in clear
    user_type             varchar(1)    NOT NULL,  -- SEC-USR-TYPE   PIC X(01): A = admin, U = user
    CONSTRAINT app_user_pk PRIMARY KEY (user_id),
    CONSTRAINT app_user_user_type_chk CHECK (user_type IN ('A', 'U'))
);
COMMENT ON TABLE app_user IS 'USRSEC (CSUSR01Y), VSAM KSDS key SEC-USR-ID; password kept only as a hash';
