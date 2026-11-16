-- One table per VSAM KSDS (plus the DALYTRAN input file), columns in copybook order, FILLER dropped.
-- Text (PIC X) columns hold the field with its trailing spaces removed; PIC 9 keys and codes are integers;
-- S9(n)V99 amounts are NUMERIC; YYYY-MM-DD dates and YYYY-MM-DD HH:MM:SS.ffffff timestamps are typed.

-- ACCTDATA (CVACT01Y), KSDS key ACCT-ID
create table accounts (
    acct_id                bigint         not null,
    active_status          varchar(1)     not null,
    curr_bal               numeric(12, 2) not null,
    credit_limit           numeric(12, 2) not null,
    cash_credit_limit      numeric(12, 2) not null,
    open_date              date,
    expiration_date        date,
    reissue_date           date,
    curr_cyc_credit        numeric(12, 2) not null,
    curr_cyc_debit         numeric(12, 2) not null,
    addr_zip               varchar(10)    not null,
    group_id               varchar(10)    not null,
    constraint pk_accounts primary key (acct_id),
    constraint ck_accounts_acct_id check (acct_id between 0 and 99999999999)
);

-- CUSTDATA (CVCUS01Y), KSDS key CUST-ID
create table customers (
    cust_id                bigint      not null,
    first_name             varchar(25) not null,
    middle_name            varchar(25) not null,
    last_name              varchar(25) not null,
    addr_line_1            varchar(50) not null,
    addr_line_2            varchar(50) not null,
    addr_line_3            varchar(50) not null,
    addr_state_cd          varchar(2)  not null,
    addr_country_cd        varchar(3)  not null,
    addr_zip               varchar(10) not null,
    phone_num_1            varchar(15) not null,
    phone_num_2            varchar(15) not null,
    ssn                    integer     not null,
    govt_issued_id         varchar(20) not null,
    dob                    date,
    eft_account_id         varchar(10) not null,
    pri_card_holder_ind    varchar(1)  not null,
    fico_credit_score      integer     not null,
    constraint pk_customers primary key (cust_id),
    constraint ck_customers_cust_id check (cust_id between 0 and 999999999),
    constraint ck_customers_ssn check (ssn between 0 and 999999999),
    constraint ck_customers_fico check (fico_credit_score between 0 and 999)
);

-- CARDDATA (CVACT02Y), KSDS key CARD-NUM, AIX on CARD-ACCT-ID
create table cards (
    card_num               varchar(16) not null,
    acct_id                bigint      not null,
    cvv_cd                 integer     not null,
    embossed_name          varchar(50) not null,
    expiration_date        date,
    active_status          varchar(1)  not null,
    constraint pk_cards primary key (card_num),
    constraint fk_cards_account foreign key (acct_id) references accounts (acct_id),
    constraint ck_cards_cvv check (cvv_cd between 0 and 999)
);
create index ix_cards_acct_id on cards (acct_id);

-- CARDXREF (CVACT03Y), KSDS key XREF-CARD-NUM, AIX on XREF-ACCT-ID
create table card_xrefs (
    card_num               varchar(16) not null,
    cust_id                bigint      not null,
    acct_id                bigint      not null,
    constraint pk_card_xrefs primary key (card_num),
    constraint fk_card_xrefs_card foreign key (card_num) references cards (card_num),
    constraint fk_card_xrefs_customer foreign key (cust_id) references customers (cust_id),
    constraint fk_card_xrefs_account foreign key (acct_id) references accounts (acct_id)
);
create index ix_card_xrefs_acct_id on card_xrefs (acct_id);

-- TRANSACT (CVTRA05Y), KSDS key TRAN-ID, AIX on TRAN-PROC-TS
create table transactions (
    tran_id                varchar(16)    not null,
    type_cd                varchar(2)     not null,
    cat_cd                 integer        not null,
    source                 varchar(10)    not null,
    description            varchar(100)   not null,
    amount                 numeric(11, 2) not null,
    merchant_id            bigint         not null,
    merchant_name          varchar(50)    not null,
    merchant_city          varchar(50)    not null,
    merchant_zip           varchar(10)    not null,
    card_num               varchar(16)    not null,
    orig_ts                timestamp(6),
    proc_ts                timestamp(6),
    constraint pk_transactions primary key (tran_id),
    constraint ck_transactions_cat_cd check (cat_cd between 0 and 9999)
);
create index ix_transactions_proc_ts on transactions (proc_ts);

-- DALYTRAN (CVTRA06Y), the sequential input POSTTRAN posts into TRANSACT; kept in file order
create table daily_transactions (
    record_seq             integer        not null,
    tran_id                varchar(16)    not null,
    type_cd                varchar(2)     not null,
    cat_cd                 integer        not null,
    source                 varchar(10)    not null,
    description            varchar(100)   not null,
    amount                 numeric(11, 2) not null,
    merchant_id            bigint         not null,
    merchant_name          varchar(50)    not null,
    merchant_city          varchar(50)    not null,
    merchant_zip           varchar(10)    not null,
    card_num               varchar(16)    not null,
    orig_ts                timestamp(6),
    proc_ts                timestamp(6),
    constraint pk_daily_transactions primary key (record_seq),
    constraint ck_daily_transactions_cat_cd check (cat_cd between 0 and 9999)
);

-- TCATBALF (CVTRA01Y), KSDS key TRAN-CAT-KEY
create table tran_cat_balances (
    acct_id                bigint         not null,
    type_cd                varchar(2)     not null,
    cat_cd                 integer        not null,
    balance                numeric(11, 2) not null,
    constraint pk_tran_cat_balances primary key (acct_id, type_cd, cat_cd),
    constraint ck_tran_cat_balances_cat_cd check (cat_cd between 0 and 9999)
);

-- DISCGRP (CVTRA02Y), KSDS key DIS-GROUP-KEY
create table disclosure_groups (
    acct_group_id          varchar(10)   not null,
    type_cd                varchar(2)    not null,
    cat_cd                 integer       not null,
    int_rate               numeric(6, 2) not null,
    constraint pk_disclosure_groups primary key (acct_group_id, type_cd, cat_cd),
    constraint ck_disclosure_groups_cat_cd check (cat_cd between 0 and 9999)
);

-- TRANTYPE (CVTRA03Y), KSDS key TRAN-TYPE
create table tran_types (
    type_cd                varchar(2)  not null,
    description            varchar(50) not null,
    constraint pk_tran_types primary key (type_cd)
);

-- TRANCATG (CVTRA04Y), KSDS key TRAN-CAT-KEY
create table tran_categories (
    type_cd                varchar(2)  not null,
    cat_cd                 integer     not null,
    description            varchar(50) not null,
    constraint pk_tran_categories primary key (type_cd, cat_cd),
    constraint ck_tran_categories_cat_cd check (cat_cd between 0 and 9999)
);

-- USRSEC (CSUSR01Y), KSDS key SEC-USR-ID; the plain-text SEC-USR-PWD is stored as a BCrypt hash
create table users (
    user_id                varchar(8)  not null,
    first_name             varchar(20) not null,
    last_name              varchar(20) not null,
    password_hash          varchar(72) not null,
    user_type              varchar(1)  not null,
    constraint pk_users primary key (user_id)
);
