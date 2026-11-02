package com.carddemo.loader;

import static com.carddemo.loader.Column.Kind.DATE;
import static com.carddemo.loader.Column.Kind.DECIMAL;
import static com.carddemo.loader.Column.Kind.INT;
import static com.carddemo.loader.Column.Kind.LONG;
import static com.carddemo.loader.Column.Kind.PASSWORD;
import static com.carddemo.loader.Column.Kind.TEXT;
import static com.carddemo.loader.Column.Kind.TIMESTAMP;

import com.carddemo.common.record.Field;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import com.carddemo.common.record.layout.DalytranRecordLayout;
import com.carddemo.common.record.layout.DisGroupRecordLayout;
import com.carddemo.common.record.layout.SampleDataset;
import com.carddemo.common.record.layout.SecUserDataLayout;
import com.carddemo.common.record.layout.TranCatBalRecordLayout;
import com.carddemo.common.record.layout.TranCatRecordLayout;
import com.carddemo.common.record.layout.TranTypeRecordLayout;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The sample datasets that are loaded and the table each one goes to, in load order (customers,
 * accounts, cards and cross references, reference data, category balances, daily transactions, users).
 * Column lists follow docs/migration/schema-mapping.md; columns without a copybook field
 * ({@code version}, {@code processed}) keep their defaults.
 */
public enum TableMapping {

    CUSTOMERS(SampleDataset.CUSTDATA, "customer.customers",
            Column.key(CustomerRecordLayout.CUST_ID, "cust_id", LONG),
            Column.of(CustomerRecordLayout.CUST_FIRST_NAME, "first_name", TEXT),
            Column.of(CustomerRecordLayout.CUST_MIDDLE_NAME, "middle_name", TEXT),
            Column.of(CustomerRecordLayout.CUST_LAST_NAME, "last_name", TEXT),
            Column.of(CustomerRecordLayout.CUST_ADDR_LINE_1, "addr_line_1", TEXT),
            Column.of(CustomerRecordLayout.CUST_ADDR_LINE_2, "addr_line_2", TEXT),
            Column.of(CustomerRecordLayout.CUST_ADDR_LINE_3, "addr_line_3", TEXT),
            Column.of(CustomerRecordLayout.CUST_ADDR_STATE_CD, "addr_state_cd", TEXT),
            Column.of(CustomerRecordLayout.CUST_ADDR_COUNTRY_CD, "addr_country_cd", TEXT),
            Column.of(CustomerRecordLayout.CUST_ADDR_ZIP, "addr_zip", TEXT),
            Column.of(CustomerRecordLayout.CUST_PHONE_NUM_1, "phone_num_1", TEXT),
            Column.of(CustomerRecordLayout.CUST_PHONE_NUM_2, "phone_num_2", TEXT),
            Column.of(CustomerRecordLayout.CUST_SSN, "ssn", LONG),
            Column.of(CustomerRecordLayout.CUST_GOVT_ISSUED_ID, "govt_issued_id", TEXT),
            Column.of(CustomerRecordLayout.CUST_DOB_YYYY_MM_DD, "date_of_birth", DATE),
            Column.of(CustomerRecordLayout.CUST_EFT_ACCOUNT_ID, "eft_account_id", TEXT),
            Column.of(CustomerRecordLayout.CUST_PRI_CARD_HOLDER_IND, "pri_card_holder_ind", TEXT),
            Column.of(CustomerRecordLayout.CUST_FICO_CREDIT_SCORE, "fico_credit_score", INT)),

    ACCOUNTS(SampleDataset.ACCTDATA, "account.accounts",
            Column.key(AccountRecordLayout.ACCT_ID, "acct_id", LONG),
            Column.of(AccountRecordLayout.ACCT_ACTIVE_STATUS, "active_status", TEXT),
            Column.of(AccountRecordLayout.ACCT_CURR_BAL, "curr_bal", DECIMAL),
            Column.of(AccountRecordLayout.ACCT_CREDIT_LIMIT, "credit_limit", DECIMAL),
            Column.of(AccountRecordLayout.ACCT_CASH_CREDIT_LIMIT, "cash_credit_limit", DECIMAL),
            Column.of(AccountRecordLayout.ACCT_OPEN_DATE, "open_date", DATE),
            Column.of(AccountRecordLayout.ACCT_EXPIRAION_DATE, "expiration_date", DATE),
            Column.of(AccountRecordLayout.ACCT_REISSUE_DATE, "reissue_date", DATE),
            Column.of(AccountRecordLayout.ACCT_CURR_CYC_CREDIT, "curr_cyc_credit", DECIMAL),
            Column.of(AccountRecordLayout.ACCT_CURR_CYC_DEBIT, "curr_cyc_debit", DECIMAL),
            Column.of(AccountRecordLayout.ACCT_ADDR_ZIP, "addr_zip", TEXT),
            Column.of(AccountRecordLayout.ACCT_GROUP_ID, "group_id", TEXT)),

    CARDS(SampleDataset.CARDDATA, "card.cards",
            Column.key(CardRecordLayout.CARD_NUM, "card_num", TEXT),
            Column.of(CardRecordLayout.CARD_ACCT_ID, "acct_id", LONG),
            Column.of(CardRecordLayout.CARD_CVV_CD, "cvv_cd", INT),
            Column.of(CardRecordLayout.CARD_EMBOSSED_NAME, "embossed_name", TEXT),
            Column.of(CardRecordLayout.CARD_EXPIRAION_DATE, "expiration_date", DATE),
            Column.of(CardRecordLayout.CARD_ACTIVE_STATUS, "active_status", TEXT)),

    CARD_XREF(SampleDataset.CARDXREF, "card.card_xref",
            Column.key(CardXrefRecordLayout.XREF_CARD_NUM, "card_num", TEXT),
            Column.of(CardXrefRecordLayout.XREF_CUST_ID, "cust_id", LONG),
            Column.of(CardXrefRecordLayout.XREF_ACCT_ID, "acct_id", LONG)),

    TRANSACTION_TYPES(SampleDataset.TRANTYPE, "transaction.transaction_types",
            Column.key(TranTypeRecordLayout.TRAN_TYPE, "type_cd", TEXT),
            Column.of(TranTypeRecordLayout.TRAN_TYPE_DESC, "type_desc", TEXT)),

    TRANSACTION_CATEGORIES(SampleDataset.TRANCATG, "transaction.transaction_categories",
            Column.key(TranCatRecordLayout.TRAN_TYPE_CD, "type_cd", TEXT),
            Column.key(TranCatRecordLayout.TRAN_CAT_CD, "cat_cd", INT),
            Column.of(TranCatRecordLayout.TRAN_CAT_TYPE_DESC, "cat_type_desc", TEXT)),

    DISCLOSURE_GROUPS(SampleDataset.DISCGRP, "transaction.disclosure_groups",
            Column.key(DisGroupRecordLayout.DIS_ACCT_GROUP_ID, "acct_group_id", TEXT),
            Column.key(DisGroupRecordLayout.DIS_TRAN_TYPE_CD, "type_cd", TEXT),
            Column.key(DisGroupRecordLayout.DIS_TRAN_CAT_CD, "cat_cd", INT),
            Column.of(DisGroupRecordLayout.DIS_INT_RATE, "int_rate", DECIMAL)),

    CATEGORY_BALANCES(SampleDataset.TCATBALF, "transaction.category_balances",
            Column.key(TranCatBalRecordLayout.TRANCAT_ACCT_ID, "acct_id", LONG),
            Column.key(TranCatBalRecordLayout.TRANCAT_TYPE_CD, "type_cd", TEXT),
            Column.key(TranCatBalRecordLayout.TRANCAT_CD, "cat_cd", INT),
            Column.of(TranCatBalRecordLayout.TRAN_CAT_BAL, "balance", DECIMAL)),

    DAILY_TRANSACTIONS(SampleDataset.DALYTRAN, "transaction.daily_transactions",
            Column.key(DalytranRecordLayout.DALYTRAN_ID, "tran_id", TEXT),
            Column.of(DalytranRecordLayout.DALYTRAN_TYPE_CD, "type_cd", TEXT),
            Column.of(DalytranRecordLayout.DALYTRAN_CAT_CD, "cat_cd", INT),
            Column.of(DalytranRecordLayout.DALYTRAN_SOURCE, "source", TEXT),
            Column.of(DalytranRecordLayout.DALYTRAN_DESC, "description", TEXT),
            Column.of(DalytranRecordLayout.DALYTRAN_AMT, "amount", DECIMAL),
            Column.of(DalytranRecordLayout.DALYTRAN_MERCHANT_ID, "merchant_id", LONG),
            Column.of(DalytranRecordLayout.DALYTRAN_MERCHANT_NAME, "merchant_name", TEXT),
            Column.of(DalytranRecordLayout.DALYTRAN_MERCHANT_CITY, "merchant_city", TEXT),
            Column.of(DalytranRecordLayout.DALYTRAN_MERCHANT_ZIP, "merchant_zip", TEXT),
            Column.of(DalytranRecordLayout.DALYTRAN_CARD_NUM, "card_num", TEXT),
            Column.of(DalytranRecordLayout.DALYTRAN_ORIG_TS, "orig_ts", TIMESTAMP),
            Column.of(DalytranRecordLayout.DALYTRAN_PROC_TS, "proc_ts", TIMESTAMP)),

    USERS(SampleDataset.USRSEC, "auth.users",
            Column.key(SecUserDataLayout.SEC_USR_ID, "user_id", TEXT),
            Column.of(SecUserDataLayout.SEC_USR_FNAME, "first_name", TEXT),
            Column.of(SecUserDataLayout.SEC_USR_LNAME, "last_name", TEXT),
            Column.of(SecUserDataLayout.SEC_USR_PWD, "password_hash", PASSWORD),
            Column.of(SecUserDataLayout.SEC_USR_TYPE, "user_type", TEXT));

    private final SampleDataset dataset;
    private final String table;
    private final List<Column> columns;

    TableMapping(SampleDataset dataset, String table, Column... columns) {
        this.dataset = dataset;
        this.table = table;
        this.columns = List.of(columns);
    }

    public SampleDataset dataset() {
        return dataset;
    }

    /** {@code schema.table}. */
    public String table() {
        return table;
    }

    List<Column> columns() {
        return columns;
    }

    List<Column> keyColumns() {
        return columns.stream().filter(Column::key).toList();
    }

    /** Copybook fields with no column (FILLER): blank when a record is written back. */
    List<Field> droppedFields() {
        Set<Field> mapped = columns.stream().map(Column::field).collect(Collectors.toSet());
        return dataset.layout().fields().stream().filter(f -> !mapped.contains(f)).toList();
    }

    String insertSql() {
        String names = columns.stream().map(Column::name).collect(Collectors.joining(", "));
        String marks = columns.stream().map(c -> "?").collect(Collectors.joining(", "));
        return "INSERT INTO " + table + " (" + names + ") VALUES (" + marks + ")";
    }

    String selectSql() {
        return "SELECT " + columns.stream().map(Column::name).collect(Collectors.joining(", ")) + " FROM " + table;
    }
}
