package com.carddemo.migration;

import static com.carddemo.migration.Field.alpha;
import static com.carddemo.migration.Field.date;
import static com.carddemo.migration.Field.signed;
import static com.carddemo.migration.Field.timestamp;
import static com.carddemo.migration.Field.unsigned;

import java.util.List;

/** Byte-exact layouts of the core CardDemo copybooks (app/cpy). Offsets are zero-based. */
public final class CopybookLayouts {

    private CopybookLayouts() {
    }

    /** CVACT01Y - ACCTDAT, 300 bytes. */
    public static final RecordLayout ACCOUNT = new RecordLayout("CVACT01Y", "account", 300, List.of(
            unsigned("ACCT-ID", "acct_id", 0, 11),
            alpha("ACCT-ACTIVE-STATUS", "active_status", 11, 1),
            signed("ACCT-CURR-BAL", "curr_bal", 12, 12, 2),
            signed("ACCT-CREDIT-LIMIT", "credit_limit", 24, 12, 2),
            signed("ACCT-CASH-CREDIT-LIMIT", "cash_credit_limit", 36, 12, 2),
            date("ACCT-OPEN-DATE", "open_date", 48),
            date("ACCT-EXPIRAION-DATE", "expiration_date", 58),
            date("ACCT-REISSUE-DATE", "reissue_date", 68),
            signed("ACCT-CURR-CYC-CREDIT", "curr_cyc_credit", 78, 12, 2),
            signed("ACCT-CURR-CYC-DEBIT", "curr_cyc_debit", 90, 12, 2),
            alpha("ACCT-ADDR-ZIP", "addr_zip", 102, 10),
            alpha("ACCT-GROUP-ID", "group_id", 112, 10)));

    /** CVACT02Y - CARDDAT, 150 bytes. */
    public static final RecordLayout CARD = new RecordLayout("CVACT02Y", "card", 150, List.of(
            alpha("CARD-NUM", "card_num", 0, 16),
            unsigned("CARD-ACCT-ID", "acct_id", 16, 11),
            alpha("CARD-CVV-CD", "cvv_cd", 27, 3),
            alpha("CARD-EMBOSSED-NAME", "embossed_name", 30, 50),
            date("CARD-EXPIRAION-DATE", "expiration_date", 80),
            alpha("CARD-ACTIVE-STATUS", "active_status", 90, 1)));

    /** CVACT03Y - CCXREF, 50 bytes. */
    public static final RecordLayout CARD_XREF = new RecordLayout("CVACT03Y", "card_xref", 50, List.of(
            alpha("XREF-CARD-NUM", "card_num", 0, 16),
            unsigned("XREF-CUST-ID", "cust_id", 16, 9),
            unsigned("XREF-ACCT-ID", "acct_id", 25, 11)));

    /** CVCUS01Y - CUSTDAT, 500 bytes. */
    public static final RecordLayout CUSTOMER = new RecordLayout("CVCUS01Y", "customer", 500, List.of(
            unsigned("CUST-ID", "cust_id", 0, 9),
            alpha("CUST-FIRST-NAME", "first_name", 9, 25),
            alpha("CUST-MIDDLE-NAME", "middle_name", 34, 25),
            alpha("CUST-LAST-NAME", "last_name", 59, 25),
            alpha("CUST-ADDR-LINE-1", "addr_line_1", 84, 50),
            alpha("CUST-ADDR-LINE-2", "addr_line_2", 134, 50),
            alpha("CUST-ADDR-LINE-3", "addr_line_3", 184, 50),
            alpha("CUST-ADDR-STATE-CD", "addr_state_cd", 234, 2),
            alpha("CUST-ADDR-COUNTRY-CD", "addr_country_cd", 236, 3),
            alpha("CUST-ADDR-ZIP", "addr_zip", 239, 10),
            alpha("CUST-PHONE-NUM-1", "phone_num_1", 249, 15),
            alpha("CUST-PHONE-NUM-2", "phone_num_2", 264, 15),
            alpha("CUST-SSN", "ssn", 279, 9),
            alpha("CUST-GOVT-ISSUED-ID", "govt_issued_id", 288, 20),
            date("CUST-DOB-YYYY-MM-DD", "dob", 308),
            alpha("CUST-EFT-ACCOUNT-ID", "eft_account_id", 318, 10),
            alpha("CUST-PRI-CARD-HOLDER-IND", "pri_card_holder_ind", 328, 1),
            unsigned("CUST-FICO-CREDIT-SCORE", "fico_credit_score", 329, 3)));

    /** CVTRA01Y - TCATBALF, 50 bytes. */
    public static final RecordLayout TRAN_CAT_BALANCE = new RecordLayout("CVTRA01Y", "tran_cat_balance", 50, List.of(
            unsigned("TRANCAT-ACCT-ID", "acct_id", 0, 11),
            alpha("TRANCAT-TYPE-CD", "type_cd", 11, 2),
            unsigned("TRANCAT-CD", "cat_cd", 13, 4),
            signed("TRAN-CAT-BAL", "balance", 17, 11, 2)));

    /** CVTRA02Y - DISCGRP, 50 bytes. */
    public static final RecordLayout DISCLOSURE_GROUP = new RecordLayout("CVTRA02Y", "disclosure_group", 50, List.of(
            alpha("DIS-ACCT-GROUP-ID", "acct_group_id", 0, 10),
            alpha("DIS-TRAN-TYPE-CD", "type_cd", 10, 2),
            unsigned("DIS-TRAN-CAT-CD", "cat_cd", 12, 4),
            signed("DIS-INT-RATE", "int_rate", 16, 6, 2)));

    /** CVTRA03Y - TRANTYPE, 60 bytes. */
    public static final RecordLayout TRANSACTION_TYPE = new RecordLayout("CVTRA03Y", "transaction_type", 60, List.of(
            alpha("TRAN-TYPE", "type_cd", 0, 2),
            alpha("TRAN-TYPE-DESC", "description", 2, 50)));

    /** CVTRA04Y - TRANCATG, 60 bytes. */
    public static final RecordLayout TRANSACTION_CATEGORY = new RecordLayout("CVTRA04Y", "transaction_category", 60,
            List.of(
                    alpha("TRAN-TYPE-CD", "type_cd", 0, 2),
                    unsigned("TRAN-CAT-CD", "cat_cd", 2, 4),
                    alpha("TRAN-CAT-TYPE-DESC", "description", 6, 50)));

    private static List<Field> transactionFields() {
        return List.of(
                alpha("TRAN-ID", "tran_id", 0, 16),
                alpha("TRAN-TYPE-CD", "type_cd", 16, 2),
                unsigned("TRAN-CAT-CD", "cat_cd", 18, 4),
                alpha("TRAN-SOURCE", "source", 22, 10),
                alpha("TRAN-DESC", "description", 32, 100),
                signed("TRAN-AMT", "amount", 132, 11, 2),
                unsigned("TRAN-MERCHANT-ID", "merchant_id", 143, 9),
                alpha("TRAN-MERCHANT-NAME", "merchant_name", 152, 50),
                alpha("TRAN-MERCHANT-CITY", "merchant_city", 202, 50),
                alpha("TRAN-MERCHANT-ZIP", "merchant_zip", 252, 10),
                alpha("TRAN-CARD-NUM", "card_num", 262, 16),
                timestamp("TRAN-ORIG-TS", "orig_ts", 278),
                timestamp("TRAN-PROC-TS", "proc_ts", 304));
    }

    /** CVTRA05Y - TRANSACT, 350 bytes. */
    public static final RecordLayout TRANSACTION = new RecordLayout("CVTRA05Y", "card_transaction", 350,
            transactionFields());

    /** CVTRA06Y - DALYTRAN, 350 bytes (same shape as CVTRA05Y, DALYTRAN- prefix). */
    public static final RecordLayout DAILY_TRANSACTION = new RecordLayout("CVTRA06Y", "daily_transaction", 350,
            transactionFields());

    /** CSUSR01Y - USRSEC, 80 bytes. SEC-USR-PWD is re-hashed with BCrypt on load. */
    public static final RecordLayout USER_SECURITY = new RecordLayout("CSUSR01Y", "user_security", 80, List.of(
            alpha("SEC-USR-ID", "user_id", 0, 8),
            alpha("SEC-USR-FNAME", "first_name", 8, 20),
            alpha("SEC-USR-LNAME", "last_name", 28, 20),
            alpha("SEC-USR-PWD", "password_hash", 48, 8),
            alpha("SEC-USR-TYPE", "user_type", 56, 1)));
}
