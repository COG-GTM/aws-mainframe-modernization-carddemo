package com.carddemo.batch.refdata;

import com.carddemo.batch.record.AccountRecord;
import com.carddemo.batch.record.CardRecord;
import com.carddemo.batch.record.CardXrefRecord;
import com.carddemo.batch.record.CustomerRecord;
import com.carddemo.batch.record.DisclosureGroupRecord;
import com.carddemo.batch.record.TranCatBalRecord;
import com.carddemo.batch.record.TransactionCategoryRecord;
import com.carddemo.batch.record.TransactionRecord;
import com.carddemo.batch.record.TransactionTypeRecord;
import java.sql.Date;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * Tables the batch module may load/back up, with their legacy fixed-width layout (data-model.md §2) and seed file
 * under {@code app/data/ASCII/} (= S3 {@code seed/ascii/}).
 */
public enum TableSpec {
    CUSTOMER("customer", "custdata.txt", true, List.of("cust_id"), line -> {
        CustomerRecord r = CustomerRecord.parse(line);
        return row("cust_id", r.custId(), "first_name", r.firstName(), "middle_name", r.middleName(),
                "last_name", r.lastName(), "addr_line_1", r.addrLine1(), "addr_line_2", r.addrLine2(),
                "addr_line_3", r.addrLine3(), "addr_state_cd", r.addrStateCd(), "addr_country_cd",
                r.addrCountryCd(), "addr_zip", r.addrZip(), "phone_num_1", r.phoneNum1(), "phone_num_2",
                r.phoneNum2(), "ssn", r.ssn(), "govt_issued_id", r.govtIssuedId(), "dob", date(r.dob()),
                "eft_account_id", r.eftAccountId(), "pri_card_holder_ind", r.priCardHolderInd(),
                "fico_credit_score", r.ficoCreditScore());
    }),
    ACCOUNT("account", "acctdata.txt", true, List.of("acct_id"), line -> {
        AccountRecord r = AccountRecord.parse(line);
        return row("acct_id", r.acctId(), "active_status", r.activeStatus(), "curr_bal", r.currBal(),
                "credit_limit", r.creditLimit(), "cash_credit_limit", r.cashCreditLimit(), "open_date",
                date(r.openDate()), "expiration_date", date(r.expirationDate()), "reissue_date",
                date(r.reissueDate()), "curr_cyc_credit", r.currCycCredit(), "curr_cyc_debit", r.currCycDebit(),
                "addr_zip", r.addrZip(), "group_id", r.groupId());
    }),
    CARD("card", "carddata.txt", true, List.of("card_num"), line -> {
        CardRecord r = CardRecord.parse(line);
        return row("card_num", r.cardNum(), "acct_id", r.acctId(), "cvv_cd", r.cvvCd(), "embossed_name",
                r.embossedName(), "expiration_date", date(r.expirationDate()), "active_status", r.activeStatus());
    }),
    CARD_XREF("card_xref", "cardxref.txt", false, List.of("card_num"), line -> {
        CardXrefRecord r = CardXrefRecord.parse(line);
        return row("card_num", r.cardNum(), "cust_id", r.custId(), "acct_id", r.acctId());
    }),
    TRANSACTION_TYPE("transaction_type", "trantype.txt", true, List.of("type_cd"), line -> {
        TransactionTypeRecord r = TransactionTypeRecord.parse(line);
        return row("type_cd", r.typeCd(), "description", r.description());
    }),
    TRANSACTION_CATEGORY("transaction_category", "trancatg.txt", false, List.of("type_cd", "cat_cd"), line -> {
        TransactionCategoryRecord r = TransactionCategoryRecord.parse(line);
        return row("type_cd", r.typeCd(), "cat_cd", r.catCd(), "description", r.description());
    }),
    DISCLOSURE_GROUP("disclosure_group", "discgrp.txt", false, List.of("acct_group_id", "type_cd", "cat_cd"),
            line -> {
                DisclosureGroupRecord r = DisclosureGroupRecord.parse(line);
                return row("acct_group_id", r.acctGroupId(), "type_cd", r.typeCd(), "cat_cd", r.catCd(),
                        "int_rate", r.intRate());
            }),
    TRAN_CAT_BALANCE("tran_cat_balance", "tcatbal.txt", true, List.of("acct_id", "type_cd", "cat_cd"), line -> {
        TranCatBalRecord r = TranCatBalRecord.parse(line);
        return row("acct_id", r.acctId(), "type_cd", r.typeCd(), "cat_cd", r.catCd(), "balance", r.balance());
    }),
    TRANSACTION("transaction", null, false, List.of("tran_id"), line -> {
        TransactionRecord r = TransactionRecord.parse(line);
        return row("tran_id", r.tranId(), "type_cd", r.typeCd(), "cat_cd", r.catCd(), "source", r.source(),
                "description", r.description(), "amt", r.amt(), "merchant_id", r.merchantId(), "merchant_name",
                r.merchantName(), "merchant_city", r.merchantCity(), "merchant_zip", r.merchantZip(), "card_num",
                r.cardNum(), "orig_ts", ts(r.origTs()), "proc_ts", ts(r.procTs()));
    });

    private final String table;
    private final String seedFile;
    private final boolean versioned;
    private final List<String> primaryKey;
    private final Function<String, Map<String, Object>> parser;

    TableSpec(String table, String seedFile, boolean versioned, List<String> primaryKey,
            Function<String, Map<String, Object>> parser) {
        this.table = table;
        this.seedFile = seedFile;
        this.versioned = versioned;
        this.primaryKey = primaryKey;
        this.parser = parser;
    }

    public String table() {
        return table;
    }

    public Optional<String> seedFile() {
        return Optional.ofNullable(seedFile);
    }

    public boolean versioned() {
        return versioned;
    }

    public List<String> primaryKey() {
        return primaryKey;
    }

    public Map<String, Object> parse(String line) {
        return parser.apply(line);
    }

    public static Optional<TableSpec> of(String table) {
        return Arrays.stream(values()).filter(t -> t.table.equals(table)).findFirst();
    }

    private static Map<String, Object> row(Object... kv) {
        Map<String, Object> m = new LinkedHashMap<>();
        for (int i = 0; i < kv.length; i += 2) {
            m.put((String) kv[i], kv[i + 1]);
        }
        return m;
    }

    private static Date date(LocalDate d) {
        return d == null ? null : Date.valueOf(d);
    }

    private static Timestamp ts(LocalDateTime t) {
        return t == null ? null : Timestamp.valueOf(t);
    }
}
