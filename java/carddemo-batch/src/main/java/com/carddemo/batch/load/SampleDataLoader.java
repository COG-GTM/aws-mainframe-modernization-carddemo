package com.carddemo.batch.load;

import com.carddemo.data.Account;
import com.carddemo.data.Card;
import com.carddemo.data.CardXref;
import com.carddemo.data.Customer;
import com.carddemo.data.DailyTransaction;
import com.carddemo.data.DisclosureGroup;
import com.carddemo.data.DisclosureGroupId;
import com.carddemo.data.TranCatBalance;
import com.carddemo.data.TranCatBalanceId;
import com.carddemo.data.TranCategory;
import com.carddemo.data.TranCategoryId;
import com.carddemo.data.TranType;
import com.carddemo.data.Transaction;
import com.carddemo.data.UserSecurity;
import com.carddemo.legacyio.CardDemoLayouts;
import com.carddemo.legacyio.FileRecord;
import com.carddemo.legacyio.LegacyFileCodec;
import com.carddemo.legacyio.Record;
import com.carddemo.legacyio.RecordLayout;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * Replaces the contents of every CardDemo table with the sample data shipped in {@code app/data}, the work the
 * ACCTFILE, CARDFILE, CUSTFILE, XREFFILE, TRANFILE, TRANTYPE, TRANCATG, DISCGRP, TCATBALF and DUSRSECJ jobs do with
 * IDCAMS DELETE/DEFINE/REPRO. The line-sequential ASCII extracts are read for every file except USRSEC, which only
 * exists as the EBCDIC extract of the records DUSRSECJ writes. TRANFILE seeds TRANSACT from DALYTRAN.PS.INIT, a single
 * low-value record, so TRANSACT starts empty; DALYTRAN itself is loaded into {@code daily_transactions}.
 */
@Component
public class SampleDataLoader {

    static final DateTimeFormatter TIMESTAMP = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS");

    private static final List<Class<?>> DELETE_ORDER = List.of(
            CardXref.class, Card.class, Customer.class, Account.class, Transaction.class, DailyTransaction.class,
            TranCatBalance.class, DisclosureGroup.class, TranCategory.class, TranType.class, UserSecurity.class);

    @PersistenceContext
    private EntityManager entityManager;

    private final PasswordEncoder passwordEncoder;

    public SampleDataLoader(PasswordEncoder passwordEncoder) {
        this.passwordEncoder = passwordEncoder;
    }

    /**
     * Deletes every row and loads the sample files under {@code dataDir} (the {@code app/data} directory) in one
     * transaction.
     *
     * @return records loaded per table, in load order
     */
    @Transactional
    public Map<String, Integer> replaceAll(Path dataDir) {
        for (Class<?> entity : DELETE_ORDER) {
            entityManager.createQuery("delete from " + entity.getSimpleName()).executeUpdate();
        }
        Path ascii = dataDir.resolve("ASCII");
        Map<String, Integer> counts = new LinkedHashMap<>();
        counts.put("accounts", load(ascii.resolve("acctdata.txt"), CardDemoLayouts.ACCOUNT, SampleDataLoader::account));
        counts.put("customers", load(ascii.resolve("custdata.txt"), CardDemoLayouts.CUSTOMER, SampleDataLoader::customer));
        counts.put("cards", load(ascii.resolve("carddata.txt"), CardDemoLayouts.CARD, SampleDataLoader::card));
        counts.put("card_xrefs", load(ascii.resolve("cardxref.txt"), CardDemoLayouts.CARD_XREF, SampleDataLoader::cardXref));
        counts.put("transactions", load(dataDir.resolve("EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS.INIT"),
                LegacyFileCodec.ebcdic(), CardDemoLayouts.TRANSACTION, SampleDataLoader::transaction));
        counts.put("daily_transactions", loadDailyTransactions(ascii.resolve("dailytran.txt")));
        counts.put("tran_types", load(ascii.resolve("trantype.txt"), CardDemoLayouts.TRAN_TYPE, SampleDataLoader::tranType));
        counts.put("tran_categories",
                load(ascii.resolve("trancatg.txt"), CardDemoLayouts.TRAN_CATEGORY, SampleDataLoader::tranCategory));
        counts.put("disclosure_groups",
                load(ascii.resolve("discgrp.txt"), CardDemoLayouts.DISCLOSURE_GROUP, SampleDataLoader::disclosureGroup));
        counts.put("tran_cat_balances",
                load(ascii.resolve("tcatbal.txt"), CardDemoLayouts.TRAN_CAT_BALANCE, SampleDataLoader::tranCatBalance));
        counts.put("users", load(dataDir.resolve("EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS"), LegacyFileCodec.ebcdic(),
                CardDemoLayouts.USER_SECURITY, this::user));
        entityManager.flush();
        entityManager.clear();
        return counts;
    }

    private int load(Path file, RecordLayout layout, Function<Record, Object> mapper) {
        return load(file, LegacyFileCodec.ascii(), layout, mapper);
    }

    private int load(Path file, LegacyFileCodec codec, RecordLayout layout, Function<Record, Object> mapper) {
        int count = 0;
        for (FileRecord fileRecord : read(file, codec, layout)) {
            Record record = fileRecord.record();
            if (isBlank(record)) {
                continue;
            }
            entityManager.persist(mapper.apply(record));
            count++;
        }
        return count;
    }

    private int loadDailyTransactions(Path file) {
        int seq = 0;
        for (FileRecord fileRecord : read(file, LegacyFileCodec.ascii(), CardDemoLayouts.DAILY_TRANSACTION)) {
            entityManager.persist(dailyTransaction(++seq, fileRecord.record()));
        }
        return seq;
    }

    private static List<FileRecord> read(Path file, LegacyFileCodec codec, RecordLayout layout) {
        try {
            return codec.read(file, layout);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read " + file, e);
        }
    }

    /**
     * A record of only spaces or low-values, like the one in DALYTRAN.PS.INIT, carries no data; its numeric fields do
     * not decode.
     */
    private static boolean isBlank(Record record) {
        return record.toMap().values().stream().allMatch(value -> value instanceof String text
                ? text.chars().allMatch(c -> c == ' ' || c == 0)
                : !(value instanceof Number));
    }

    static Account account(Record r) {
        Account a = new Account();
        a.setAcctId(r.getLong("ACCT-ID"));
        a.setActiveStatus(text(r, "ACCT-ACTIVE-STATUS"));
        a.setCurrBal(r.getDecimal("ACCT-CURR-BAL"));
        a.setCreditLimit(r.getDecimal("ACCT-CREDIT-LIMIT"));
        a.setCashCreditLimit(r.getDecimal("ACCT-CASH-CREDIT-LIMIT"));
        a.setOpenDate(date(r, "ACCT-OPEN-DATE"));
        a.setExpirationDate(date(r, "ACCT-EXPIRAION-DATE"));
        a.setReissueDate(date(r, "ACCT-REISSUE-DATE"));
        a.setCurrCycCredit(r.getDecimal("ACCT-CURR-CYC-CREDIT"));
        a.setCurrCycDebit(r.getDecimal("ACCT-CURR-CYC-DEBIT"));
        a.setAddrZip(text(r, "ACCT-ADDR-ZIP"));
        a.setGroupId(text(r, "ACCT-GROUP-ID"));
        return a;
    }

    static Customer customer(Record r) {
        Customer c = new Customer();
        c.setCustId(r.getLong("CUST-ID"));
        c.setFirstName(text(r, "CUST-FIRST-NAME"));
        c.setMiddleName(text(r, "CUST-MIDDLE-NAME"));
        c.setLastName(text(r, "CUST-LAST-NAME"));
        c.setAddrLine1(text(r, "CUST-ADDR-LINE-1"));
        c.setAddrLine2(text(r, "CUST-ADDR-LINE-2"));
        c.setAddrLine3(text(r, "CUST-ADDR-LINE-3"));
        c.setAddrStateCd(text(r, "CUST-ADDR-STATE-CD"));
        c.setAddrCountryCd(text(r, "CUST-ADDR-COUNTRY-CD"));
        c.setAddrZip(text(r, "CUST-ADDR-ZIP"));
        c.setPhoneNum1(text(r, "CUST-PHONE-NUM-1"));
        c.setPhoneNum2(text(r, "CUST-PHONE-NUM-2"));
        c.setSsn(integer(r, "CUST-SSN"));
        c.setGovtIssuedId(text(r, "CUST-GOVT-ISSUED-ID"));
        c.setDob(date(r, "CUST-DOB-YYYY-MM-DD"));
        c.setEftAccountId(text(r, "CUST-EFT-ACCOUNT-ID"));
        c.setPriCardHolderInd(text(r, "CUST-PRI-CARD-HOLDER-IND"));
        c.setFicoCreditScore(integer(r, "CUST-FICO-CREDIT-SCORE"));
        return c;
    }

    static Card card(Record r) {
        Card c = new Card();
        c.setCardNum(text(r, "CARD-NUM"));
        c.setAcctId(r.getLong("CARD-ACCT-ID"));
        c.setCvvCd(integer(r, "CARD-CVV-CD"));
        c.setEmbossedName(text(r, "CARD-EMBOSSED-NAME"));
        c.setExpirationDate(date(r, "CARD-EXPIRAION-DATE"));
        c.setActiveStatus(text(r, "CARD-ACTIVE-STATUS"));
        return c;
    }

    static CardXref cardXref(Record r) {
        CardXref x = new CardXref();
        x.setCardNum(text(r, "XREF-CARD-NUM"));
        x.setCustId(r.getLong("XREF-CUST-ID"));
        x.setAcctId(r.getLong("XREF-ACCT-ID"));
        return x;
    }

    static Transaction transaction(Record r) {
        Transaction t = new Transaction();
        t.setTranId(text(r, "TRAN-ID"));
        t.setTypeCd(text(r, "TRAN-TYPE-CD"));
        t.setCatCd(integer(r, "TRAN-CAT-CD"));
        t.setSource(text(r, "TRAN-SOURCE"));
        t.setDescription(text(r, "TRAN-DESC"));
        t.setAmount(r.getDecimal("TRAN-AMT"));
        t.setMerchantId(r.getLong("TRAN-MERCHANT-ID"));
        t.setMerchantName(text(r, "TRAN-MERCHANT-NAME"));
        t.setMerchantCity(text(r, "TRAN-MERCHANT-CITY"));
        t.setMerchantZip(text(r, "TRAN-MERCHANT-ZIP"));
        t.setCardNum(text(r, "TRAN-CARD-NUM"));
        t.setOrigTs(timestamp(r, "TRAN-ORIG-TS"));
        t.setProcTs(timestamp(r, "TRAN-PROC-TS"));
        return t;
    }

    static DailyTransaction dailyTransaction(int recordSeq, Record r) {
        DailyTransaction t = new DailyTransaction();
        t.setRecordSeq(recordSeq);
        t.setTranId(text(r, "DALYTRAN-ID"));
        t.setTypeCd(text(r, "DALYTRAN-TYPE-CD"));
        t.setCatCd(integer(r, "DALYTRAN-CAT-CD"));
        t.setSource(text(r, "DALYTRAN-SOURCE"));
        t.setDescription(text(r, "DALYTRAN-DESC"));
        t.setAmount(r.getDecimal("DALYTRAN-AMT"));
        t.setMerchantId(r.getLong("DALYTRAN-MERCHANT-ID"));
        t.setMerchantName(text(r, "DALYTRAN-MERCHANT-NAME"));
        t.setMerchantCity(text(r, "DALYTRAN-MERCHANT-CITY"));
        t.setMerchantZip(text(r, "DALYTRAN-MERCHANT-ZIP"));
        t.setCardNum(text(r, "DALYTRAN-CARD-NUM"));
        t.setOrigTs(timestamp(r, "DALYTRAN-ORIG-TS"));
        t.setProcTs(timestamp(r, "DALYTRAN-PROC-TS"));
        return t;
    }

    static TranType tranType(Record r) {
        TranType t = new TranType();
        t.setTypeCd(text(r, "TRAN-TYPE"));
        t.setDescription(text(r, "TRAN-TYPE-DESC"));
        return t;
    }

    static TranCategory tranCategory(Record r) {
        TranCategory c = new TranCategory();
        c.setId(new TranCategoryId(text(r, "TRAN-TYPE-CD"), integer(r, "TRAN-CAT-CD")));
        c.setDescription(text(r, "TRAN-CAT-TYPE-DESC"));
        return c;
    }

    static DisclosureGroup disclosureGroup(Record r) {
        DisclosureGroup g = new DisclosureGroup();
        g.setId(new DisclosureGroupId(
                text(r, "DIS-ACCT-GROUP-ID"), text(r, "DIS-TRAN-TYPE-CD"), integer(r, "DIS-TRAN-CAT-CD")));
        g.setIntRate(r.getDecimal("DIS-INT-RATE"));
        return g;
    }

    static TranCatBalance tranCatBalance(Record r) {
        TranCatBalance b = new TranCatBalance();
        b.setId(new TranCatBalanceId(r.getLong("TRANCAT-ACCT-ID"), text(r, "TRANCAT-TYPE-CD"), integer(r, "TRANCAT-CD")));
        b.setBalance(r.getDecimal("TRAN-CAT-BAL"));
        return b;
    }

    private UserSecurity user(Record r) {
        UserSecurity u = new UserSecurity();
        u.setUserId(text(r, "SEC-USR-ID"));
        u.setFirstName(text(r, "SEC-USR-FNAME"));
        u.setLastName(text(r, "SEC-USR-LNAME"));
        u.setPasswordHash(passwordEncoder.encode(text(r, "SEC-USR-PWD")));
        u.setUserType(text(r, "SEC-USR-TYPE"));
        return u;
    }

    private static String text(Record r, String field) {
        return r.getString(field).stripTrailing();
    }

    private static Integer integer(Record r, String field) {
        return r.getDecimal(field).intValueExact();
    }

    private static LocalDate date(Record r, String field) {
        String value = r.getString(field).strip();
        return value.isEmpty() ? null : LocalDate.parse(value);
    }

    private static LocalDateTime timestamp(Record r, String field) {
        String value = r.getString(field).strip();
        return value.isEmpty() ? null : LocalDateTime.parse(value, TIMESTAMP);
    }
}
