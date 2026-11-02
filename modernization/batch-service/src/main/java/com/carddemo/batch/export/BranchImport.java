package com.carddemo.batch.export;

import static com.carddemo.common.record.layout.ExportRecordLayout.*;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.Field;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * {@code CBIMPORT}: splits EXPFILE (read in {@code EXPORT-SEQUENCE-NUM} order) back into the customer,
 * account, cross-reference, transaction and card files; a record of any other type goes to ERROUT
 * (132 bytes) and the run still ends with RC 0. 3000-VALIDATE-IMPORT only DISPLAYs.
 */
final class BranchImport {

    static final String CUSTOUT = "CUSTOUT";
    static final String ACCTOUT = "ACCTOUT";
    static final String XREFOUT = "XREFOUT";
    static final String TRNXOUT = "TRNXOUT";
    static final String CARDOUT = "CARDOUT";
    static final String ERROUT = "ERROUT";
    static final int ERROR_RECORD_LENGTH = 132;

    record Output(List<CobolRecord> customers, List<CobolRecord> accounts, List<CobolRecord> xrefs,
                  List<CobolRecord> transactions, List<CobolRecord> cards, List<String> errors, List<String> sysout) {
    }

    private BranchImport() {
    }

    /** {@code now} replaces {@code FUNCTION CURRENT-DATE} (the service clock, with its UTC offset). */
    static Output run(List<CobolRecord> export, ZonedDateTime now, CodePage codePage) {
        List<CobolRecord> customers = new ArrayList<>();
        List<CobolRecord> accounts = new ArrayList<>();
        List<CobolRecord> xrefs = new ArrayList<>();
        List<CobolRecord> transactions = new ArrayList<>();
        List<CobolRecord> cards = new ArrayList<>();
        List<String> errors = new ArrayList<>();
        String currentDate = now.format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
                + String.format("%02d", now.getNano() / 10_000_000)
                + now.format(DateTimeFormatter.ofPattern("xx"));
        List<String> sysout = new ArrayList<>();
        sysout.add("CBIMPORT: Starting Customer Data Import");
        sysout.add("CBIMPORT: Import Date: " + now.format(DateTimeFormatter.ISO_LOCAL_DATE));
        sysout.add("CBIMPORT: Import Time: " + now.format(DateTimeFormatter.ofPattern("HH:mm:ss")));
        for (CobolRecord e : export) {
            switch (e.get(EXPORT_REC_TYPE)) {
                case "C" -> {
                    CobolRecord c = outputRecord(CustomerRecordLayout.LAYOUT, codePage)
                            .set(CustomerRecordLayout.CUST_ID, e.get(EXP_CUST_ID));
                    copy(c, CustomerRecordLayout.CUST_FIRST_NAME, e, EXP_CUST_FIRST_NAME);
                    copy(c, CustomerRecordLayout.CUST_MIDDLE_NAME, e, EXP_CUST_MIDDLE_NAME);
                    copy(c, CustomerRecordLayout.CUST_LAST_NAME, e, EXP_CUST_LAST_NAME);
                    copy(c, CustomerRecordLayout.CUST_ADDR_LINE_1, e, EXP_CUST_ADDR_LINE.get(0));
                    copy(c, CustomerRecordLayout.CUST_ADDR_LINE_2, e, EXP_CUST_ADDR_LINE.get(1));
                    copy(c, CustomerRecordLayout.CUST_ADDR_LINE_3, e, EXP_CUST_ADDR_LINE.get(2));
                    copy(c, CustomerRecordLayout.CUST_ADDR_STATE_CD, e, EXP_CUST_ADDR_STATE_CD);
                    copy(c, CustomerRecordLayout.CUST_ADDR_COUNTRY_CD, e, EXP_CUST_ADDR_COUNTRY_CD);
                    copy(c, CustomerRecordLayout.CUST_ADDR_ZIP, e, EXP_CUST_ADDR_ZIP);
                    copy(c, CustomerRecordLayout.CUST_PHONE_NUM_1, e, EXP_CUST_PHONE_NUM.get(0));
                    copy(c, CustomerRecordLayout.CUST_PHONE_NUM_2, e, EXP_CUST_PHONE_NUM.get(1));
                    c.set(CustomerRecordLayout.CUST_SSN, e.get(EXP_CUST_SSN));
                    copy(c, CustomerRecordLayout.CUST_GOVT_ISSUED_ID, e, EXP_CUST_GOVT_ISSUED_ID);
                    copy(c, CustomerRecordLayout.CUST_DOB_YYYY_MM_DD, e, EXP_CUST_DOB_YYYY_MM_DD);
                    copy(c, CustomerRecordLayout.CUST_EFT_ACCOUNT_ID, e, EXP_CUST_EFT_ACCOUNT_ID);
                    copy(c, CustomerRecordLayout.CUST_PRI_CARD_HOLDER_IND, e, EXP_CUST_PRI_CARD_HOLDER_IND);
                    c.set(CustomerRecordLayout.CUST_FICO_CREDIT_SCORE, e.get(EXP_CUST_FICO_CREDIT_SCORE));
                    customers.add(c);
                }
                case "A" -> {
                    CobolRecord a = outputRecord(AccountRecordLayout.LAYOUT, codePage)
                            .set(AccountRecordLayout.ACCT_ID, e.get(EXP_ACCT_ID));
                    copy(a, AccountRecordLayout.ACCT_ACTIVE_STATUS, e, EXP_ACCT_ACTIVE_STATUS);
                    a.set(AccountRecordLayout.ACCT_CURR_BAL, e.get(EXP_ACCT_CURR_BAL))
                            .set(AccountRecordLayout.ACCT_CREDIT_LIMIT, e.get(EXP_ACCT_CREDIT_LIMIT))
                            .set(AccountRecordLayout.ACCT_CASH_CREDIT_LIMIT, e.get(EXP_ACCT_CASH_CREDIT_LIMIT));
                    copy(a, AccountRecordLayout.ACCT_OPEN_DATE, e, EXP_ACCT_OPEN_DATE);
                    copy(a, AccountRecordLayout.ACCT_EXPIRAION_DATE, e, EXP_ACCT_EXPIRAION_DATE);
                    copy(a, AccountRecordLayout.ACCT_REISSUE_DATE, e, EXP_ACCT_REISSUE_DATE);
                    a.set(AccountRecordLayout.ACCT_CURR_CYC_CREDIT, e.get(EXP_ACCT_CURR_CYC_CREDIT))
                            .set(AccountRecordLayout.ACCT_CURR_CYC_DEBIT, e.get(EXP_ACCT_CURR_CYC_DEBIT));
                    copy(a, AccountRecordLayout.ACCT_ADDR_ZIP, e, EXP_ACCT_ADDR_ZIP);
                    copy(a, AccountRecordLayout.ACCT_GROUP_ID, e, EXP_ACCT_GROUP_ID);
                    accounts.add(a);
                }
                case "X" -> {
                    CobolRecord x = outputRecord(CardXrefRecordLayout.LAYOUT, codePage);
                    copy(x, CardXrefRecordLayout.XREF_CARD_NUM, e, EXP_XREF_CARD_NUM);
                    x.set(CardXrefRecordLayout.XREF_CUST_ID, e.get(EXP_XREF_CUST_ID))
                            .set(CardXrefRecordLayout.XREF_ACCT_ID, e.get(EXP_XREF_ACCT_ID));
                    xrefs.add(x);
                }
                case "T" -> {
                    CobolRecord t = outputRecord(TranRecordLayout.LAYOUT, codePage);
                    copy(t, TranRecordLayout.TRAN_ID, e, EXP_TRAN_ID);
                    copy(t, TranRecordLayout.TRAN_TYPE_CD, e, EXP_TRAN_TYPE_CD);
                    t.set(TranRecordLayout.TRAN_CAT_CD, e.get(EXP_TRAN_CAT_CD));
                    copy(t, TranRecordLayout.TRAN_SOURCE, e, EXP_TRAN_SOURCE);
                    copy(t, TranRecordLayout.TRAN_DESC, e, EXP_TRAN_DESC);
                    t.set(TranRecordLayout.TRAN_AMT, e.get(EXP_TRAN_AMT))
                            .set(TranRecordLayout.TRAN_MERCHANT_ID, e.get(EXP_TRAN_MERCHANT_ID));
                    copy(t, TranRecordLayout.TRAN_MERCHANT_NAME, e, EXP_TRAN_MERCHANT_NAME);
                    copy(t, TranRecordLayout.TRAN_MERCHANT_CITY, e, EXP_TRAN_MERCHANT_CITY);
                    copy(t, TranRecordLayout.TRAN_MERCHANT_ZIP, e, EXP_TRAN_MERCHANT_ZIP);
                    copy(t, TranRecordLayout.TRAN_CARD_NUM, e, EXP_TRAN_CARD_NUM);
                    copy(t, TranRecordLayout.TRAN_ORIG_TS, e, EXP_TRAN_ORIG_TS);
                    copy(t, TranRecordLayout.TRAN_PROC_TS, e, EXP_TRAN_PROC_TS);
                    transactions.add(t);
                }
                case "D" -> {
                    CobolRecord d = outputRecord(CardRecordLayout.LAYOUT, codePage);
                    copy(d, CardRecordLayout.CARD_NUM, e, EXP_CARD_NUM);
                    d.set(CardRecordLayout.CARD_ACCT_ID, e.get(EXP_CARD_ACCT_ID))
                            .set(CardRecordLayout.CARD_CVV_CD, e.get(EXP_CARD_CVV_CD));
                    copy(d, CardRecordLayout.CARD_EMBOSSED_NAME, e, EXP_CARD_EMBOSSED_NAME);
                    copy(d, CardRecordLayout.CARD_EXPIRAION_DATE, e, EXP_CARD_EXPIRAION_DATE);
                    copy(d, CardRecordLayout.CARD_ACTIVE_STATUS, e, EXP_CARD_ACTIVE_STATUS);
                    cards.add(d);
                }
                default -> errors.add(errorRecord(currentDate, e));
            }
        }
        sysout.add("CBIMPORT: Import validation completed");
        sysout.add("CBIMPORT: No validation errors detected");
        sysout.add("CBIMPORT: Import completed");
        sysout.add("CBIMPORT: Total Records Read: " + BranchExport.count(export.size()));
        sysout.add("CBIMPORT: Customers Imported: " + BranchExport.count(customers.size()));
        sysout.add("CBIMPORT: Accounts Imported: " + BranchExport.count(accounts.size()));
        sysout.add("CBIMPORT: XRefs Imported: " + BranchExport.count(xrefs.size()));
        sysout.add("CBIMPORT: Transactions Imported: " + BranchExport.count(transactions.size()));
        sysout.add("CBIMPORT: Cards Imported: " + BranchExport.count(cards.size()));
        sysout.add("CBIMPORT: Errors Written: " + BranchExport.count(errors.size()));
        sysout.add("CBIMPORT: Unknown Record Types: " + BranchExport.count(errors.size()));
        return new Output(customers, accounts, xrefs, transactions, cards, errors, sysout);
    }

    /**
     * The FD record of an output file after {@code INITIALIZE}: named fields blank / zero, FILLER untouched.
     * The record area starts as binary zeros, so FILLER is written as X'00', which is what the GnuCOBOL
     * reference produces. <b>Hypothesis:</b> on z/OS FILLER would hold whatever the output buffer held.
     */
    static CobolRecord outputRecord(RecordLayout layout, CodePage codePage) {
        CobolRecord record = CobolRecord.of(layout, codePage, new byte[layout.length()]);
        for (Field field : layout.fields()) {
            if (field.name().equals("FILLER")) {
                continue;
            }
            if (field instanceof AlphanumericField alpha) {
                record.set(alpha, "");
            } else {
                record.set((NumericField) field, BigDecimal.ZERO);
            }
        }
        return record;
    }

    /**
     * {@code WS-ERROR-RECORD} written into the 132-byte ERROUT record: CURRENT-DATE (21 characters in an
     * X(26) field), '|', record type, '|', EXPORT-SEQUENCE-NUM as {@code PIC 9(07)}, '|', the message, blanks.
     */
    static String errorRecord(String currentDate, CobolRecord export) {
        String text = pad(currentDate, 26) + "|" + export.get(EXPORT_REC_TYPE) + "|"
                + String.format("%07d", export.getLong(EXPORT_SEQUENCE_NUM) % 10_000_000L) + "|"
                + pad("Unknown record type encountered", 50) + " ".repeat(43);
        return pad(text, ERROR_RECORD_LENGTH);
    }

    private static void copy(CobolRecord target, AlphanumericField to, CobolRecord source, AlphanumericField from) {
        target.set(to, source.get(from));
    }

    private static String pad(String value, int width) {
        return value.length() >= width ? value.substring(0, width) : value + " ".repeat(width - value.length());
    }
}
