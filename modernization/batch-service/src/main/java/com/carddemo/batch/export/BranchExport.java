package com.carddemo.batch.export;

import static com.carddemo.common.record.layout.ExportRecordLayout.*;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import com.carddemo.common.record.layout.ExportRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * {@code CBEXPORT}: one EXPFILE record ({@code CVEXPORT}, 500 bytes) per customer, account, card
 * cross reference, transaction and card, in that order and each file in key order, numbered by
 * {@code EXPORT-SEQUENCE-NUM} (binary) from 1, stamped with the run time, branch {@code 0001},
 * region {@code NORTH}. The SYSOUT lines are the program's DISPLAYs.
 */
final class BranchExport {

    record Inputs(List<CobolRecord> customers, List<CobolRecord> accounts, List<CobolRecord> xrefs,
                  List<CobolRecord> transactions, List<CobolRecord> cards) {
    }

    record Output(List<CobolRecord> records, List<String> sysout) {
    }

    private BranchExport() {
    }

    /** {@code now} replaces {@code ACCEPT ... FROM DATE YYYYMMDD} / {@code FROM TIME} (the service clock). */
    static Output run(Inputs in, LocalDateTime now, CodePage codePage) {
        String date = now.format(DateTimeFormatter.ISO_LOCAL_DATE);
        String time = now.format(DateTimeFormatter.ofPattern("HH:mm:ss"));
        String timestamp = date + " " + time + ".00";
        List<String> sysout = new ArrayList<>();
        List<CobolRecord> out = new ArrayList<>();
        sysout.add("CBEXPORT: Starting Customer Data Export");
        sysout.add("CBEXPORT: Export Date: " + date);
        sysout.add("CBEXPORT: Export Time: " + time);

        sysout.add("CBEXPORT: Processing customer records");
        for (CobolRecord c : in.customers()) {
            CobolRecord r = header(out, "C", timestamp, codePage)
                    .set(EXP_CUST_ID, c.get(CustomerRecordLayout.CUST_ID));
            copy(r, EXP_CUST_FIRST_NAME, c, CustomerRecordLayout.CUST_FIRST_NAME);
            copy(r, EXP_CUST_MIDDLE_NAME, c, CustomerRecordLayout.CUST_MIDDLE_NAME);
            copy(r, EXP_CUST_LAST_NAME, c, CustomerRecordLayout.CUST_LAST_NAME);
            copy(r, EXP_CUST_ADDR_LINE.get(0), c, CustomerRecordLayout.CUST_ADDR_LINE_1);
            copy(r, EXP_CUST_ADDR_LINE.get(1), c, CustomerRecordLayout.CUST_ADDR_LINE_2);
            copy(r, EXP_CUST_ADDR_LINE.get(2), c, CustomerRecordLayout.CUST_ADDR_LINE_3);
            copy(r, EXP_CUST_ADDR_STATE_CD, c, CustomerRecordLayout.CUST_ADDR_STATE_CD);
            copy(r, EXP_CUST_ADDR_COUNTRY_CD, c, CustomerRecordLayout.CUST_ADDR_COUNTRY_CD);
            copy(r, EXP_CUST_ADDR_ZIP, c, CustomerRecordLayout.CUST_ADDR_ZIP);
            copy(r, EXP_CUST_PHONE_NUM.get(0), c, CustomerRecordLayout.CUST_PHONE_NUM_1);
            copy(r, EXP_CUST_PHONE_NUM.get(1), c, CustomerRecordLayout.CUST_PHONE_NUM_2);
            r.set(EXP_CUST_SSN, c.get(CustomerRecordLayout.CUST_SSN));
            copy(r, EXP_CUST_GOVT_ISSUED_ID, c, CustomerRecordLayout.CUST_GOVT_ISSUED_ID);
            copy(r, EXP_CUST_DOB_YYYY_MM_DD, c, CustomerRecordLayout.CUST_DOB_YYYY_MM_DD);
            copy(r, EXP_CUST_EFT_ACCOUNT_ID, c, CustomerRecordLayout.CUST_EFT_ACCOUNT_ID);
            copy(r, EXP_CUST_PRI_CARD_HOLDER_IND, c, CustomerRecordLayout.CUST_PRI_CARD_HOLDER_IND);
            r.set(EXP_CUST_FICO_CREDIT_SCORE, c.get(CustomerRecordLayout.CUST_FICO_CREDIT_SCORE));
        }
        sysout.add("CBEXPORT: Customers exported: " + count(in.customers().size()));

        sysout.add("CBEXPORT: Processing account records");
        for (CobolRecord a : in.accounts()) {
            CobolRecord r = header(out, "A", timestamp, codePage)
                    .set(EXP_ACCT_ID, a.get(AccountRecordLayout.ACCT_ID));
            copy(r, EXP_ACCT_ACTIVE_STATUS, a, AccountRecordLayout.ACCT_ACTIVE_STATUS);
            r.set(EXP_ACCT_CURR_BAL, a.get(AccountRecordLayout.ACCT_CURR_BAL))
                    .set(EXP_ACCT_CREDIT_LIMIT, a.get(AccountRecordLayout.ACCT_CREDIT_LIMIT))
                    .set(EXP_ACCT_CASH_CREDIT_LIMIT, a.get(AccountRecordLayout.ACCT_CASH_CREDIT_LIMIT));
            copy(r, EXP_ACCT_OPEN_DATE, a, AccountRecordLayout.ACCT_OPEN_DATE);
            copy(r, EXP_ACCT_EXPIRAION_DATE, a, AccountRecordLayout.ACCT_EXPIRAION_DATE);
            copy(r, EXP_ACCT_REISSUE_DATE, a, AccountRecordLayout.ACCT_REISSUE_DATE);
            r.set(EXP_ACCT_CURR_CYC_CREDIT, a.get(AccountRecordLayout.ACCT_CURR_CYC_CREDIT))
                    .set(EXP_ACCT_CURR_CYC_DEBIT, a.get(AccountRecordLayout.ACCT_CURR_CYC_DEBIT));
            copy(r, EXP_ACCT_ADDR_ZIP, a, AccountRecordLayout.ACCT_ADDR_ZIP);
            copy(r, EXP_ACCT_GROUP_ID, a, AccountRecordLayout.ACCT_GROUP_ID);
        }
        sysout.add("CBEXPORT: Accounts exported: " + count(in.accounts().size()));

        sysout.add("CBEXPORT: Processing cross-reference records");
        for (CobolRecord x : in.xrefs()) {
            CobolRecord r = header(out, "X", timestamp, codePage);
            copy(r, EXP_XREF_CARD_NUM, x, CardXrefRecordLayout.XREF_CARD_NUM);
            r.set(EXP_XREF_CUST_ID, x.get(CardXrefRecordLayout.XREF_CUST_ID))
                    .set(EXP_XREF_ACCT_ID, x.get(CardXrefRecordLayout.XREF_ACCT_ID));
        }
        sysout.add("CBEXPORT: Cross-references exported: " + count(in.xrefs().size()));

        sysout.add("CBEXPORT: Processing transaction records");
        for (CobolRecord t : in.transactions()) {
            CobolRecord r = header(out, "T", timestamp, codePage);
            copy(r, EXP_TRAN_ID, t, TranRecordLayout.TRAN_ID);
            copy(r, EXP_TRAN_TYPE_CD, t, TranRecordLayout.TRAN_TYPE_CD);
            r.set(EXP_TRAN_CAT_CD, t.get(TranRecordLayout.TRAN_CAT_CD));
            copy(r, EXP_TRAN_SOURCE, t, TranRecordLayout.TRAN_SOURCE);
            copy(r, EXP_TRAN_DESC, t, TranRecordLayout.TRAN_DESC);
            r.set(EXP_TRAN_AMT, t.get(TranRecordLayout.TRAN_AMT))
                    .set(EXP_TRAN_MERCHANT_ID, t.get(TranRecordLayout.TRAN_MERCHANT_ID));
            copy(r, EXP_TRAN_MERCHANT_NAME, t, TranRecordLayout.TRAN_MERCHANT_NAME);
            copy(r, EXP_TRAN_MERCHANT_CITY, t, TranRecordLayout.TRAN_MERCHANT_CITY);
            copy(r, EXP_TRAN_MERCHANT_ZIP, t, TranRecordLayout.TRAN_MERCHANT_ZIP);
            copy(r, EXP_TRAN_CARD_NUM, t, TranRecordLayout.TRAN_CARD_NUM);
            copy(r, EXP_TRAN_ORIG_TS, t, TranRecordLayout.TRAN_ORIG_TS);
            copy(r, EXP_TRAN_PROC_TS, t, TranRecordLayout.TRAN_PROC_TS);
        }
        sysout.add("CBEXPORT: Transactions exported: " + count(in.transactions().size()));

        sysout.add("CBEXPORT: Processing card records");
        for (CobolRecord d : in.cards()) {
            CobolRecord r = header(out, "D", timestamp, codePage);
            copy(r, EXP_CARD_NUM, d, CardRecordLayout.CARD_NUM);
            r.set(EXP_CARD_ACCT_ID, d.get(CardRecordLayout.CARD_ACCT_ID))
                    .set(EXP_CARD_CVV_CD, d.get(CardRecordLayout.CARD_CVV_CD));
            copy(r, EXP_CARD_EMBOSSED_NAME, d, CardRecordLayout.CARD_EMBOSSED_NAME);
            copy(r, EXP_CARD_EXPIRAION_DATE, d, CardRecordLayout.CARD_EXPIRAION_DATE);
            copy(r, EXP_CARD_ACTIVE_STATUS, d, CardRecordLayout.CARD_ACTIVE_STATUS);
        }
        sysout.add("CBEXPORT: Cards exported: " + count(in.cards().size()));

        sysout.add("CBEXPORT: Export completed");
        sysout.add("CBEXPORT: Customers Exported: " + count(in.customers().size()));
        sysout.add("CBEXPORT: Accounts Exported: " + count(in.accounts().size()));
        sysout.add("CBEXPORT: XRefs Exported: " + count(in.xrefs().size()));
        sysout.add("CBEXPORT: Transactions Exported: " + count(in.transactions().size()));
        sysout.add("CBEXPORT: Cards Exported: " + count(in.cards().size()));
        sysout.add("CBEXPORT: Total Records Exported: " + count(out.size()));
        return new Output(out, sysout);
    }

    /** {@code INITIALIZE EXPORT-RECORD}, record type, timestamp, next sequence number, branch and region. */
    private static CobolRecord header(List<CobolRecord> out, String type, String timestamp, CodePage codePage) {
        CobolRecord r = CobolRecord.initialized(ExportRecordLayout.LAYOUT, codePage)
                .set(EXPORT_REC_TYPE, type)
                .set(EXPORT_TIMESTAMP, timestamp)
                .set(EXPORT_SEQUENCE_NUM, out.size() + 1L)
                .set(EXPORT_BRANCH_ID, "0001")
                .set(EXPORT_REGION_CODE, "NORTH");
        out.add(r);
        return r;
    }

    private static void copy(CobolRecord target, AlphanumericField to, CobolRecord source, AlphanumericField from) {
        target.set(to, source.get(from));
    }

    /** A {@code PIC 9(09)} counter as DISPLAY shows it. */
    static String count(long value) {
        return String.format("%09d", value % 1_000_000_000L);
    }
}
