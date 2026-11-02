package com.carddemo.batch.reader;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.Field;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.common.record.Usage;
import com.carddemo.common.record.layout.AccountRecordLayout;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;

/**
 * {@code CBACT01C}: reads ACCTFILE in key order and for each account DISPLAYs its fields, writes the
 * OUTFILE record (107 bytes), the ARRYFILE record (110 bytes) and two RECFM=V records to VBRCFILE.
 * The Assembler date routine {@code COBDATFT} it calls is replaced by {@code java.time} (see
 * {@link #reissueDate}), not ported.
 */
final class AccountFileDump {

    static final String PROGRAM = "CBACT01C";

    /** {@code OUT-ACCT-REC} (FD OUT-FILE, LRECL 107). */
    static final class Out {
        private static final RecordLayout.Builder B = RecordLayout.builder("CBACT01C-OUTFILE", "OUT-ACCT-REC");
        static final NumericField ACCT_ID = B.numeric("OUT-ACCT-ID", "9(11)");
        static final com.carddemo.common.record.AlphanumericField ACTIVE_STATUS = B.alpha("OUT-ACCT-ACTIVE-STATUS", "X(01)");
        static final NumericField CURR_BAL = B.numeric("OUT-ACCT-CURR-BAL", "S9(10)V99");
        static final NumericField CREDIT_LIMIT = B.numeric("OUT-ACCT-CREDIT-LIMIT", "S9(10)V99");
        static final NumericField CASH_CREDIT_LIMIT = B.numeric("OUT-ACCT-CASH-CREDIT-LIMIT", "S9(10)V99");
        static final com.carddemo.common.record.AlphanumericField OPEN_DATE = B.alpha("OUT-ACCT-OPEN-DATE", "X(10)");
        static final com.carddemo.common.record.AlphanumericField EXPIRAION_DATE = B.alpha("OUT-ACCT-EXPIRAION-DATE", "X(10)");
        static final com.carddemo.common.record.AlphanumericField REISSUE_DATE = B.alpha("OUT-ACCT-REISSUE-DATE", "X(10)");
        static final NumericField CURR_CYC_CREDIT = B.numeric("OUT-ACCT-CURR-CYC-CREDIT", "S9(10)V99");
        static final NumericField CURR_CYC_DEBIT = B.numeric("OUT-ACCT-CURR-CYC-DEBIT", "S9(10)V99 COMP-3");
        static final com.carddemo.common.record.AlphanumericField GROUP_ID = B.alpha("OUT-ACCT-GROUP-ID", "X(10)");
        static final RecordLayout LAYOUT = B.build(107);
    }

    /** {@code ARR-ARRAY-REC} (FD ARRY-FILE, LRECL 110): five balance / cycle-debit pairs. */
    static final class Arry {
        private static final RecordLayout.Builder B = RecordLayout.builder("CBACT01C-ARRYFILE", "ARR-ARRAY-REC");
        static final NumericField ACCT_ID = B.numeric("ARR-ACCT-ID", "9(11)");
        static final List<NumericField> CURR_BAL = new ArrayList<>();
        static final List<NumericField> CURR_CYC_DEBIT = new ArrayList<>();
        static {
            for (int i = 1; i <= 5; i++) {
                CURR_BAL.add(B.numeric("ARR-ACCT-CURR-BAL(" + i + ")", "S9(10)V99"));
                CURR_CYC_DEBIT.add(B.numeric("ARR-ACCT-CURR-CYC-DEBIT(" + i + ")", "S9(10)V99 COMP-3"));
            }
        }
        static final com.carddemo.common.record.AlphanumericField FILLER = B.alpha("ARR-FILLER", "X(04)");
        static final RecordLayout LAYOUT = B.build(110);
    }

    record Output(List<CobolRecord> outRecords, List<CobolRecord> arrayRecords, List<byte[]> variableRecords,
                  List<String> sysout) {
    }

    private static final String[] LABELS = {"ACCT-ID", "ACCT-ACTIVE-STATUS", "ACCT-CURR-BAL", "ACCT-CREDIT-LIMIT",
            "ACCT-CASH-CREDIT-LIMIT", "ACCT-OPEN-DATE", "ACCT-EXPIRAION-DATE", "ACCT-REISSUE-DATE",
            "ACCT-CURR-CYC-CREDIT", "ACCT-CURR-CYC-DEBIT", "ACCT-GROUP-ID"};

    private AccountFileDump() {
    }

    static Output run(List<CobolRecord> accounts, CodePage codePage) {
        List<String> sysout = new ArrayList<>();
        List<CobolRecord> outs = new ArrayList<>();
        List<CobolRecord> arrays = new ArrayList<>();
        List<byte[]> variable = new ArrayList<>();
        sysout.add("START OF EXECUTION OF PROGRAM " + PROGRAM);
        // The FD record area of OUT-FILE: kept between records, binary zeros before the first WRITE
        // (what the GnuCOBOL reference shows). OUT-ACCT-CURR-CYC-DEBIT is only moved when the account's
        // cycle debit is zero (1300-POPUL-ACCT-RECORD), so otherwise the previous record's bytes stay.
        CobolRecord out = CobolRecord.of(Out.LAYOUT, codePage, new byte[Out.LAYOUT.length()]);
        for (CobolRecord a : accounts) {
            display(sysout, a);
            out.set(Out.ACCT_ID, a.get(AccountRecordLayout.ACCT_ID))
                    .set(Out.ACTIVE_STATUS, a.get(AccountRecordLayout.ACCT_ACTIVE_STATUS))
                    .set(Out.CURR_BAL, a.get(AccountRecordLayout.ACCT_CURR_BAL))
                    .set(Out.CREDIT_LIMIT, a.get(AccountRecordLayout.ACCT_CREDIT_LIMIT))
                    .set(Out.CASH_CREDIT_LIMIT, a.get(AccountRecordLayout.ACCT_CASH_CREDIT_LIMIT))
                    .set(Out.OPEN_DATE, a.get(AccountRecordLayout.ACCT_OPEN_DATE))
                    .set(Out.EXPIRAION_DATE, a.get(AccountRecordLayout.ACCT_EXPIRAION_DATE))
                    .set(Out.REISSUE_DATE, reissueDate(a.get(AccountRecordLayout.ACCT_REISSUE_DATE)))
                    .set(Out.CURR_CYC_CREDIT, a.get(AccountRecordLayout.ACCT_CURR_CYC_CREDIT));
            if (a.get(AccountRecordLayout.ACCT_CURR_CYC_DEBIT).signum() == 0) {
                out.set(Out.CURR_CYC_DEBIT, new BigDecimal("2525.00"));
            }
            out.set(Out.GROUP_ID, a.get(AccountRecordLayout.ACCT_GROUP_ID));
            outs.add(CobolRecord.of(Out.LAYOUT, codePage, out.toBytes()));

            BigDecimal balance = a.get(AccountRecordLayout.ACCT_CURR_BAL);
            CobolRecord arr = initialize(Arry.LAYOUT, codePage)
                    .set(Arry.ACCT_ID, a.get(AccountRecordLayout.ACCT_ID))
                    .set(Arry.CURR_BAL.get(0), balance)
                    .set(Arry.CURR_CYC_DEBIT.get(0), new BigDecimal("1005.00"))
                    .set(Arry.CURR_BAL.get(1), balance)
                    .set(Arry.CURR_CYC_DEBIT.get(1), new BigDecimal("1525.00"))
                    .set(Arry.CURR_BAL.get(2), new BigDecimal("-1025.00"))
                    .set(Arry.CURR_CYC_DEBIT.get(2), new BigDecimal("-2500.00"));
            arrays.add(arr);

            byte[] bytes = a.toBytes();
            String reissue = a.get(AccountRecordLayout.ACCT_REISSUE_DATE);
            String vb1 = text(a, AccountRecordLayout.ACCT_ID) + text(a, AccountRecordLayout.ACCT_ACTIVE_STATUS);
            String vb2 = text(a, AccountRecordLayout.ACCT_ID) + text(a, AccountRecordLayout.ACCT_CURR_BAL)
                    + text(a, AccountRecordLayout.ACCT_CREDIT_LIMIT) + reissue.substring(0, 4);
            sysout.add("VBRC-REC1:" + vb1);
            sysout.add("VBRC-REC2:" + vb2);
            variable.add(codePage.encodeText(vb1));
            variable.add(codePage.encodeText(vb2));
            sysout.add(codePage.decodeText(bytes, 0, bytes.length));
        }
        sysout.add("END OF EXECUTION OF PROGRAM " + PROGRAM);
        return new Output(outs, arrays, variable, sysout);
    }

    /**
     * {@code CALL 'COBDATFT'} with CODATECN-TYPE '2' (YYYY-MM-DD in) and CODATECN-OUTTYPE '2' (YYYYMMDD
     * out), then {@code MOVE CODATECN-0UT-DATE TO OUT-ACCT-REISSUE-DATE}: the 8-character date followed by
     * the two blank bytes of the untouched output area. The Assembler routine only moves bytes
     * (positions 1-4, 6-7, 9-10) without checking the date; java.time does the conversion for a valid
     * date and the same positional move keeps any other value as the routine would.
     */
    static String reissueDate(String yyyyMmDd) {
        try {
            return LocalDate.parse(yyyyMmDd).format(DateTimeFormatter.BASIC_ISO_DATE) + "  ";
        } catch (DateTimeParseException ex) {
            return yyyyMmDd.substring(0, 4) + yyyyMmDd.substring(5, 7) + yyyyMmDd.substring(8, 10) + "  ";
        }
    }

    /**
     * {@code INITIALIZE} as the reference shows it: text blank, packed zero, and zoned decimals as plain
     * zero digits (no overpunched sign on the last byte, unlike a MOVE of zero).
     */
    static CobolRecord initialize(RecordLayout layout, CodePage codePage) {
        byte[] bytes = CobolRecord.initialized(layout, codePage).toBytes();
        for (Field field : layout.fields()) {
            if (field instanceof NumericField numeric && numeric.usage() == Usage.DISPLAY) {
                System.arraycopy(codePage.encodeText("0".repeat(numeric.length())), 0, bytes, numeric.offset(), numeric.length());
            }
        }
        return CobolRecord.of(layout, codePage, bytes);
    }

    /** 1100-DISPLAY-ACCT-RECORD: numbers as GnuCOBOL DISPLAYs them (digits, then the sign of a signed item). */
    private static void display(List<String> sysout, CobolRecord a) {
        Field[] fields = {AccountRecordLayout.ACCT_ID, AccountRecordLayout.ACCT_ACTIVE_STATUS,
                AccountRecordLayout.ACCT_CURR_BAL, AccountRecordLayout.ACCT_CREDIT_LIMIT,
                AccountRecordLayout.ACCT_CASH_CREDIT_LIMIT, AccountRecordLayout.ACCT_OPEN_DATE,
                AccountRecordLayout.ACCT_EXPIRAION_DATE, AccountRecordLayout.ACCT_REISSUE_DATE,
                AccountRecordLayout.ACCT_CURR_CYC_CREDIT, AccountRecordLayout.ACCT_CURR_CYC_DEBIT,
                AccountRecordLayout.ACCT_GROUP_ID};
        for (int i = 0; i < fields.length; i++) {
            sysout.add(String.format("%-24s:", LABELS[i]) + displayValue(a, fields[i]));
        }
        sysout.add("-".repeat(49));
    }

    static String displayValue(CobolRecord record, Field field) {
        if (field instanceof NumericField numeric) {
            BigDecimal value = record.get(numeric);
            String digits = value.unscaledValue().abs().toString();
            String padded = "0".repeat(Math.max(0, numeric.digits() - digits.length())) + digits;
            return numeric.signed() ? padded + (value.signum() < 0 ? "-" : "+") : padded;
        }
        return text(record, field);
    }

    /** The field's bytes as text (a group move or DISPLAY of the item as stored). */
    static String text(CobolRecord record, Field field) {
        return record.codePage().decodeText(record.toBytes(), field.offset(), field.length());
    }
}
