package com.carddemo.batch.tranrept;

import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.RecordSink;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.codec.CobolDisplay;
import com.carddemo.common.codec.CobolNumeric;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.NumericEdited;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.transaction.TransactionCategoryId;
import com.carddemo.transaction.TransactionCategoryRecord;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionTypeRecord;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Locale;
import java.util.Optional;

/**
 * {@code CBTRN03C} (TRANREPT STEP15): prints the daily transaction report (copybook CVTRA07Y, 133-byte records) from
 * the TRANFILE extract sorted by card number. DATEPARM gives WS-START-DATE / WS-END-DATE; a record is reported when
 * {@code TRAN-PROC-TS(1:10)} (not ORIG-TS) lies in the window, compared byte-wise in the record's code page. A
 * record outside the window runs {@code NEXT SENTENCE}, whose sentence ends with the period after
 * {@code END-PERFORM}: the loop stops there, without page or grand total (the TRANREPT SORT step has already dropped
 * such records, so this only matters for a TRANFILE that was not extracted). A new TRAN-CARD-NUM writes the
 * "Account Total" of the previous card (it is a per-card subtotal) and reads CARDXREF; each record reads TRANTYPE and
 * TRANCATG. Page breaks test {@code MOD(WS-LINE-COUNTER, 20) = 0} before each detail line, where the counter counts
 * every line written, headers and totals included. At end of file the last record's TRAN-AMT is added once more to
 * the page and account totals, the page total and the grand total are written and the last card's account total is
 * never written (rules doc CBTRN03C.md).
 */
public final class Cbtrn03c {

    public static final String PROGRAM = "CBTRN03C";
    public static final String TRANFILE = "TRANFILE";
    public static final String CARDXREF = "CARDXREF";
    public static final String TRANTYPE = "TRANTYPE";
    public static final String TRANCATG = "TRANCATG";
    public static final String DATEPARM = "DATEPARM";
    public static final String TRANREPT = "TRANREPT";
    public static final int REPORT_LRECL = 133;
    public static final int PAGE_SIZE = 20;

    static final String DETAIL_AMOUNT = "-ZZZ,ZZZ,ZZZ.ZZ";
    static final String TOTAL_AMOUNT = "+ZZZ,ZZZ,ZZZ.ZZ";
    static final String SEPARATOR = "-".repeat(REPORT_LRECL);

    /** {@code read}: TRANFILE records; {@code reported}: detail lines; {@code lines}: report records written. */
    public record Result(long read, long reported, long lines, ReturnCode returnCode) {
    }

    private final KsdsInput tranfile;
    private final KeyedDataset<String, CardXrefRecord> cardxref;
    private final KeyedDataset<String, TransactionTypeRecord> trantype;
    private final KeyedDataset<TransactionCategoryId, TransactionCategoryRecord> trancatg;
    private final KsdsInput dateparm;
    private final RecordSink report;
    private final RecordEncoding encoding;
    private final Sysout sysout;

    private String startDate = " ".repeat(10);
    private String endDate = " ".repeat(10);
    private boolean firstTime = true;
    private long lineCounter;
    private BigDecimal pageTotal = BigDecimal.ZERO;
    private BigDecimal accountTotal = BigDecimal.ZERO;
    private BigDecimal grandTotal = BigDecimal.ZERO;
    private String currCardNum = " ".repeat(16);
    private long xrefAcctId;
    private String typeDesc = "";
    private String catDesc = "";
    private String reptStartDate = "";
    private String reptEndDate = "";
    private long lines;

    public Cbtrn03c(KsdsInput tranfile, KeyedDataset<String, CardXrefRecord> cardxref,
                    KeyedDataset<String, TransactionTypeRecord> trantype,
                    KeyedDataset<TransactionCategoryId, TransactionCategoryRecord> trancatg, KsdsInput dateparm,
                    RecordSink report, RecordEncoding encoding, Sysout sysout) {
        this.tranfile = tranfile;
        this.cardxref = cardxref;
        this.trantype = trantype;
        this.trancatg = trancatg;
        this.dateparm = dateparm;
        this.report = report;
        this.encoding = encoding;
        this.sysout = sysout;
    }

    /**
     * The main line: TRANFILE to end of file ({@code 1000-TRANFILE-GET-NEXT}) within the DATEPARM window, with the
     * card break, page and grand totals.
     */
    public Result run() {
        sysout.display("START OF EXECUTION OF PROGRAM " + PROGRAM);
        io(tranfile::open, "ERROR OPENING TRANFILE");
        io(report::open, "ERROR OPENING REPTFILE");
        io(cardxref::open, "ERROR OPENING CROSS REF FILE");
        io(trantype::open, "ERROR OPENING TRANSACTION TYPE FILE");
        io(trancatg::open, "ERROR OPENING TRANSACTION CATG FILE");
        io(dateparm::open, "ERROR OPENING DATE PARM FILE");
        boolean endOfFile = !readDateparm();
        long read = 0;
        long reported = 0;
        FixedWidthRecord tran = FixedWidthRecord.spaces(TransactionRecord.MAPPER.layout(), encoding);
        boolean anyRecord = false;
        while (!endOfFile) {
            Optional<FixedWidthRecord> next;
            try {
                next = tranfile.readNext();
            } catch (FileStatusException e) {
                throw sysout.ioAbend("ERROR READING TRANSACTION FILE", e);
            }
            if (next.isPresent()) {
                tran = next.get();
                anyRecord = true;
                read++;
            } else {
                endOfFile = true;
            }
            String procDate = tran.getString("TRAN-PROC-TS").substring(0, 10);
            if (!(compare(procDate, startDate) >= 0 && compare(procDate, endDate) <= 0)) {
                break;
            }
            if (!endOfFile) {
                sysout.display(tran.text());
                TransactionRecord t = TransactionRecord.MAPPER.fromRecord(tran);
                String cardNum = tran.getString("TRAN-CARD-NUM");
                if (!currCardNum.equals(cardNum)) {
                    if (!firstTime) {
                        writeAccountTotals();
                    }
                    currCardNum = cardNum;
                    lookupXref(cardNum);
                }
                lookupTrantype(t.tranTypeCd());
                lookupTrancatg(new TransactionCategoryId(t.tranTypeCd(), t.tranCatCd()));
                writeTransactionReport(tran, t.amount());
                reported++;
            } else {
                BigDecimal amount = anyRecord ? TransactionRecord.MAPPER.fromRecord(tran).amount() : BigDecimal.ZERO;
                sysout.display("TRAN-AMT " + CobolDisplay.numeric(amount, 11, 2, true));
                sysout.display("WS-PAGE-TOTAL" + CobolDisplay.numeric(pageTotal, 11, 2, true));
                pageTotal = add(pageTotal, amount);
                accountTotal = add(accountTotal, amount);
                writePageTotals();
                writeGrandTotals();
            }
        }
        io(tranfile::close, "ERROR CLOSING POSTED TRANSACTION FILE");
        io(report::close, "ERROR CLOSING REPORT FILE");
        io(cardxref::close, "ERROR CLOSING CROSS REF FILE");
        io(trantype::close, "ERROR CLOSING TRANSACTION TYPE FILE");
        io(trancatg::close, "ERROR CLOSING TRANSACTION CATG FILE");
        io(dateparm::close, "ERROR CLOSING DATE PARM FILE");
        sysout.display("END OF EXECUTION OF PROGRAM " + PROGRAM);
        return new Result(read, reported, lines, ReturnCode.OK);
    }

    /** {@code 0550-DATEPARM-READ}; false at end of file (END-OF-FILE = 'Y': no report at all). */
    private boolean readDateparm() {
        Optional<FixedWidthRecord> record;
        try {
            record = dateparm.readNext();
        } catch (FileStatusException e) {
            throw sysout.ioAbend("ERROR READING DATEPARM FILE", e);
        }
        if (record.isEmpty()) {
            return false;
        }
        String text = pad(record.get().text(), 21);
        startDate = text.substring(0, 10);
        endDate = text.substring(11, 21);
        sysout.display("Reporting from " + startDate + " to " + endDate);
        return true;
    }

    /** {@code 1500-A-LOOKUP-XREF}. */
    private void lookupXref(String cardNum) {
        CardXrefRecord xref = cardxref.read(cardNum).orElseThrow(() -> sysout.ioAbend(
                "INVALID CARD NUMBER : " + cardNum, notFound(CARDXREF)));
        xrefAcctId = xref.acctId();
    }

    /** {@code 1500-B-LOOKUP-TRANTYPE}. */
    private void lookupTrantype(String type) {
        TransactionTypeRecord record = trantype.read(type).orElseThrow(() -> sysout.ioAbend(
                "INVALID TRANSACTION TYPE : " + pad(type, 2), notFound(TRANTYPE)));
        typeDesc = record.description();
    }

    /** {@code 1500-C-LOOKUP-TRANCATG}. */
    private void lookupTrancatg(TransactionCategoryId key) {
        TransactionCategoryRecord record = trancatg.read(key).orElseThrow(() -> sysout.ioAbend(
                "INVALID TRAN CATG KEY : " + categoryKey(key), notFound(TRANCATG)));
        catDesc = record.description();
    }

    static String categoryKey(TransactionCategoryId key) {
        return String.format(Locale.ROOT, "%-2s%04d", key.tranTypeCd(), key.tranCatCd());
    }

    /** {@code 1100-WRITE-TRANSACTION-REPORT}. */
    private void writeTransactionReport(FixedWidthRecord tran, BigDecimal amount) {
        if (firstTime) {
            firstTime = false;
            reptStartDate = startDate;
            reptEndDate = endDate;
            writeHeaders();
        }
        if (lineCounter % PAGE_SIZE == 0) {
            writePageTotals();
            writeHeaders();
        }
        pageTotal = add(pageTotal, amount);
        accountTotal = add(accountTotal, amount);
        writeDetail(tran, amount);
    }

    /** {@code 1110-WRITE-PAGE-TOTALS}: adds the page total to the grand total. */
    private void writePageTotals() {
        write(pad("Page Total", 11) + ".".repeat(86) + NumericEdited.format(pageTotal, TOTAL_AMOUNT));
        grandTotal = add(grandTotal, pageTotal);
        pageTotal = BigDecimal.ZERO;
        lineCounter++;
        write(SEPARATOR);
        lineCounter++;
    }

    /** {@code 1120-WRITE-ACCOUNT-TOTALS}. */
    private void writeAccountTotals() {
        write("Account Total" + ".".repeat(84) + NumericEdited.format(accountTotal, TOTAL_AMOUNT));
        accountTotal = BigDecimal.ZERO;
        lineCounter++;
        write(SEPARATOR);
        lineCounter++;
    }

    /** {@code 1110-WRITE-GRAND-TOTALS}. */
    private void writeGrandTotals() {
        write("Grand Total" + ".".repeat(86) + NumericEdited.format(grandTotal, TOTAL_AMOUNT));
    }

    /** {@code 1120-WRITE-HEADERS}. */
    private void writeHeaders() {
        write(reportNameHeader(reptStartDate, reptEndDate));
        lineCounter++;
        write("");
        lineCounter++;
        write(columnHeader());
        lineCounter++;
        write(SEPARATOR);
        lineCounter++;
    }

    static String reportNameHeader(String start, String end) {
        return pad("DALYREPT", 38) + pad("Daily Transaction Report", 41) + "Date Range: " + pad(start, 10) + " to "
                + pad(end, 10);
    }

    static String columnHeader() {
        return pad("Transaction ID", 17) + pad("Account ID", 12) + pad("Transaction Type", 19)
                + pad("Tran Category", 35) + pad("Tran Source", 14) + " " + pad("        Amount", 16);
    }

    /** {@code 1120-WRITE-DETAIL}: TRANSACTION-DETAIL-REPORT. */
    private void writeDetail(FixedWidthRecord tran, BigDecimal amount) {
        write(detail(tran.getString("TRAN-ID"), xrefAcctId, tran.getString("TRAN-TYPE-CD"), typeDesc,
                tran.getString("TRAN-CAT-CD"), catDesc, tran.getString("TRAN-SOURCE"), amount));
        lineCounter++;
    }

    static String detail(String tranId, long acctId, String typeCd, String typeDesc, String catCd, String catDesc,
                         String source, BigDecimal amount) {
        return pad(tranId, 16) + " " + String.format(Locale.ROOT, "%011d", acctId) + " " + pad(typeCd, 2) + "-"
                + pad(typeDesc, 15) + " " + pad(catCd, 4) + "-" + pad(catDesc, 29) + " " + pad(source, 10)
                + "    " + NumericEdited.format(amount, DETAIL_AMOUNT) + "  ";
    }

    /** {@code 1111-WRITE-REPORT-REC}: {@code WRITE FD-REPTFILE-REC}, abend on a bad status. */
    private void write(String line) {
        try {
            report.write(new FixedWidthRecord(encoding.encode(pad(line, REPORT_LRECL)), encoding));
        } catch (FileStatusException e) {
            throw sysout.ioAbend("ERROR WRITING REPTFILE", e);
        }
        lines++;
    }

    /** Alphanumeric comparison in the program's collating sequence (the record code page). */
    private int compare(String a, String b) {
        return Arrays.compareUnsigned(encoding.encode(a), encoding.encode(b));
    }

    private static BigDecimal add(BigDecimal total, BigDecimal amount) {
        return CobolNumeric.truncate(total.add(amount), 11, 2, true);
    }

    private static FileStatusException notFound(String ddname) {
        return new FileStatusException(ddname, "READ", FileStatus.RECORD_NOT_FOUND);
    }

    static String pad(String value, int length) {
        String v = value == null ? "" : value;
        return v.length() >= length ? v.substring(0, length) : v + " ".repeat(length - v.length());
    }

    private void io(Runnable action, String message) {
        try {
            action.run();
        } catch (FileStatusException e) {
            throw sysout.ioAbend(message, e);
        }
    }
}
