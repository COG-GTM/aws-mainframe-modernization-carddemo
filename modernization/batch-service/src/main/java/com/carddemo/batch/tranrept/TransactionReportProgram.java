package com.carddemo.batch.tranrept;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

/**
 * CBTRN03C: the TRANREPT report ({@code CVTRA07Y}, 133-byte records) from TRANFILE, the
 * transactions STEP05R sorted by card number and filtered on the DATEPARM dates.
 *
 * <p>Ported paragraph by paragraph, including the behavior the source has (each confirmed by running
 * the COBOL under GnuCOBOL, {@code scripts/migration/tranrept/}):
 * <ul>
 *   <li>A record whose {@code TRAN-PROC-TS(1:10)} is outside the dates runs {@code NEXT SENTENCE},
 *       which leaves the {@code PERFORM UNTIL} (the sentence ends after {@code END-PERFORM}): the
 *       report stops there with no totals. The SORT step drops such records, so this only shows for
 *       a TRANFILE that was not built by STEP05R.</li>
 *   <li>"Account Total" is a per-card subtotal, written when TRAN-CARD-NUM changes, and the last
 *       card's is never written.</li>
 *   <li>At end of file the last record's TRAN-AMT is added once more to the page total (and so to
 *       the grand total) before the page and grand totals are written.</li>
 *   <li>The page break tests {@code MOD(WS-LINE-COUNTER, 20) = 0} only before a detail line, and the
 *       counter counts every line (headers and totals too), so a break can be skipped when totals
 *       step over a multiple of 20.</li>
 *   <li>Totals are {@code S9(09)V99}: an ADD that overflows keeps the low-order nine integer digits.</li>
 *   <li>No DATEPARM record ends the run before the first read: an empty report.</li>
 * </ul>
 */
public final class TransactionReportProgram {

    public static final int LRECL = 133;
    public static final int PAGE_SIZE = 20;

    private static final RecordLayout.Builder B = RecordLayout.builder("CBTRN03C", "FD-REPTFILE-REC");
    public static final AlphanumericField REPORT_RECORD = B.alpha("FD-REPTFILE-REC", "X(133)");
    public static final RecordLayout REPORT_LAYOUT = B.build(LRECL);

    private static final int PROC_DATE_OFFSET = TranRecordLayout.TRAN_PROC_TS.offset();
    private static final BigDecimal TOTAL_MODULUS = new BigDecimal("1000000000");
    static final String SEPARATOR = "-".repeat(LRECL);

    /** {@code read}: TRANFILE records read; {@code reported}: detail lines; {@code lines}: report records. */
    public record Result(int read, int reported, List<String> lines) {
    }

    private final ReferenceData reference;
    private final List<String> lines = new ArrayList<>();

    private String reptStartDate = " ".repeat(10);
    private String reptEndDate = " ".repeat(10);
    private boolean firstTime = true;
    private long lineCounter;
    private BigDecimal pageTotal = BigDecimal.ZERO;
    private BigDecimal accountTotal = BigDecimal.ZERO;
    private BigDecimal grandTotal = BigDecimal.ZERO;
    private String currCardNum = " ".repeat(16);
    private long xrefAcctId;
    private String typeDesc = "";
    private String catDesc = "";

    private TransactionReportProgram(ReferenceData reference) {
        this.reference = reference;
    }

    /**
     * Runs the program. {@code dateParm} is empty when DATEPARM has no record; {@code tranfile} are
     * TRAN-RECORDs in {@code codePage}. Throws {@link TransactionReportAbend} where the COBOL abends.
     */
    public static Result run(Optional<DateParm> dateParm, List<CobolRecord> tranfile, ReferenceData reference,
                             CodePage codePage) {
        return new TransactionReportProgram(reference).execute(dateParm, tranfile, codePage);
    }

    /** The report lines as TRANREPT records in {@code codePage}. */
    public static List<CobolRecord> records(List<String> lines, CodePage codePage) {
        return lines.stream().map(line -> CobolRecord.initialized(REPORT_LAYOUT, codePage).set(REPORT_RECORD, line))
                .toList();
    }

    private Result execute(Optional<DateParm> dateParm, List<CobolRecord> tranfile, CodePage codePage) {
        boolean endOfFile = dateParm.isEmpty();
        Iterator<CobolRecord> records = tranfile.iterator();
        // READ ... INTO TRAN-RECORD leaves the last record in place at end of file.
        CobolRecord tran = CobolRecord.initialized(TranRecordLayout.LAYOUT, codePage);
        int read = 0;
        int reported = 0;
        while (!endOfFile) {
            if (records.hasNext()) {
                tran = records.next();
                read++;
            } else {
                endOfFile = true;
            }
            if (!dateParm.get().includes(tran.toBytes(), PROC_DATE_OFFSET)) {
                break; // NEXT SENTENCE
            }
            if (!endOfFile) {
                String cardNum = tran.get(TranRecordLayout.TRAN_CARD_NUM);
                if (!currCardNum.equals(cardNum)) {
                    if (!firstTime) {
                        writeAccountTotals();
                    }
                    currCardNum = cardNum;
                    lookupXref(cardNum);
                }
                String typeCd = tran.get(TranRecordLayout.TRAN_TYPE_CD);
                int catCd = tran.getInt(TranRecordLayout.TRAN_CAT_CD);
                lookupTrantype(typeCd);
                lookupTrancatg(typeCd, catCd);
                writeTransactionReport(dateParm.get(), tran);
                reported++;
            } else {
                BigDecimal amount = tran.get(TranRecordLayout.TRAN_AMT);
                pageTotal = add(pageTotal, amount);
                accountTotal = add(accountTotal, amount);
                writePageTotals();
                writeGrandTotals();
            }
        }
        return new Result(read, reported, List.copyOf(lines));
    }

    private void lookupXref(String cardNum) {
        xrefAcctId = reference.accountIdForCard(cardNum)
                .orElseThrow(() -> new TransactionReportAbend("INVALID CARD NUMBER : " + cardNum));
    }

    private void lookupTrantype(String typeCd) {
        typeDesc = reference.typeDescription(typeCd)
                .orElseThrow(() -> new TransactionReportAbend("INVALID TRANSACTION TYPE : " + typeCd));
    }

    private void lookupTrancatg(String typeCd, int catCd) {
        catDesc = reference.categoryDescription(typeCd, catCd)
                .orElseThrow(() -> new TransactionReportAbend(
                        "INVALID TRAN CATG KEY : " + typeCd + String.format("%04d", catCd)));
    }

    /** {@code 1100-WRITE-TRANSACTION-REPORT}. */
    private void writeTransactionReport(DateParm dateParm, CobolRecord tran) {
        if (firstTime) {
            firstTime = false;
            reptStartDate = dateParm.startText();
            reptEndDate = dateParm.endText();
            writeHeaders();
        }
        if (lineCounter % PAGE_SIZE == 0) {
            writePageTotals();
            writeHeaders();
        }
        BigDecimal amount = tran.get(TranRecordLayout.TRAN_AMT);
        pageTotal = add(pageTotal, amount);
        accountTotal = add(accountTotal, amount);
        writeDetail(tran, amount);
    }

    /** {@code 1110-WRITE-PAGE-TOTALS}. */
    private void writePageTotals() {
        write(pad("Page Total", 11) + ".".repeat(86) + EditedAmount.total(pageTotal));
        grandTotal = add(grandTotal, pageTotal);
        pageTotal = BigDecimal.ZERO;
        lineCounter++;
        write(SEPARATOR);
        lineCounter++;
    }

    /** {@code 1120-WRITE-ACCOUNT-TOTALS}. */
    private void writeAccountTotals() {
        write(pad("Account Total", 13) + ".".repeat(84) + EditedAmount.total(accountTotal));
        accountTotal = BigDecimal.ZERO;
        lineCounter++;
        write(SEPARATOR);
        lineCounter++;
    }

    /** {@code 1110-WRITE-GRAND-TOTALS}: does not count a line. */
    private void writeGrandTotals() {
        write(pad("Grand Total", 11) + ".".repeat(86) + EditedAmount.total(grandTotal));
    }

    /** {@code 1120-WRITE-HEADERS}: REPORT-NAME-HEADER, a blank line, TRANSACTION-HEADER-1 and -2. */
    private void writeHeaders() {
        write(pad("DALYREPT", 38) + pad("Daily Transaction Report", 41) + pad("Date Range: ", 12)
                + reptStartDate + " to " + reptEndDate);
        lineCounter++;
        write("");
        lineCounter++;
        write(pad("Transaction ID", 17) + pad("Account ID", 12) + pad("Transaction Type", 19)
                + pad("Tran Category", 35) + pad("Tran Source", 14) + " " + pad("        Amount", 16));
        lineCounter++;
        write(SEPARATOR);
        lineCounter++;
    }

    /** {@code 1120-WRITE-DETAIL}: TRANSACTION-DETAIL-REPORT. */
    private void writeDetail(CobolRecord tran, BigDecimal amount) {
        write(pad(tran.get(TranRecordLayout.TRAN_ID), 16) + " "
                + String.format("%011d", xrefAcctId) + " "
                + pad(tran.get(TranRecordLayout.TRAN_TYPE_CD), 2) + "-"
                + pad(typeDesc, 15) + " "
                + String.format("%04d", tran.getInt(TranRecordLayout.TRAN_CAT_CD)) + "-"
                + pad(catDesc, 29) + " "
                + pad(tran.get(TranRecordLayout.TRAN_SOURCE), 10) + "    "
                + EditedAmount.detail(amount) + "  ");
        lineCounter++;
    }

    /** {@code 1111-WRITE-REPORT-REC}: one 133-byte record. */
    private void write(String line) {
        lines.add(pad(line, LRECL));
    }

    /** ADD to an {@code S9(09)V99} field without ON SIZE ERROR: high-order digits are lost. */
    static BigDecimal add(BigDecimal total, BigDecimal amount) {
        return total.add(amount).remainder(TOTAL_MODULUS);
    }

    /** MOVE to {@code PIC X(n)}: left-justified, space-filled, cut on the right. */
    static String pad(String value, int length) {
        String text = value == null ? "" : value;
        return text.length() >= length ? text.substring(0, length) : text + " ".repeat(length - text.length());
    }
}
