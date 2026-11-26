package com.carddemo.programs.cbtrn03c;

import com.carddemo.programs.cbtrn03c.copybook.CardXrefRecord;
import com.carddemo.programs.cbtrn03c.copybook.ReportLayouts;
import com.carddemo.programs.cbtrn03c.copybook.ReportLayouts.ReportNameHeader;
import com.carddemo.programs.cbtrn03c.copybook.ReportLayouts.TransactionDetailReport;
import com.carddemo.programs.cbtrn03c.copybook.TranCatRecord;
import com.carddemo.programs.cbtrn03c.copybook.TranRecord;
import com.carddemo.programs.cbtrn03c.copybook.TranTypeRecord;
import com.carddemo.programs.cbtrn03c.io.FileStatus;
import com.carddemo.programs.cbtrn03c.io.KeyedInputFile;
import com.carddemo.programs.cbtrn03c.io.ReadResult;
import com.carddemo.programs.cbtrn03c.io.SequentialInputFile;
import com.carddemo.programs.cbtrn03c.io.SequentialOutputFile;
import com.carddemo.programs.cbtrn03c.support.CobolNumeric;
import com.carddemo.programs.cbtrn03c.support.Fixed;
import com.carddemo.programs.cbtrn03c.support.ZonedDecimal;

import java.io.PrintStream;
import java.math.BigDecimal;
import java.util.Objects;

/**
 * CBTRN03C - print the transaction detail report (job TRANREPT, step STEP10R).
 *
 * <p>DD names: TRANFILE (sequential, LRECL 350), CARDXREF / TRANTYPE / TRANCATG (KSDS, random),
 * DATEPARM (sequential, LRECL 80), TRANREPT (sequential output, LRECL 133). DISPLAY output goes to
 * {@code sysout}. Unrecoverable errors raise {@link AbendException} with code 999.
 */
public final class Cbtrn03c {

    public static final int TRANFILE_LRECL = 350;
    public static final int DATEPARM_LRECL = 80;
    public static final int TRANREPT_LRECL = 133;
    public static final int ABEND_CODE = 999;

    static final int APPL_RESULT_AOK = 0;
    static final int APPL_RESULT_EOF = 16;
    static final int WS_PAGE_SIZE = 20;
    private static final long LINE_COUNTER_MODULUS = 1_000_000_000L;
    private static final int TOTAL_INTEGER_DIGITS = 9;
    private static final int TOTAL_SCALE = 2;

    private final SequentialInputFile transactFile;
    private final KeyedInputFile xrefFile;
    private final KeyedInputFile trantypeFile;
    private final KeyedInputFile trancatgFile;
    private final SequentialOutputFile reportFile;
    private final SequentialInputFile dateParmsFile;
    private final PrintStream sysout;

    // FILE STATUS fields
    private String tranfileStatus = "  ";
    private String cardxrefStatus = "  ";
    private String trantypeStatus = "  ";
    private String trancatgStatus = "  ";
    private String tranreptStatus = "  ";
    private String dateparmStatus = "  ";

    // Record areas (COPY CVTRA05Y / CVACT03Y / CVTRA03Y / CVTRA04Y / CVTRA07Y)
    private String tranRecordImage = TranRecord.initial().toFixedWidth();
    private TranRecord tranRecord = TranRecord.initial();
    private CardXrefRecord cardXrefRecord = CardXrefRecord.initial();
    private TranTypeRecord tranTypeRecord = TranTypeRecord.initial();
    private TranCatRecord tranCatRecord = TranCatRecord.initial();
    private ReportNameHeader reportNameHeader = ReportNameHeader.initial();

    // FD key fields
    private String fdXrefCardNum = Fixed.spaces(16);
    private String fdTranType = Fixed.spaces(2);
    private String fdTranCatKey = Fixed.spaces(6);

    // 01 WS-DATEPARM-RECORD
    private String wsStartDate = Fixed.spaces(10);
    private String wsEndDate = Fixed.spaces(10);

    // 01 WS-REPORT-VARS
    private boolean wsFirstTime = true;
    private long wsLineCounter;
    private BigDecimal wsPageTotal = zeroTotal();
    private BigDecimal wsAccountTotal = zeroTotal();
    private BigDecimal wsGrandTotal = zeroTotal();
    private String wsCurrCardNum = Fixed.spaces(16);

    private String ioStatus = "  ";
    private int applResult;
    private boolean endOfFile;

    public Cbtrn03c(
            SequentialInputFile transactFile,
            KeyedInputFile xrefFile,
            KeyedInputFile trantypeFile,
            KeyedInputFile trancatgFile,
            SequentialOutputFile reportFile,
            SequentialInputFile dateParmsFile,
            PrintStream sysout) {
        this.transactFile = Objects.requireNonNull(transactFile, "TRANFILE");
        this.xrefFile = Objects.requireNonNull(xrefFile, "CARDXREF");
        this.trantypeFile = Objects.requireNonNull(trantypeFile, "TRANTYPE");
        this.trancatgFile = Objects.requireNonNull(trancatgFile, "TRANCATG");
        this.reportFile = Objects.requireNonNull(reportFile, "TRANREPT");
        this.dateParmsFile = Objects.requireNonNull(dateParmsFile, "DATEPARM");
        this.sysout = Objects.requireNonNull(sysout, "sysout");
    }

    /** PROCEDURE DIVISION mainline. Returns the GOBACK return code (0). */
    public int run() {
        sysout.println("START OF EXECUTION OF PROGRAM CBTRN03C");
        tranfileOpen();
        reptfileOpen();
        cardxrefOpen();
        trantypeOpen();
        trancatgOpen();
        dateparmOpen();

        dateparmRead();

        mainLoop:
        while (!endOfFile) {
            if (!endOfFile) {
                tranfileGetNext();
                String procDate = tranRecord.tranProcTs().substring(0, 10);
                if (!(procDate.compareTo(wsStartDate) >= 0 && procDate.compareTo(wsEndDate) <= 0)) {
                    // NEXT SENTENCE: control passes to the statement after the period that ends
                    // the PERFORM ... END-PERFORM sentence, i.e. out of the loop.
                    break mainLoop;
                }
                if (!endOfFile) {
                    sysout.println(tranRecordImage);
                    if (!wsCurrCardNum.equals(tranRecord.tranCardNum())) {
                        if (!wsFirstTime) {
                            writeAccountTotals();
                        }
                        wsCurrCardNum = tranRecord.tranCardNum();
                        fdXrefCardNum = tranRecord.tranCardNum();
                        lookupXref();
                    }
                    fdTranType = tranRecord.tranTypeCd();
                    lookupTrantype();
                    fdTranCatKey = new TranCatRecord.TranCatKey(tranRecord.tranTypeCd(), tranRecord.tranCatCd())
                            .toFixedWidth();
                    lookupTrancatg();
                    writeTransactionReport();
                } else {
                    sysout.println("TRAN-AMT " + tranAmtDisplay());
                    sysout.println("WS-PAGE-TOTAL" + ZonedDecimal.encode(wsPageTotal, TOTAL_INTEGER_DIGITS, TOTAL_SCALE));
                    wsPageTotal = addTotal(wsPageTotal, tranRecord.tranAmt());
                    wsAccountTotal = addTotal(wsAccountTotal, tranRecord.tranAmt());
                    writePageTotals();
                    writeGrandTotals();
                }
            }
        }

        tranfileClose();
        reptfileClose();
        cardxrefClose();
        trantypeClose();
        trancatgClose();
        dateparmClose();

        sysout.println("END OF EXECUTION OF PROGRAM CBTRN03C");
        return 0;
    }

    // 0550-DATEPARM-READ
    void dateparmRead() {
        ReadResult result = dateParmsFile.read();
        dateparmStatus = result.status();
        if (FileStatus.OK.equals(dateparmStatus)) {
            String rec = Fixed.pic(result.record(), DATEPARM_LRECL);
            wsStartDate = rec.substring(0, 10);
            wsEndDate = rec.substring(11, 21);
        }
        applResult = switch (dateparmStatus) {
            case "00" -> 0;
            case "10" -> 16;
            default -> 12;
        };
        if (applAok()) {
            sysout.println("Reporting from " + wsStartDate + " to " + wsEndDate);
        } else if (applEof()) {
            endOfFile = true;
        } else {
            sysout.println("ERROR READING DATEPARM FILE");
            ioStatus = dateparmStatus;
            displayIoStatus();
            abendProgram("ERROR READING DATEPARM FILE");
        }
    }

    // 1000-TRANFILE-GET-NEXT
    void tranfileGetNext() {
        ReadResult result = transactFile.read();
        tranfileStatus = result.status();
        if (FileStatus.OK.equals(tranfileStatus)) {
            tranRecordImage = Fixed.pic(result.record(), TranRecord.LENGTH);
            tranRecord = TranRecord.parse(tranRecordImage);
        }
        applResult = switch (tranfileStatus) {
            case "00" -> 0;
            case "10" -> 16;
            default -> 12;
        };
        if (!applAok()) {
            if (applEof()) {
                endOfFile = true;
            } else {
                sysout.println("ERROR READING TRANSACTION FILE");
                ioStatus = tranfileStatus;
                displayIoStatus();
                abendProgram("ERROR READING TRANSACTION FILE");
            }
        }
    }

    // 1100-WRITE-TRANSACTION-REPORT
    void writeTransactionReport() {
        if (wsFirstTime) {
            wsFirstTime = false;
            reportNameHeader = new ReportNameHeader(wsStartDate, wsEndDate);
            writeHeaders();
        }
        if (wsLineCounter % WS_PAGE_SIZE == 0) {
            writePageTotals();
            writeHeaders();
        }
        wsPageTotal = addTotal(wsPageTotal, tranRecord.tranAmt());
        wsAccountTotal = addTotal(wsAccountTotal, tranRecord.tranAmt());
        writeDetail();
    }

    // 1110-WRITE-PAGE-TOTALS
    void writePageTotals() {
        writeReportRec(ReportLayouts.reportPageTotals(wsPageTotal));
        wsGrandTotal = addTotal(wsGrandTotal, wsPageTotal);
        wsPageTotal = zeroTotal();
        incrementLineCounter();
        writeReportRec(ReportLayouts.TRANSACTION_HEADER_2);
        incrementLineCounter();
    }

    // 1120-WRITE-ACCOUNT-TOTALS
    void writeAccountTotals() {
        writeReportRec(ReportLayouts.reportAccountTotals(wsAccountTotal));
        wsAccountTotal = zeroTotal();
        incrementLineCounter();
        writeReportRec(ReportLayouts.TRANSACTION_HEADER_2);
        incrementLineCounter();
    }

    // 1110-WRITE-GRAND-TOTALS
    void writeGrandTotals() {
        writeReportRec(ReportLayouts.reportGrandTotals(wsGrandTotal));
    }

    // 1120-WRITE-HEADERS
    void writeHeaders() {
        writeReportRec(reportNameHeader.format());
        incrementLineCounter();
        writeReportRec(Fixed.spaces(TRANREPT_LRECL));
        incrementLineCounter();
        writeReportRec(ReportLayouts.TRANSACTION_HEADER_1);
        incrementLineCounter();
        writeReportRec(ReportLayouts.TRANSACTION_HEADER_2);
        incrementLineCounter();
    }

    // 1111-WRITE-REPORT-REC
    void writeReportRec(String line) {
        tranreptStatus = reportFile.write(Fixed.pic(line, TRANREPT_LRECL));
        applResult = FileStatus.OK.equals(tranreptStatus) ? 0 : 12;
        if (!applAok()) {
            sysout.println("ERROR WRITING REPTFILE");
            ioStatus = tranreptStatus;
            displayIoStatus();
            abendProgram("ERROR WRITING REPTFILE");
        }
    }

    // 1120-WRITE-DETAIL
    void writeDetail() {
        // INITIALIZE TRANSACTION-DETAIL-REPORT, then every named field is moved.
        TransactionDetailReport detail = new TransactionDetailReport(
                tranRecord.tranId(),
                cardXrefRecord.xrefAcctIdDisplay(),
                tranRecord.tranTypeCd(),
                tranTypeRecord.tranTypeDesc(),
                tranRecord.tranCatCd(),
                tranCatRecord.tranCatTypeDesc(),
                tranRecord.tranSource(),
                tranRecord.tranAmt());
        writeReportRec(detail.format());
        incrementLineCounter();
    }

    // 0000-TRANFILE-OPEN
    void tranfileOpen() {
        tranfileStatus = transactFile.open();
        checkOpenOrClose(tranfileStatus, "ERROR OPENING TRANFILE");
    }

    // 0100-REPTFILE-OPEN
    void reptfileOpen() {
        tranreptStatus = reportFile.open();
        checkOpenOrClose(tranreptStatus, "ERROR OPENING REPTFILE");
    }

    // 0200-CARDXREF-OPEN
    void cardxrefOpen() {
        cardxrefStatus = xrefFile.open();
        checkOpenOrClose(cardxrefStatus, "ERROR OPENING CROSS REF FILE");
    }

    // 0300-TRANTYPE-OPEN
    void trantypeOpen() {
        trantypeStatus = trantypeFile.open();
        checkOpenOrClose(trantypeStatus, "ERROR OPENING TRANSACTION TYPE FILE");
    }

    // 0400-TRANCATG-OPEN
    void trancatgOpen() {
        trancatgStatus = trancatgFile.open();
        checkOpenOrClose(trancatgStatus, "ERROR OPENING TRANSACTION CATG FILE");
    }

    // 0500-DATEPARM-OPEN
    void dateparmOpen() {
        dateparmStatus = dateParmsFile.open();
        checkOpenOrClose(dateparmStatus, "ERROR OPENING DATE PARM FILE");
    }

    // 1500-A-LOOKUP-XREF
    void lookupXref() {
        ReadResult result = xrefFile.read(fdXrefCardNum);
        cardxrefStatus = result.status();
        if (FileStatus.isInvalidKey(cardxrefStatus)) {
            invalidKey("INVALID CARD NUMBER : " + fdXrefCardNum);
        } else if (FileStatus.isSuccessful(cardxrefStatus)) {
            cardXrefRecord = CardXrefRecord.parse(result.record());
        }
    }

    // 1500-B-LOOKUP-TRANTYPE
    void lookupTrantype() {
        ReadResult result = trantypeFile.read(fdTranType);
        trantypeStatus = result.status();
        if (FileStatus.isInvalidKey(trantypeStatus)) {
            invalidKey("INVALID TRANSACTION TYPE : " + fdTranType);
        } else if (FileStatus.isSuccessful(trantypeStatus)) {
            tranTypeRecord = TranTypeRecord.parse(result.record());
        }
    }

    // 1500-C-LOOKUP-TRANCATG
    void lookupTrancatg() {
        ReadResult result = trancatgFile.read(fdTranCatKey);
        trancatgStatus = result.status();
        if (FileStatus.isInvalidKey(trancatgStatus)) {
            invalidKey("INVALID TRAN CATG KEY : " + fdTranCatKey);
        } else if (FileStatus.isSuccessful(trancatgStatus)) {
            tranCatRecord = TranCatRecord.parse(result.record());
        }
    }

    // 9000-TRANFILE-CLOSE
    void tranfileClose() {
        tranfileStatus = transactFile.close();
        checkOpenOrClose(tranfileStatus, "ERROR CLOSING POSTED TRANSACTION FILE");
    }

    // 9100-REPTFILE-CLOSE
    void reptfileClose() {
        tranreptStatus = reportFile.close();
        checkOpenOrClose(tranreptStatus, "ERROR CLOSING REPORT FILE");
    }

    // 9200-CARDXREF-CLOSE
    void cardxrefClose() {
        cardxrefStatus = xrefFile.close();
        checkOpenOrClose(cardxrefStatus, "ERROR CLOSING CROSS REF FILE");
    }

    // 9300-TRANTYPE-CLOSE
    void trantypeClose() {
        trantypeStatus = trantypeFile.close();
        checkOpenOrClose(trantypeStatus, "ERROR CLOSING TRANSACTION TYPE FILE");
    }

    // 9400-TRANCATG-CLOSE
    void trancatgClose() {
        trancatgStatus = trancatgFile.close();
        checkOpenOrClose(trancatgStatus, "ERROR CLOSING TRANSACTION CATG FILE");
    }

    // 9500-DATEPARM-CLOSE
    void dateparmClose() {
        dateparmStatus = dateParmsFile.close();
        checkOpenOrClose(dateparmStatus, "ERROR CLOSING DATE PARM FILE");
    }

    // 9999-ABEND-PROGRAM
    void abendProgram(String reason) {
        sysout.println("ABENDING PROGRAM");
        throw new AbendException(ABEND_CODE, reason);
    }

    // 9910-DISPLAY-IO-STATUS
    void displayIoStatus() {
        sysout.println("FILE STATUS IS: NNNN" + formatIoStatus(ioStatus));
    }

    /** IO-STATUS-04 as built by 9910-DISPLAY-IO-STATUS. */
    static String formatIoStatus(String ioStatus) {
        String status = Fixed.pic(ioStatus, 2);
        char stat1 = status.charAt(0);
        char stat2 = status.charAt(1);
        if (!Fixed.isNumeric(status) || stat1 == '9') {
            // TWO-BYTES-BINARY = X'00' || IO-STAT2, moved to IO-STATUS-0403 PIC 999
            int twoBytesBinary = stat2 & 0xFF;
            return stat1 + Fixed.unsigned(twoBytesBinary, 3);
        }
        return "00" + status;
    }

    /** 88 APPL-AOK VALUE 0. */
    boolean applAok() {
        return applResult == APPL_RESULT_AOK;
    }

    /** 88 APPL-EOF VALUE 16. */
    boolean applEof() {
        return applResult == APPL_RESULT_EOF;
    }

    private void checkOpenOrClose(String status, String errorMessage) {
        applResult = FileStatus.OK.equals(status) ? 0 : 12;
        if (!applAok()) {
            sysout.println(errorMessage);
            ioStatus = status;
            displayIoStatus();
            abendProgram(errorMessage);
        }
    }

    private void invalidKey(String message) {
        sysout.println(message);
        ioStatus = "23";
        displayIoStatus();
        abendProgram(message);
    }

    private String tranAmtDisplay() {
        return tranRecordImage.substring(TranRecord.TRAN_AMT_OFFSET, TranRecord.TRAN_AMT_OFFSET + TranRecord.TRAN_AMT_LENGTH);
    }

    private void incrementLineCounter() {
        wsLineCounter = (wsLineCounter + 1) % LINE_COUNTER_MODULUS;
    }

    private static BigDecimal addTotal(BigDecimal total, BigDecimal amount) {
        return CobolNumeric.truncate(total.add(amount), TOTAL_INTEGER_DIGITS, TOTAL_SCALE);
    }

    private static BigDecimal zeroTotal() {
        return BigDecimal.ZERO.setScale(TOTAL_SCALE);
    }

    // Visible for tests: working-storage snapshots.
    long lineCounter() {
        return wsLineCounter;
    }

    BigDecimal grandTotal() {
        return wsGrandTotal;
    }

    BigDecimal pageTotal() {
        return wsPageTotal;
    }

    BigDecimal accountTotal() {
        return wsAccountTotal;
    }

    boolean endOfFile() {
        return endOfFile;
    }

    String startDate() {
        return wsStartDate;
    }

    String endDate() {
        return wsEndDate;
    }
}
