package com.carddemo.programs.cbstm03a;

import com.carddemo.programs.cbstm03a.copybook.AccountRecord;
import com.carddemo.programs.cbstm03a.copybook.CardXrefRecord;
import com.carddemo.programs.cbstm03a.copybook.CustomerRecord;
import com.carddemo.programs.cbstm03a.copybook.M03bArea;
import com.carddemo.programs.cbstm03a.copybook.TrnxRecord;
import com.carddemo.programs.cbstm03a.io.RecordWriter;
import com.carddemo.programs.cbstm03a.support.CobolString;
import com.carddemo.programs.cbstm03a.support.Pic;
import java.math.BigDecimal;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * CBSTM03A - prints account statements (STMTFILE, 80-byte text) and their HTML
 * rendition (HTMLFILE, 100-byte records) from the card cross-reference, customer,
 * account and (card-number-keyed) transaction files, all read via CBSTM03B.
 *
 * <p>The COBOL control flow (ALTER / GO TO driven by WS-FL-DD) is kept as an explicit
 * paragraph state machine: every paragraph that ends in {@code GO TO} returns the next
 * paragraph to run. {@code PERFORM}ed paragraphs are ordinary method calls.
 */
public final class Cbstm03a {

    static final int MAX_CARDS = 51;        // WS-CARD-TBL OCCURS 51 TIMES
    static final int MAX_TRANS_PER_CARD = 10; // WS-TRAN-TBL OCCURS 10 TIMES

    /** GO TO targets. */
    enum Paragraph {
        START_0000,
        MAINLINE_1000,
        FILE_OPEN_8100,
        TRNXFILE_OPEN_8100,
        XREFFILE_OPEN_8200,
        CUSTFILE_OPEN_8300,
        ACCTFILE_OPEN_8400,
        READTRNX_READ_8500,
        EXIT_8599,
        GOBACK_9999
    }

    private final JobContext job;
    private final Cbstm03bCall cbstm03b;
    private final RecordWriter stmtFile;
    private final RecordWriter htmlFile;
    private final Consumer<String> display;

    // COMP-VARIABLES
    private int crCnt;
    private int trCnt;
    // COMP3-VARIABLES / MISC-VARIABLES
    private BigDecimal wsTotalAmt = BigDecimal.ZERO.setScale(2);
    private String wsFlDd = "TRNXFILE";
    private BigDecimal wsTrnAmt = BigDecimal.ZERO.setScale(2);
    private String wsSaveCard = Pic.x("", 16);
    private String endOfFile = "N";
    private final M03bArea wsM03bArea = new M03bArea();
    private final StatementLines statementLines = new StatementLines();
    private HtmlFixedLine htmlFixedLn;
    // WS-TRNX-TABLE / WS-TRN-TBL-CNTR
    private final String[] wsCardNum = new String[MAX_CARDS];
    private final String[][] wsTranNum = new String[MAX_CARDS][MAX_TRANS_PER_CARD];
    private final String[][] wsTranRest = new String[MAX_CARDS][MAX_TRANS_PER_CARD];
    private final int[] wsTrct = new int[MAX_CARDS];
    // COPY COSTM01 / CVACT03Y / CUSTREC / CVACT01Y
    private TrnxRecord trnxRecord = TrnxRecord.parse("");
    private CardXrefRecord cardXrefRecord = CardXrefRecord.parse("");
    private CustomerRecord customerRecord = CustomerRecord.parse("");
    private AccountRecord accountRecord = AccountRecord.parse("");
    // ALTER 8100-FILE-OPEN TO PROCEED TO ...
    private Paragraph fileOpen8100Target = Paragraph.TRNXFILE_OPEN_8100;

    public Cbstm03a(JobContext job, Cbstm03bCall cbstm03b, RecordWriter stmtFile, RecordWriter htmlFile,
            Consumer<String> display) {
        this.job = Objects.requireNonNull(job);
        this.cbstm03b = Objects.requireNonNull(cbstm03b);
        this.stmtFile = Objects.requireNonNull(stmtFile);
        this.htmlFile = Objects.requireNonNull(htmlFile);
        this.display = Objects.requireNonNull(display);
    }

    /**
     * Runs the program to GOBACK.
     *
     * @throws Cbstm03aAbendException on any 9999-ABEND-PROGRAM path
     */
    public void run() {
        procedureDivision();
        initializeTrnxTable();
        Paragraph next = Paragraph.START_0000;
        while (next != null) {
            next = switch (next) {
                case START_0000 -> start0000();
                case MAINLINE_1000 -> mainline1000();
                case FILE_OPEN_8100 -> fileOpen8100();
                case TRNXFILE_OPEN_8100 -> trnxfileOpen8100();
                case XREFFILE_OPEN_8200 -> xreffileOpen8200();
                case CUSTFILE_OPEN_8300 -> custfileOpen8300();
                case ACCTFILE_OPEN_8400 -> acctfileOpen8400();
                case READTRNX_READ_8500 -> readtrnxRead8500();
                case EXIT_8599 -> exit8599();
                case GOBACK_9999 -> null;
            };
        }
    }

    // PROCEDURE DIVISION prologue: TIOT walk (via JobContext) + OPEN OUTPUT
    private void procedureDivision() {
        display.accept("Running JCL : " + Pic.x(job.jobName(), 8) + " Step " + Pic.x(job.stepName(), 8));
        display.accept("DD Names from TIOT: ");
        for (JobContext.DdEntry entry : job.ddEntries()) {
            display.accept(": " + Pic.x(entry.ddName(), 8)
                    + (entry.validUcb() ? " -- valid UCB" : " --  null UCB"));
        }
        // The IF after END-PERFORM re-displays the terminating (all LOW-VALUES) entry.
        display.accept(": " + "\0".repeat(8) + " -- null  UCB");
    }

    // INITIALIZE WS-TRNX-TABLE WS-TRN-TBL-CNTR
    private void initializeTrnxTable() {
        for (int c = 0; c < MAX_CARDS; c++) {
            wsCardNum[c] = Pic.x("", 16);
            wsTrct[c] = 0;
            for (int t = 0; t < MAX_TRANS_PER_CARD; t++) {
                wsTranNum[c][t] = Pic.x("", 16);
                wsTranRest[c][t] = Pic.x("", TrnxRecord.REST_LENGTH);
            }
        }
    }

    // 0000-START
    private Paragraph start0000() {
        return switch (wsFlDd) {
            case "TRNXFILE" -> alter8100(Paragraph.TRNXFILE_OPEN_8100);
            case "XREFFILE" -> alter8100(Paragraph.XREFFILE_OPEN_8200);
            case "CUSTFILE" -> alter8100(Paragraph.CUSTFILE_OPEN_8300);
            case "ACCTFILE" -> alter8100(Paragraph.ACCTFILE_OPEN_8400);
            case "READTRNX" -> Paragraph.READTRNX_READ_8500;
            default -> Paragraph.GOBACK_9999;
        };
    }

    private Paragraph alter8100(Paragraph target) {
        fileOpen8100Target = target;
        return Paragraph.FILE_OPEN_8100;
    }

    // 1000-MAINLINE (falls through into 9999-GOBACK)
    private Paragraph mainline1000() {
        while (!endOfFile.equals("Y")) {
            if (endOfFile.equals("N")) {
                xreffileGetNext1000();
                if (endOfFile.equals("N")) {
                    custfileGet2000();
                    acctfileGet3000();
                    createStatement5000();
                    wsTotalAmt = BigDecimal.ZERO.setScale(2);
                    trnxfileGet4000();
                }
            }
        }
        trnxfileClose9100();
        xreffileClose9200();
        custfileClose9300();
        acctfileClose9400();
        stmtFile.close();
        htmlFile.close();
        return Paragraph.GOBACK_9999;
    }

    // 1000-XREFFILE-GET-NEXT
    private void xreffileGetNext1000() {
        wsM03bArea.dd("XREFFILE");
        wsM03bArea.oper(M03bArea.Oper.READ);
        wsM03bArea.rc("00");
        wsM03bArea.fldt("");
        cbstm03b.call(wsM03bArea);
        switch (wsM03bArea.rc()) {
            case "00" -> {
            }
            case "10" -> endOfFile = "Y";
            default -> abendProgram9999("ERROR READING XREFFILE");
        }
        cardXrefRecord = CardXrefRecord.parse(wsM03bArea.fldt());
    }

    // 2000-CUSTFILE-GET
    private void custfileGet2000() {
        wsM03bArea.dd("CUSTFILE");
        wsM03bArea.oper(M03bArea.Oper.READ_K);
        wsM03bArea.key(cardXrefRecord.xrefCustIdDisplay());
        wsM03bArea.keyLn(CardXrefRecord.CUST_ID_LENGTH);
        wsM03bArea.rc("00");
        wsM03bArea.fldt("");
        cbstm03b.call(wsM03bArea);
        if (!wsM03bArea.rc().equals("00")) {
            abendProgram9999("ERROR READING CUSTFILE");
        }
        customerRecord = CustomerRecord.parse(wsM03bArea.fldt());
    }

    // 3000-ACCTFILE-GET
    private void acctfileGet3000() {
        wsM03bArea.dd("ACCTFILE");
        wsM03bArea.oper(M03bArea.Oper.READ_K);
        wsM03bArea.key(cardXrefRecord.xrefAcctIdDisplay());
        wsM03bArea.keyLn(CardXrefRecord.ACCT_ID_LENGTH);
        wsM03bArea.rc("00");
        wsM03bArea.fldt("");
        cbstm03b.call(wsM03bArea);
        if (!wsM03bArea.rc().equals("00")) {
            abendProgram9999("ERROR READING ACCTFILE");
        }
        accountRecord = AccountRecord.parse(wsM03bArea.fldt());
    }

    // 4000-TRNXFILE-GET
    private void trnxfileGet4000() {
        String xrefCardNum = cardXrefRecord.xrefCardNum();
        for (int crJmp = 1; !(crJmp > crCnt || wsCardNum[crJmp - 1].compareTo(xrefCardNum) > 0); crJmp++) {
            if (xrefCardNum.equals(wsCardNum[crJmp - 1])) {
                String cardNum = wsCardNum[crJmp - 1];
                for (int trJmp = 1; trJmp <= wsTrct[crJmp - 1]; trJmp++) {
                    trnxRecord = TrnxRecord.of(cardNum, wsTranNum[crJmp - 1][trJmp - 1],
                            wsTranRest[crJmp - 1][trJmp - 1]);
                    writeTrans6000();
                    wsTotalAmt = Pic.store(wsTotalAmt.add(trnxRecord.trnxAmt()), 9, 2);
                }
            }
        }
        wsTrnAmt = Pic.store(wsTotalAmt, 9, 2);
        statementLines.stTotalTramt = Pic.editZeroSuppressed(wsTrnAmt, 9);
        writeStmt(StatementLines.ST_LINE12);
        writeStmt(statementLines.stLine14a());
        writeStmt(StatementLines.ST_LINE15);

        writeHtml(HtmlFixedLine.HTML_LTRS);
        writeHtml(HtmlFixedLine.HTML_L10);
        writeHtml(HtmlFixedLine.HTML_L75);
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_LTRE);
        writeHtml(HtmlFixedLine.HTML_L78);
        writeHtml(HtmlFixedLine.HTML_L79);
        writeHtml(HtmlFixedLine.HTML_L80);
    }

    // 5000-CREATE-STATEMENT
    private void createStatement5000() {
        statementLines.initialize();
        writeStmt(StatementLines.ST_LINE0);
        writeHtmlHeader5100();
        statementLines.stName = CobolString.into(statementLines.stName)
                .delimitedBy(customerRecord.custFirstName(), " ").bySize(" ")
                .delimitedBy(customerRecord.custMiddleName(), " ").bySize(" ")
                .delimitedBy(customerRecord.custLastName(), " ").bySize(" ")
                .result();
        statementLines.stAdd1 = Pic.x(customerRecord.custAddrLine1(), 50);
        statementLines.stAdd2 = Pic.x(customerRecord.custAddrLine2(), 50);
        statementLines.stAdd3 = CobolString.into(statementLines.stAdd3)
                .delimitedBy(customerRecord.custAddrLine3(), " ").bySize(" ")
                .delimitedBy(customerRecord.custAddrStateCd(), " ").bySize(" ")
                .delimitedBy(customerRecord.custAddrCountryCd(), " ").bySize(" ")
                .delimitedBy(customerRecord.custAddrZip(), " ").bySize(" ")
                .result();
        statementLines.stAcctId = Pic.x(accountRecord.acctIdDisplay(), 20);
        statementLines.stCurrBal = Pic.editNines(accountRecord.acctCurrBal(), 9);
        statementLines.stFicoScore = Pic.x(customerRecord.custFicoCreditScoreDisplay(), 20);
        writeHtmlNmadbs5200();

        writeStmt(statementLines.stLine1());
        writeStmt(statementLines.stLine2());
        writeStmt(statementLines.stLine3());
        writeStmt(statementLines.stLine4());
        writeStmt(StatementLines.ST_LINE5);
        writeStmt(StatementLines.ST_LINE6);
        writeStmt(StatementLines.ST_LINE5);
        writeStmt(statementLines.stLine7());
        writeStmt(statementLines.stLine8());
        writeStmt(statementLines.stLine9());
        writeStmt(StatementLines.ST_LINE10);
        writeStmt(StatementLines.ST_LINE11);
        writeStmt(StatementLines.ST_LINE12);
        writeStmt(StatementLines.ST_LINE13);
        writeStmt(StatementLines.ST_LINE12);
    }

    // 5100-WRITE-HTML-HEADER THRU 5100-EXIT
    private void writeHtmlHeader5100() {
        writeHtml(HtmlFixedLine.HTML_L01);
        writeHtml(HtmlFixedLine.HTML_L02);
        writeHtml(HtmlFixedLine.HTML_L03);
        writeHtml(HtmlFixedLine.HTML_L04);
        writeHtml(HtmlFixedLine.HTML_L05);
        writeHtml(HtmlFixedLine.HTML_L06);
        writeHtml(HtmlFixedLine.HTML_L07);
        writeHtml(HtmlFixedLine.HTML_L08);
        writeHtml(HtmlFixedLine.HTML_LTRS);
        writeHtml(HtmlFixedLine.HTML_L10);

        // HTML-L11: '<h3>Statement for Account Number: ' L11-ACCT X(20) '</h3>'
        htmlFile.write("<h3>Statement for Account Number: " + Pic.x(accountRecord.acctIdDisplay(), 20) + "</h3>");
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_LTRE);
        writeHtml(HtmlFixedLine.HTML_LTRS);
        writeHtml(HtmlFixedLine.HTML_L15);
        writeHtml(HtmlFixedLine.HTML_L16);
        writeHtml(HtmlFixedLine.HTML_L17);
        writeHtml(HtmlFixedLine.HTML_L18);
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_LTRE);
        writeHtml(HtmlFixedLine.HTML_LTRS);
        writeHtml(HtmlFixedLine.HTML_L22_35);
    }

    // 5200-WRITE-HTML-NMADBS THRU 5200-EXIT
    private void writeHtmlNmadbs5200() {
        String l23Name = Pic.x(statementLines.stName, 50);
        htmlFile.write(CobolString.into(Pic.x("", HtmlFixedLine.LENGTH))
                .delimitedBy("<p style=\"font-size:16px\">", "*")
                .delimitedBy(l23Name, "  ")
                .bySize("  ")
                .delimitedBy("</p>", "*")
                .result());
        htmlFile.write(paragraphLine(statementLines.stAdd1, "  "));
        htmlFile.write(paragraphLine(statementLines.stAdd2, "  "));
        htmlFile.write(paragraphLine(statementLines.stAdd3, "  "));

        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_LTRE);
        writeHtml(HtmlFixedLine.HTML_LTRS);
        writeHtml(HtmlFixedLine.HTML_L30_42);
        writeHtml(HtmlFixedLine.HTML_L31);
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_LTRE);
        writeHtml(HtmlFixedLine.HTML_LTRS);
        writeHtml(HtmlFixedLine.HTML_L22_35);

        htmlFile.write(labelledLine("<p>Account ID         : ", statementLines.stAcctId));
        htmlFile.write(labelledLine("<p>Current Balance    : ", statementLines.stCurrBal));
        htmlFile.write(labelledLine("<p>FICO Score         : ", statementLines.stFicoScore));
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_LTRE);
        writeHtml(HtmlFixedLine.HTML_LTRS);
        writeHtml(HtmlFixedLine.HTML_L30_42);
        writeHtml(HtmlFixedLine.HTML_L43);
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_LTRE);
        writeHtml(HtmlFixedLine.HTML_LTRS);
        writeHtml(HtmlFixedLine.HTML_L47);
        writeHtml(HtmlFixedLine.HTML_L48);
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_L50);
        writeHtml(HtmlFixedLine.HTML_L51);
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_L53);
        writeHtml(HtmlFixedLine.HTML_L54);
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_LTRE);
    }

    // 6000-WRITE-TRANS
    private void writeTrans6000() {
        statementLines.stTranid = Pic.x(trnxRecord.trnxId(), 16);
        statementLines.stTrandt = Pic.x(trnxRecord.trnxDesc(), 49);
        statementLines.stTranamt = Pic.editZeroSuppressed(trnxRecord.trnxAmt(), 9);
        writeStmt(statementLines.stLine14());

        writeHtml(HtmlFixedLine.HTML_LTRS);
        writeHtml(HtmlFixedLine.HTML_L58);
        htmlFile.write(labelledLine("<p>", statementLines.stTranid));
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_L61);
        htmlFile.write(labelledLine("<p>", statementLines.stTrandt));
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_L64);
        htmlFile.write(labelledLine("<p>", statementLines.stTranamt));
        writeHtml(HtmlFixedLine.HTML_LTDE);
        writeHtml(HtmlFixedLine.HTML_LTRE);
    }

    // 8100-FILE-OPEN (GO TO altered by 0000-START)
    private Paragraph fileOpen8100() {
        return fileOpen8100Target;
    }

    // 8100-TRNXFILE-OPEN
    private Paragraph trnxfileOpen8100() {
        wsM03bArea.dd("TRNXFILE");
        wsM03bArea.oper(M03bArea.Oper.OPEN);
        wsM03bArea.rc("00");
        cbstm03b.call(wsM03bArea);
        requireOk("ERROR OPENING TRNXFILE");

        wsM03bArea.oper(M03bArea.Oper.READ);
        wsM03bArea.fldt("");
        cbstm03b.call(wsM03bArea);
        requireOk("ERROR READING TRNXFILE");

        trnxRecord = TrnxRecord.parse(wsM03bArea.fldt());
        wsSaveCard = trnxRecord.trnxCardNum();
        crCnt = 1;
        trCnt = 0;
        wsFlDd = "READTRNX";
        return Paragraph.START_0000;
    }

    // 8200-XREFFILE-OPEN
    private Paragraph xreffileOpen8200() {
        open("XREFFILE", "ERROR OPENING XREFFILE");
        wsFlDd = "CUSTFILE";
        return Paragraph.START_0000;
    }

    // 8300-CUSTFILE-OPEN
    private Paragraph custfileOpen8300() {
        open("CUSTFILE", "ERROR OPENING CUSTFILE");
        wsFlDd = "ACCTFILE";
        return Paragraph.START_0000;
    }

    // 8400-ACCTFILE-OPEN
    private Paragraph acctfileOpen8400() {
        open("ACCTFILE", "ERROR OPENING ACCTFILE");
        return Paragraph.MAINLINE_1000;
    }

    // 8500-READTRNX-READ
    private Paragraph readtrnxRead8500() {
        if (wsSaveCard.equals(trnxRecord.trnxCardNum())) {
            trCnt++;
        } else {
            wsTrct[crCnt - 1] = trCnt;
            crCnt++;
            trCnt = 1;
        }
        if (crCnt > MAX_CARDS || trCnt > MAX_TRANS_PER_CARD) {
            throw new IllegalStateException("WS-TRNX-TABLE overflow: card " + crCnt + " of " + MAX_CARDS
                    + ", transaction " + trCnt + " of " + MAX_TRANS_PER_CARD);
        }
        wsCardNum[crCnt - 1] = trnxRecord.trnxCardNum();
        wsTranNum[crCnt - 1][trCnt - 1] = trnxRecord.trnxId();
        wsTranRest[crCnt - 1][trCnt - 1] = trnxRecord.rest();
        wsSaveCard = trnxRecord.trnxCardNum();

        wsM03bArea.dd("TRNXFILE");
        wsM03bArea.oper(M03bArea.Oper.READ);
        wsM03bArea.fldt("");
        cbstm03b.call(wsM03bArea);
        return switch (wsM03bArea.rc()) {
            case "00" -> {
                trnxRecord = TrnxRecord.parse(wsM03bArea.fldt());
                yield Paragraph.READTRNX_READ_8500;
            }
            case "10" -> Paragraph.EXIT_8599;
            default -> {
                abendProgram9999("ERROR READING TRNXFILE");
                yield Paragraph.EXIT_8599;
            }
        };
    }

    // 8599-EXIT
    private Paragraph exit8599() {
        wsTrct[crCnt - 1] = trCnt;
        wsFlDd = "XREFFILE";
        return Paragraph.START_0000;
    }

    // 9100-TRNXFILE-CLOSE
    private void trnxfileClose9100() {
        close("TRNXFILE");
    }

    // 9200-XREFFILE-CLOSE
    private void xreffileClose9200() {
        close("XREFFILE");
    }

    // 9300-CUSTFILE-CLOSE
    private void custfileClose9300() {
        close("CUSTFILE");
    }

    // 9400-ACCTFILE-CLOSE
    private void acctfileClose9400() {
        close("ACCTFILE");
    }

    // 9999-ABEND-PROGRAM (preceded by the caller's two error DISPLAYs)
    private void abendProgram9999(String reason) {
        display.accept(reason);
        display.accept("RETURN CODE: " + wsM03bArea.rc());
        display.accept("ABENDING PROGRAM");
        throw new Cbstm03aAbendException(reason, wsM03bArea.rc());
    }

    private void open(String dd, String errorMessage) {
        wsM03bArea.dd(dd);
        wsM03bArea.oper(M03bArea.Oper.OPEN);
        wsM03bArea.rc("00");
        cbstm03b.call(wsM03bArea);
        requireOk(errorMessage);
    }

    private void close(String dd) {
        wsM03bArea.dd(dd);
        wsM03bArea.oper(M03bArea.Oper.CLOSE);
        wsM03bArea.rc("00");
        cbstm03b.call(wsM03bArea);
        requireOk("ERROR CLOSING " + dd);
    }

    // IF WS-M03B-RC = '00' OR '04' CONTINUE ELSE DISPLAY ... PERFORM 9999-ABEND-PROGRAM
    private void requireOk(String errorMessage) {
        if (!wsM03bArea.rc().equals("00") && !wsM03bArea.rc().equals("04")) {
            abendProgram9999(errorMessage);
        }
    }

    private static String labelledLine(String label, String value) {
        return CobolString.into(Pic.x("", HtmlFixedLine.LENGTH))
                .delimitedBy(label, "*")
                .delimitedBy(value, "*")
                .delimitedBy("</p>", "*")
                .result();
    }

    private static String paragraphLine(String value, String delimiter) {
        return CobolString.into(Pic.x("", HtmlFixedLine.LENGTH))
                .delimitedBy("<p>", "*")
                .delimitedBy(value, delimiter)
                .bySize("  ")
                .delimitedBy("</p>", "*")
                .result();
    }

    private void writeStmt(String line) {
        stmtFile.write(Pic.x(line, StatementLines.LENGTH));
    }

    private void writeHtml(HtmlFixedLine line) {
        htmlFixedLn = line;
        htmlFile.write(htmlFixedLn.line());
    }
}
