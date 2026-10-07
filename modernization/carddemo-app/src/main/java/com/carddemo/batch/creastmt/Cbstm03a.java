package com.carddemo.batch.creastmt;

import com.carddemo.account.AccountRecord;
import com.carddemo.batch.creastmt.Cbstm03b.Operation;
import com.carddemo.batch.creastmt.Cbstm03b.Response;
import com.carddemo.batch.harness.RecordSink;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.AbendException;
import com.carddemo.common.codec.CobolNumeric;
import com.carddemo.common.codec.Copybook;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.NumericEdited;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordFormatException;
import com.carddemo.common.codec.RecordLayout;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;

/**
 * {@code CBSTM03A} (CREASTMT STEP040): one plain-text statement (STMTFILE, 80-byte records) and one HTML statement
 * (HTMLFILE, 100-byte records) per CARDXREF record, i.e. per card. All file I/O goes through {@link Cbstm03b}.
 * <ol>
 * <li>Opens TRNXFILE and reads the whole TRXFL (sorted by card, then transaction id) into the two-dimensional
 * {@code WS-TRNX-TABLE} ({@code OCCURS 51} cards x {@code OCCURS 10} transactions of 334 bytes), then opens XREFFILE,
 * CUSTFILE and ACCTFILE. An empty TRXFL abends (the first read's status 10 is not accepted).</li>
 * <li>For each XREF record: keyed reads of CUSTFILE (XREF-CUST-ID) and ACCTFILE (XREF-ACCT-ID), statement header,
 * then the card's table entries (the scan stops at the first table card greater than the XREF card) with
 * {@code WS-TOTAL-AMT} summed, then the totals and trailer lines.</li>
 * </ol>
 * The table is kept as the COBOL storage: subscripts are not checked, so a card with more than 10 transactions writes
 * the 11th onwards over the next card's slot (and the next card overwrites them back), exactly as GnuCOBOL does;
 * an entry that would fall outside the 51 x 10 table, or a 52nd card, abends instead of overlaying other storage.
 * Rules doc CBSTM03A.md.
 */
public final class Cbstm03a {

    public static final String PROGRAM = "CBSTM03A";
    public static final String STMTFILE = "STMTFILE";
    public static final String HTMLFILE = "HTMLFILE";
    public static final int STMT_LRECL = 80;
    public static final int HTML_LRECL = 100;
    public static final int MAX_CARDS = 51;
    public static final int MAX_TRANSACTIONS = 10;

    static final RecordLayout TRNX_LAYOUT = Copybook.layout("COSTM01");
    static final RecordLayout CUST_LAYOUT = Copybook.layout("CUSTREC");
    static final int CARD_LEN = 16;
    static final int TRAN_NUM_LEN = 16;
    static final int TRAN_REST_LEN = 318;
    static final int TRAN_LEN = TRAN_NUM_LEN + TRAN_REST_LEN;
    static final int CARD_SLOT_LEN = CARD_LEN + MAX_TRANSACTIONS * TRAN_LEN;
    /** TRNX-DESC and TRNX-AMT inside TRNX-REST. */
    static final int REST_DESC_OFFSET = 16;
    static final int REST_DESC_LEN = 100;
    static final int REST_AMT_OFFSET = 116;
    static final int REST_AMT_LEN = 11;

    static final String AMOUNT_PICTURE = "Z(9).99-";
    static final String BALANCE_PICTURE = "9(9).99-";

    static final String ST_LINE0 = "*".repeat(31) + "START OF STATEMENT" + "*".repeat(31);
    static final String ST_LINE5 = "-".repeat(80);
    static final String ST_LINE6 = " ".repeat(33) + "Basic Details " + " ".repeat(33);
    static final String ST_LINE11 = " ".repeat(30) + "TRANSACTION SUMMARY " + " ".repeat(30);
    static final String ST_LINE13 = "Tran ID         " + pad("Tran Details    ", 51) + "  Tran Amount";
    static final String ST_LINE15 = "*".repeat(32) + "END OF STATEMENT" + "*".repeat(32);

    static final String HTML_L01 = "<!DOCTYPE html>";
    static final String HTML_L02 = "<html lang=\"en\">";
    static final String HTML_L03 = "<head>";
    static final String HTML_L04 = "<meta charset=\"utf-8\">";
    static final String HTML_L05 = "<title>HTML Table Layout</title>";
    static final String HTML_L06 = "</head>";
    static final String HTML_L07 = "<body style=\"margin:0px;\">";
    static final String HTML_L08 =
            "<table  align=\"center\" frame=\"box\" style=\"width:70%; font:12px Segoe UI,sans-serif;\">";
    static final String HTML_LTRS = "<tr>";
    static final String HTML_LTRE = "</tr>";
    static final String HTML_LTDE = "</td>";
    static final String HTML_L10 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">";
    static final String HTML_L15 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#FFAF33;\">";
    static final String HTML_L16 = "<p style=\"font-size:16px\">Bank of XYZ</p>";
    static final String HTML_L17 = "<p>410 Terry Ave N</p>";
    static final String HTML_L18 = "<p>Seattle WA 99999</p>";
    static final String HTML_L22_35 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#f2f2f2;\">";
    static final String HTML_L30_42 =
            "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#33FFD1; text-align:center;\">";
    static final String HTML_L31 = "<p style=\"font-size:16px\">Basic Details</p>";
    static final String HTML_L43 = "<p style=\"font-size:16px\">Transaction Summary</p>";
    static final String HTML_L47 =
            "<td style=\"width:25%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">";
    static final String HTML_L48 = "<p style=\"font-size:16px\">Tran ID</p>";
    static final String HTML_L50 =
            "<td style=\"width:55%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">";
    static final String HTML_L51 = "<p style=\"font-size:16px\">Tran Details</p>";
    static final String HTML_L53 =
            "<td style=\"width:20%; padding:0px 5px; background-color:#33FF5E; text-align:right;\">";
    static final String HTML_L54 = "<p style=\"font-size:16px\">Amount</p>";
    static final String HTML_L58 =
            "<td style=\"width:25%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">";
    static final String HTML_L61 =
            "<td style=\"width:55%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">";
    static final String HTML_L64 =
            "<td style=\"width:20%; padding:0px 5px; background-color:#f2f2f2; text-align:right;\">";
    static final String HTML_L75 = "<h3>End of Statement</h3>";
    static final String HTML_L78 = "</table>";
    static final String HTML_L79 = "</body>";
    static final String HTML_L80 = "</html>";
    static final String HTML_P16 = "<p style=\"font-size:16px\">";

    /**
     * {@code read}: TRNXFILE records; {@code statements}: XREF records (one statement each); {@code transactions}:
     * transaction lines written; {@code stmtLines} / {@code htmlLines}: records written.
     */
    public record Result(long read, long statements, long transactions, long stmtLines, long htmlLines,
                         ReturnCode returnCode) {
    }

    private final Cbstm03b files;
    private final RecordSink stmtfile;
    private final RecordSink htmlfile;
    private final RecordEncoding encoding;
    private final Sysout sysout;
    private final String jobName;
    private final String stepName;

    private final TransactionTable table;
    private final boolean htmlEscape;
    private long read;
    private long statements;
    private long transactions;

    public Cbstm03a(Cbstm03b files, RecordSink stmtfile, RecordSink htmlfile, RecordEncoding encoding, Sysout sysout,
                    String jobName, String stepName) {
        this(files, stmtfile, htmlfile, encoding, sysout, jobName, stepName, false);
    }

    /**
     * @param htmlEscape {@code carddemo.batch.creastmt.html-escape}: escape the customer name and address lines of
     *                   STATEMNT.HTML (rules doc CBSTM03A.md, Deviation D-1); {@code false} keeps the legacy bytes
     */
    public Cbstm03a(Cbstm03b files, RecordSink stmtfile, RecordSink htmlfile, RecordEncoding encoding, Sysout sysout,
                    String jobName, String stepName, boolean htmlEscape) {
        this.htmlEscape = htmlEscape;
        this.files = files;
        this.stmtfile = stmtfile;
        this.htmlfile = htmlfile;
        this.encoding = encoding;
        this.sysout = sysout;
        this.jobName = jobName;
        this.stepName = stepName;
        this.table = new TransactionTable(encoding);
    }

    /**
     * {@code 1000-MAINLINE}: per CARDXREF record ({@code 1000-XREFFILE-GET-NEXT}) the customer and account reads,
     * the statement and its transactions; the opens at the top replace the {@code 0000-START} {@code ALTER}/{@code GO
     * TO}
     * open sequence.
     */
    public Result run() {
        // DISPLAY 'Running JCL : ' TIOTNJOB ' Step ' TIOTJSTP; the TIOT DD walk that follows is z/OS control-block
        // introspection and is not reproduced (rules doc CBSTM03A.md, R-1).
        sysout.display("Running JCL : ", pad(jobName, 8), " Step ", pad(stepName, 8));
        stmtfile.open();
        htmlfile.open();
        loadTransactions();
        open(Cbstm03b.XREFFILE);
        open(Cbstm03b.CUSTFILE);
        open(Cbstm03b.ACCTFILE);
        while (true) {
            Response xref = files.call(Cbstm03b.XREFFILE, Operation.READ);
            if (xref.is("10")) {
                break;
            }
            if (!xref.is("00")) {
                throw abend("ERROR READING XREFFILE", xref.returnCode());
            }
            FixedWidthRecord xrefRecord = xref.record().orElseThrow().as(CardXrefRecord.MAPPER.layout());
            FixedWidthRecord customer = keyed(Cbstm03b.CUSTFILE, xrefRecord.getString("XREF-CUST-ID"))
                    .as(CUST_LAYOUT);
            FixedWidthRecord account = keyed(Cbstm03b.ACCTFILE, xrefRecord.getString("XREF-ACCT-ID"))
                    .as(AccountRecord.MAPPER.layout());
            statements++;
            createStatement(customer, account);
            writeTransactions(xrefRecord.bytes());
        }
        close(Cbstm03b.TRNXFILE);
        close(Cbstm03b.XREFFILE);
        close(Cbstm03b.CUSTFILE);
        close(Cbstm03b.ACCTFILE);
        stmtfile.close();
        htmlfile.close();
        return new Result(read, statements, transactions, stmtfile.count(), htmlfile.count(), ReturnCode.OK);
    }

    /**
     * {@code 8100-TRNXFILE-OPEN} + {@code 8500-READTRNX-READ}: the whole TRXFL into WS-TRNX-TABLE / WS-TRN-TBL-CNTR.
     */
    private void loadTransactions() {
        open(Cbstm03b.TRNXFILE);
        Response first = files.call(Cbstm03b.TRNXFILE, Operation.READ);
        if (!first.is("00") && !first.is("04")) {
            throw abend("ERROR READING TRNXFILE", first.returnCode());
        }
        byte[] trnx = trnxImage(first);
        read++;
        byte[] saveCard = Arrays.copyOfRange(trnx, 0, CARD_LEN);
        int crCnt = 1;
        int trCnt = 0;
        while (true) {
            if (Arrays.equals(saveCard, 0, CARD_LEN, trnx, 0, CARD_LEN)) {
                trCnt++;
            } else {
                table.setCount(crCnt, trCnt);
                crCnt++;
                trCnt = 1;
            }
            table.store(crCnt, trCnt, trnx);
            saveCard = Arrays.copyOfRange(trnx, 0, CARD_LEN);
            Response next = files.call(Cbstm03b.TRNXFILE, Operation.READ);
            if (next.is("00")) {
                trnx = trnxImage(next);
                read++;
            } else if (next.is("10")) {
                break;
            } else {
                throw abend("ERROR READING TRNXFILE", next.returnCode());
            }
        }
        table.setCount(crCnt, trCnt);
        table.cards = crCnt;
    }

    /** MOVE WS-M03B-FLDT TO TRNX-RECORD: the first 350 bytes of the area, space-padded. */
    private byte[] trnxImage(Response response) {
        byte[] image = new byte[TRNX_LAYOUT.length()];
        Arrays.fill(image, encoding.space());
        byte[] data = response.record().orElseThrow().bytes();
        System.arraycopy(data, 0, image, 0, Math.min(data.length, image.length));
        return image;
    }

    /** {@code 4000-TRNXFILE-GET}: the XREF card's transactions, the total and the statement trailers. */
    private void writeTransactions(byte[] xrefCard) {
        BigDecimal total = BigDecimal.ZERO;
        for (int crJmp = 1; crJmp <= table.cards
                && Arrays.compareUnsigned(table.area, table.cardOffset(crJmp), table.cardOffset(crJmp) + CARD_LEN,
                xrefCard, 0, CARD_LEN) <= 0; crJmp++) {
            if (Arrays.equals(table.area, table.cardOffset(crJmp), table.cardOffset(crJmp) + CARD_LEN, xrefCard, 0,
                    CARD_LEN)) {
                for (int trJmp = 1; trJmp <= table.counts[crJmp - 1]; trJmp++) {
                    int offset = table.tranOffset(crJmp, trJmp);
                    String tranId = encoding.decode(table.area, offset, TRAN_NUM_LEN);
                    String rest = encoding.decode(table.area, offset + TRAN_NUM_LEN, TRAN_REST_LEN);
                    BigDecimal amount = zoned(rest.substring(REST_AMT_OFFSET, REST_AMT_OFFSET + REST_AMT_LEN));
                    writeTransaction(tranId, rest.substring(REST_DESC_OFFSET, REST_DESC_OFFSET + REST_DESC_LEN),
                            amount);
                    // ADD TRNX-AMT TO WS-TOTAL-AMT (COMP-3 S9(9)V99, no ON SIZE ERROR: high-order truncation)
                    total = CobolNumeric.truncate(total.add(amount), 11, 2, true);
                }
            }
        }
        stmt(ST_LINE5);
        stmt("Total EXP:" + " ".repeat(56) + "$" + NumericEdited.format(total, AMOUNT_PICTURE));
        stmt(ST_LINE15);
        html(HTML_LTRS, HTML_L10, HTML_L75, HTML_LTDE, HTML_LTRE, HTML_L78, HTML_L79, HTML_L80);
    }

    /** {@code 6000-WRITE-TRANS}. */
    private void writeTransaction(String tranId, String desc, BigDecimal amount) {
        transactions++;
        String stTranId = pad(tranId, 16);
        String stTranDt = pad(desc, 49);
        String stTranAmt = NumericEdited.format(amount, AMOUNT_PICTURE);
        stmt(stTranId + " " + stTranDt + "$" + stTranAmt);
        html(HTML_LTRS, HTML_L58);
        html(string(HTML_LRECL, "<p>", "*", stTranId, "*", "</p>", "*"));
        html(HTML_LTDE, HTML_L61);
        html(string(HTML_LRECL, "<p>", "*", stTranDt, "*", "</p>", "*"));
        html(HTML_LTDE, HTML_L64);
        html(string(HTML_LRECL, "<p>", "*", stTranAmt, "*", "</p>", "*"));
        html(HTML_LTDE, HTML_LTRE);
    }

    /** {@code 5000-CREATE-STATEMENT} + {@code 5100-WRITE-HTML-HEADER} + {@code 5200-WRITE-HTML-NMADBS}. */
    private void createStatement(FixedWidthRecord customer, FixedWidthRecord account) {
        String acctId = account.getString("ACCT-ID");
        stmt(ST_LINE0);
        html(HTML_L01, HTML_L02, HTML_L03, HTML_L04, HTML_L05, HTML_L06, HTML_L07, HTML_L08, HTML_LTRS, HTML_L10);
        html("<h3>Statement for Account Number: " + pad(acctId, 20) + "</h3>");
        html(HTML_LTDE, HTML_LTRE, HTML_LTRS, HTML_L15, HTML_L16, HTML_L17, HTML_L18, HTML_LTDE, HTML_LTRE,
                HTML_LTRS, HTML_L22_35);

        String stName = string(75, customer.getString("CUST-FIRST-NAME"), " ", " ", null,
                customer.getString("CUST-MIDDLE-NAME"), " ", " ", null,
                customer.getString("CUST-LAST-NAME"), " ", " ", null);
        String stAdd1 = pad(customer.getString("CUST-ADDR-LINE-1"), 50);
        String stAdd2 = pad(customer.getString("CUST-ADDR-LINE-2"), 50);
        String stAdd3 = string(80, customer.getString("CUST-ADDR-LINE-3"), " ", " ", null,
                customer.getString("CUST-ADDR-STATE-CD"), " ", " ", null,
                customer.getString("CUST-ADDR-COUNTRY-CD"), " ", " ", null,
                customer.getString("CUST-ADDR-ZIP"), " ", " ", null);
        String stAcctId = pad(acctId, 20);
        // MOVE ACCT-CURR-BAL (S9(10)V99) TO ST-CURR-BAL (9(9).99-): the high-order digit is lost
        String stCurrBal = NumericEdited.format(account.getDecimal("ACCT-CURR-BAL"), BALANCE_PICTURE);
        String stFicoScore = pad(customer.getString("CUST-FICO-CREDIT-SCORE"), 20);

        String l23Name = stName.substring(0, 50);
        html(dataLine(HTML_P16, l23Name));
        for (String address : List.of(stAdd1, stAdd2, stAdd3)) {
            html(dataLine("<p>", address));
        }
        html(HTML_LTDE, HTML_LTRE, HTML_LTRS, HTML_L30_42, HTML_L31, HTML_LTDE, HTML_LTRE, HTML_LTRS, HTML_L22_35);
        html(string(HTML_LRECL, "<p>Account ID         : ", "*", stAcctId, "*", "</p>", "*"));
        html(string(HTML_LRECL, "<p>Current Balance    : ", "*", stCurrBal, "*", "</p>", "*"));
        html(string(HTML_LRECL, "<p>FICO Score         : ", "*", stFicoScore, "*", "</p>", "*"));
        html(HTML_LTDE, HTML_LTRE, HTML_LTRS, HTML_L30_42, HTML_L43, HTML_LTDE, HTML_LTRE, HTML_LTRS, HTML_L47,
                HTML_L48, HTML_LTDE, HTML_L50, HTML_L51, HTML_LTDE, HTML_L53, HTML_L54, HTML_LTDE, HTML_LTRE);

        stmt(pad(stName, 75) + " ".repeat(5));
        stmt(stAdd1 + " ".repeat(30));
        stmt(stAdd2 + " ".repeat(30));
        stmt(stAdd3);
        stmt(ST_LINE5);
        stmt(ST_LINE6);
        stmt(ST_LINE5);
        stmt("Account ID         :" + stAcctId + " ".repeat(40));
        stmt("Current Balance    :" + stCurrBal + " ".repeat(47));
        stmt("FICO Score         :" + stFicoScore + " ".repeat(40));
        stmt(ST_LINE5);
        stmt(ST_LINE11);
        stmt(ST_LINE5);
        stmt(ST_LINE13);
        stmt(ST_LINE5);
    }

    /**
     * CBSTM03A {@code 2000-CUSTFILE-GET} / CBSTM03A {@code 3000-ACCTFILE-GET}: keyed read through CBSTM03B; anything
     * but 00 abends.
     */
    private FixedWidthRecord keyed(String dd, String key) {
        Response response = files.call(dd, Operation.READ_KEY, key, key.length());
        if (!response.is("00")) {
            throw abend("ERROR READING " + dd, response.returnCode());
        }
        return response.record().orElseThrow();
    }

    private void open(String dd) {
        Response response = files.call(dd, Operation.OPEN);
        if (!response.is("00") && !response.is("04")) {
            throw abend("ERROR OPENING " + dd, response.returnCode());
        }
    }

    private void close(String dd) {
        Response response = files.call(dd, Operation.CLOSE);
        if (!response.is("00") && !response.is("04")) {
            throw abend("ERROR CLOSING " + dd, response.returnCode());
        }
    }

    /** 9999-ABEND-PROGRAM: {@code CALL 'CEE3ABD'} without an abend code. */
    private AbendException abend(String message, String returnCode) {
        sysout.display(message);
        sysout.display("RETURN CODE: ", returnCode);
        sysout.display("ABENDING PROGRAM");
        return AbendException.carddemo(message + " (RC " + returnCode + ")", null);
    }

    private void stmt(String line) {
        stmtfile.write(new FixedWidthRecord(encoding.encode(pad(line, STMT_LRECL)), encoding));
    }

    /**
     * A name/address line of {@code 5200-WRITE-HTML-NMADBS}: {@code prefix} + {@code value DELIMITED BY '  '} + two
     * spaces + {@code </p>}. With {@link #htmlEscape} the value is HTML-escaped first, cut before an entity that
     * would not fit in the 100-byte record.
     */
    private String dataLine(String prefix, String value) {
        if (!htmlEscape) {
            return string(HTML_LRECL, prefix, "*", value, "  ", "  ", null, "</p>", "*");
        }
        int cut = value.indexOf("  ");
        String text = cut < 0 ? value : value.substring(0, cut);
        String escaped = escape(text, HTML_LRECL - prefix.length() - "  </p>".length());
        return string(HTML_LRECL, prefix, "*", escaped, null, "  ", null, "</p>", "*");
    }

    /** {@code & < >} as entities (text content only, quotes need no escaping there), at most {@code room} characters, never cutting inside an entity. */
    static String escape(String text, int room) {
        StringBuilder out = new StringBuilder(text.length());
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            String piece = switch (c) {
                case '&' -> "&amp;";
                case '<' -> "&lt;";
                case '>' -> "&gt;";
                default -> String.valueOf(c);
            };
            if (out.length() + piece.length() > room) {
                break;
            }
            out.append(piece);
        }
        return out.toString();
    }

    private void html(String... lines) {
        for (String line : lines) {
            htmlfile.write(new FixedWidthRecord(encoding.encode(pad(line, HTML_LRECL)), encoding));
        }
    }

    /**
     * {@code STRING source DELIMITED BY delimiter ... INTO target} into a space-filled target of {@code length}:
     * {@code parts} are (source, delimiter) pairs, a null delimiter meaning {@code DELIMITED BY SIZE}. Each source
     * is cut before the first occurrence of its delimiter; the transfer stops when the target is full (overflow).
     */
    static String string(int length, String... parts) {
        StringBuilder out = new StringBuilder(length);
        for (int i = 0; i < parts.length && out.length() < length; i += 2) {
            String source = parts[i];
            String delimiter = parts[i + 1];
            int cut = delimiter == null ? -1 : source.indexOf(delimiter);
            out.append(cut < 0 ? source : source.substring(0, cut));
        }
        return pad(out.toString(), length);
    }

    static String pad(String value, int length) {
        String v = value == null ? "" : value;
        return v.length() >= length ? v.substring(0, length) : v + " ".repeat(length - v.length());
    }

    /**
     * TRNX-AMT as GnuCOBOL reads a DISPLAY {@code S9(9)V99}: invalid digit bytes (spaces in an overlaid table entry)
     * count as their low nibble, a non-overpunch sign byte as positive.
     */
    static BigDecimal zoned(String text) {
        try {
            return CobolNumeric.decodeZoned(text, 2, true);
        } catch (RecordFormatException e) {
            StringBuilder digits = new StringBuilder(text.length());
            for (int i = 0; i < text.length() - 1; i++) {
                digits.append(digit(text.charAt(i)));
            }
            char last = text.charAt(text.length() - 1);
            try {
                return CobolNumeric.decodeZoned(digits.toString() + last, 2, true);
            } catch (RecordFormatException ignored) {
                return CobolNumeric.decodeZoned(digits.toString() + digit(last), 2, true);
            }
        }
    }

    private static char digit(char c) {
        if (c >= '0' && c <= '9') {
            return c;
        }
        int nibble = c & 0x0F;
        return nibble <= 9 ? (char) ('0' + nibble) : '0';
    }

    /** WS-TRNX-TABLE (51 x (16 + 10 x 334) bytes) and WS-TRN-TBL-CNTR (51 x S9(4) COMP) as COBOL storage. */
    final class TransactionTable {

        final byte[] area = new byte[MAX_CARDS * CARD_SLOT_LEN];
        final int[] counts = new int[MAX_CARDS];
        int cards;

        TransactionTable(RecordEncoding encoding) {
            Arrays.fill(area, encoding.space());
        }

        int cardOffset(int cr) {
            return (cr - 1) * CARD_SLOT_LEN;
        }

        int tranOffset(int cr, int tr) {
            return cardOffset(cr) + CARD_LEN + (tr - 1) * TRAN_LEN;
        }

        /** MOVE TRNX-CARD-NUM / TRNX-ID / TRNX-REST TO WS-CARD-NUM (cr) / WS-TRAN-NUM (cr, tr) / WS-TRAN-REST. */
        void store(int cr, int tr, byte[] trnx) {
            if (cr > MAX_CARDS || tranOffset(cr, tr) + TRAN_LEN > area.length) {
                throw overflow(cr, tr);
            }
            System.arraycopy(trnx, 0, area, cardOffset(cr), CARD_LEN);
            System.arraycopy(trnx, CARD_LEN, area, tranOffset(cr, tr), TRAN_LEN);
        }

        /** MOVE TR-CNT TO WS-TRCT (cr). */
        void setCount(int cr, int count) {
            if (cr > MAX_CARDS) {
                throw overflow(cr, count);
            }
            counts[cr - 1] = count;
        }

        private AbendException overflow(int cr, int tr) {
            String message = "WS-TRNX-TABLE OVERFLOW: CARD " + cr + " TRANSACTION " + tr + " IS OUTSIDE THE "
                    + MAX_CARDS + " x " + MAX_TRANSACTIONS + " TABLE";
            sysout.display(message);
            sysout.display("ABENDING PROGRAM");
            return AbendException.carddemo(message, null);
        }
    }
}
