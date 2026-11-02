package com.carddemo.batch.statement;

import com.carddemo.batch.statement.StatementFileService.Dd;
import com.carddemo.batch.statement.StatementFileService.Result;
import com.carddemo.common.cobol.CobolValues;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import com.carddemo.common.record.layout.TrnxRecordLayout;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * {@code CBSTM03A}: one statement per card cross-reference record (XREFFILE in key order) with the
 * customer's name and address, the account's id and balance, the FICO score and the card's transactions
 * from TRNXFILE, written as 80-byte text (STMTFILE) and 100-byte HTML (HTMLFILE) lines.
 *
 * <p>The program first loads TRNXFILE into {@code WS-TRNX-TABLE} (51 cards x 10 transactions). Exceeding
 * that table is a subscript overflow in the COBOL (storage overwritten, no SSRANGE); here it stops the
 * job instead (deliberate deviation, docs/migration/validation/batch-service-golden-sets.md). The z/OS
 * TIOT walk at the start only DISPLAYs DD names and has no equivalent. A file error abends (RC 16)
 * with the program's message.
 */
final class StatementWriter {

    static final int MAX_CARDS = 51;
    static final int MAX_TRANSACTIONS = 10;

    record Output(List<String> statementLines, List<String> htmlLines, int statements, int transactions) {
    }

    private record CardTransactions(byte[] card, List<CobolRecord> transactions) {
    }

    private final List<String> stmt = new ArrayList<>();
    private final List<String> html = new ArrayList<>();

    private StatementWriter() {
    }

    static Output run(StatementFileService files) {
        return new StatementWriter().write(files);
    }

    private Output write(StatementFileService files) {
        List<CardTransactions> table = loadTransactions(files);
        int statements = 0;
        int written = 0;
        while (true) {
            Result xrefRead = files.read(Dd.XREFFILE);
            if ("10".equals(xrefRead.status())) {
                break;
            }
            check(xrefRead, "ERROR READING XREFFILE", true);
            CobolRecord xref = xrefRead.record();
            CobolRecord customer = check(files.readKey(Dd.CUSTFILE, text(xref, CardXrefRecordLayout.XREF_CUST_ID.offset(), 9)),
                    "ERROR READING CUSTFILE", false).record();
            CobolRecord account = check(files.readKey(Dd.ACCTFILE, text(xref, CardXrefRecordLayout.XREF_ACCT_ID.offset(), 11)),
                    "ERROR READING ACCTFILE", false).record();
            createStatement(customer, account);
            statements++;
            BigDecimal total = BigDecimal.ZERO.setScale(2);
            byte[] xrefCard = slice(xref, 0, 16);
            for (CardTransactions card : table) {
                int cmp = Arrays.compareUnsigned(card.card(), xrefCard);
                if (cmp > 0) {
                    break;
                }
                if (cmp == 0) {
                    for (CobolRecord t : card.transactions()) {
                        writeTransaction(t);
                        written++;
                        total = total.add(t.get(TrnxRecordLayout.TRNX_AMT)).remainder(new BigDecimal("1000000000"));
                    }
                }
            }
            stmt.add(dashes());
            stmt.add(pad("Total EXP:", 66) + "$" + zEdit(total));
            stmt.add("*".repeat(32) + "END OF STATEMENT" + "*".repeat(32));
            html.addAll(List.of(LTRS, L10, "<h3>End of Statement</h3>", LTDE, LTRE, "</table>", "</body>", "</html>"));
        }
        return new Output(List.copyOf(stmt), List.copyOf(html), statements, written);
    }

    /** 8100-TRNXFILE-OPEN / 8500-READTRNX-READ: groups consecutive records of one card. */
    private static List<CardTransactions> loadTransactions(StatementFileService files) {
        List<CardTransactions> table = new ArrayList<>();
        Result read = check(files.read(Dd.TRNXFILE), "ERROR READING TRNXFILE", false);
        while (true) {
            CobolRecord t = read.record();
            byte[] card = slice(t, 0, 16);
            if (table.isEmpty() || !Arrays.equals(table.get(table.size() - 1).card(), card)) {
                if (table.size() == MAX_CARDS) {
                    throw new IllegalStateException("WS-TRNX-TABLE holds " + MAX_CARDS + " cards; TRNXFILE has more");
                }
                table.add(new CardTransactions(card, new ArrayList<>()));
            }
            List<CobolRecord> transactions = table.get(table.size() - 1).transactions();
            if (transactions.size() == MAX_TRANSACTIONS) {
                throw new IllegalStateException("WS-TRNX-TABLE holds " + MAX_TRANSACTIONS + " transactions per card; card "
                        + t.get(TrnxRecordLayout.TRNX_CARD_NUM) + " has more");
            }
            transactions.add(t);
            read = files.read(Dd.TRNXFILE);
            if ("10".equals(read.status())) {
                return table;
            }
            check(read, "ERROR READING TRNXFILE", true);
        }
    }

    /** 5000-CREATE-STATEMENT with 5100-WRITE-HTML-HEADER and 5200-WRITE-HTML-NMADBS. */
    private void createStatement(CobolRecord c, CobolRecord a) {
        String name = pad(upTo(c.get(CustomerRecordLayout.CUST_FIRST_NAME), " ") + " "
                + upTo(c.get(CustomerRecordLayout.CUST_MIDDLE_NAME), " ") + " "
                + upTo(c.get(CustomerRecordLayout.CUST_LAST_NAME), " ") + " ", 75);
        String add1 = c.get(CustomerRecordLayout.CUST_ADDR_LINE_1);
        String add2 = c.get(CustomerRecordLayout.CUST_ADDR_LINE_2);
        String add3 = pad(upTo(c.get(CustomerRecordLayout.CUST_ADDR_LINE_3), " ") + " "
                + upTo(c.get(CustomerRecordLayout.CUST_ADDR_STATE_CD), " ") + " "
                + upTo(c.get(CustomerRecordLayout.CUST_ADDR_COUNTRY_CD), " ") + " "
                + upTo(c.get(CustomerRecordLayout.CUST_ADDR_ZIP), " ") + " ", 80);
        String acctId = pad(CobolValues.zeroPad(a.getLong(AccountRecordLayout.ACCT_ID), 11), 20);
        String balance = nineEdit(a.get(AccountRecordLayout.ACCT_CURR_BAL));
        String fico = pad(CobolValues.zeroPad(c.getLong(CustomerRecordLayout.CUST_FICO_CREDIT_SCORE), 3), 20);

        stmt.add("*".repeat(31) + "START OF STATEMENT" + "*".repeat(31));
        html.addAll(List.of("<!DOCTYPE html>", "<html lang=\"en\">", "<head>", "<meta charset=\"utf-8\">",
                "<title>HTML Table Layout</title>", "</head>", "<body style=\"margin:0px;\">",
                "<table  align=\"center\" frame=\"box\" style=\"width:70%; font:12px Segoe UI,sans-serif;\">",
                LTRS, L10, "<h3>Statement for Account Number: " + acctId + "</h3>", LTDE, LTRE,
                LTRS, "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#FFAF33;\">",
                "<p style=\"font-size:16px\">Bank of XYZ</p>", "<p>410 Terry Ave N</p>", "<p>Seattle WA 99999</p>",
                LTDE, LTRE, LTRS, L22_35));
        html.add("<p style=\"font-size:16px\">" + upTo(name.substring(0, 50), "  ") + "  </p>");
        html.add("<p>" + upTo(add1, "  ") + "  </p>");
        html.add("<p>" + upTo(add2, "  ") + "  </p>");
        html.add("<p>" + upTo(add3, "  ") + "  </p>");
        html.addAll(List.of(LTDE, LTRE, LTRS, L30_42, "<p style=\"font-size:16px\">Basic Details</p>", LTDE, LTRE,
                LTRS, L22_35));
        html.add("<p>Account ID         : " + upTo(acctId, "*") + "</p>");
        html.add("<p>Current Balance    : " + upTo(balance, "*") + "</p>");
        html.add("<p>FICO Score         : " + upTo(fico, "*") + "</p>");
        html.addAll(List.of(LTDE, LTRE, LTRS, L30_42, "<p style=\"font-size:16px\">Transaction Summary</p>", LTDE, LTRE,
                LTRS, td(25, "#33FF5E", "left"), "<p style=\"font-size:16px\">Tran ID</p>", LTDE,
                td(55, "#33FF5E", "left"), "<p style=\"font-size:16px\">Tran Details</p>", LTDE,
                td(20, "#33FF5E", "right"), "<p style=\"font-size:16px\">Amount</p>", LTDE, LTRE));

        stmt.add(name + "     ");
        stmt.add(pad(add1, 50) + " ".repeat(30));
        stmt.add(pad(add2, 50) + " ".repeat(30));
        stmt.add(add3);
        stmt.add(dashes());
        stmt.add(" ".repeat(33) + "Basic Details " + " ".repeat(33));
        stmt.add(dashes());
        stmt.add("Account ID         :" + acctId + " ".repeat(40));
        stmt.add("Current Balance    :" + balance + " ".repeat(47));
        stmt.add("FICO Score         :" + fico + " ".repeat(40));
        stmt.add(dashes());
        stmt.add(" ".repeat(30) + "TRANSACTION SUMMARY " + " ".repeat(30));
        stmt.add(dashes());
        stmt.add("Tran ID         " + pad("Tran Details    ", 51) + "  Tran Amount");
        stmt.add(dashes());
    }

    /** 6000-WRITE-TRANS. */
    private void writeTransaction(CobolRecord t) {
        String id = t.get(TrnxRecordLayout.TRNX_ID);
        String details = t.get(TrnxRecordLayout.TRNX_DESC).substring(0, 49);
        String amount = zEdit(t.get(TrnxRecordLayout.TRNX_AMT));
        stmt.add(id + " " + details + "$" + amount);
        html.addAll(List.of(LTRS, td(25, "#f2f2f2", "left"), "<p>" + upTo(id, "*") + "</p>", LTDE,
                td(55, "#f2f2f2", "left"), "<p>" + upTo(details, "*") + "</p>", LTDE,
                td(20, "#f2f2f2", "right"), "<p>" + upTo(amount, "*") + "</p>", LTDE, LTRE));
    }

    private static final String LTRS = "<tr>";
    private static final String LTRE = "</tr>";
    private static final String LTDE = "</td>";
    private static final String L10 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">";
    private static final String L22_35 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#f2f2f2;\">";
    private static final String L30_42 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#33FFD1; text-align:center;\">";

    private static String td(int width, String color, String align) {
        return "<td style=\"width:" + width + "%; padding:0px 5px; background-color:" + color + "; text-align:" + align + ";\">";
    }

    private static Result check(Result result, String message, boolean allowNull) {
        if (!result.ok()) {
            throw new IllegalStateException(message + " RETURN CODE: " + result.status());
        }
        return result;
    }

    /** {@code STRING x DELIMITED BY d}: the text before the first {@code d}, or all of it. */
    static String upTo(String value, String delimiter) {
        int at = value.indexOf(delimiter);
        return at < 0 ? value : value.substring(0, at);
    }

    /** {@code PIC Z(9).99-}: nine integer digits with leading zeros blanked, two decimals, trailing sign. */
    static String zEdit(BigDecimal value) {
        String digits = digits(value);
        String integer = digits.substring(0, 9).replaceFirst("^0+", "");
        return " ".repeat(9 - integer.length()) + integer + "." + digits.substring(9) + (value.signum() < 0 ? "-" : " ");
    }

    /** {@code PIC 9(9).99-} from {@code S9(10)V99}: the high-order digit is dropped, as the MOVE truncates. */
    static String nineEdit(BigDecimal value) {
        String digits = digits(value);
        return digits.substring(0, 9) + "." + digits.substring(9) + (value.signum() < 0 ? "-" : " ");
    }

    private static String digits(BigDecimal value) {
        long cents = value.abs().setScale(2, RoundingMode.DOWN).movePointRight(2).longValueExact() % 100_000_000_000L;
        return String.format("%011d", cents);
    }

    private static String dashes() {
        return "-".repeat(80);
    }

    private static String pad(String value, int width) {
        return value.length() >= width ? value.substring(0, width) : value + " ".repeat(width - value.length());
    }

    private static byte[] slice(CobolRecord record, int offset, int length) {
        return Arrays.copyOfRange(record.toBytes(), offset, offset + length);
    }

    private static String text(CobolRecord record, int offset, int length) {
        return record.codePage().decodeText(record.toBytes(), offset, length);
    }
}
