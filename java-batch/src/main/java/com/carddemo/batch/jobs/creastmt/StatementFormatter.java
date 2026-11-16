package com.carddemo.batch.jobs.creastmt;

import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.CustomerRecord;
import com.carddemo.batch.model.StatementTransactionRecord;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * CBSTM03A paragraphs 5000-CREATE-STATEMENT, 5100-WRITE-HTML-HEADER, 5200-WRITE-HTML-NMADBS,
 * 6000-WRITE-TRANS and the trailer of 4000-TRNXFILE-GET, with the STATEMENT-LINES and
 * HTML-LINES layouts reproduced field for field.
 */
public final class StatementFormatter {

    static final int STMT_LRECL = 80;
    static final int HTML_LRECL = 100;

    private static final String ST_LINE0 = "*".repeat(31) + "START OF STATEMENT" + "*".repeat(31);
    private static final String ST_LINE5 = "-".repeat(80);
    private static final String ST_LINE6 = " ".repeat(33) + Cobol.pic("Basic Details", 14) + " ".repeat(33);
    private static final String ST_LINE11 = " ".repeat(30) + "TRANSACTION SUMMARY " + " ".repeat(30);
    private static final String ST_LINE13 = Cobol.pic("Tran ID", 16) + Cobol.pic("Tran Details", 51) + "  Tran Amount";
    private static final String ST_LINE15 = "*".repeat(32) + "END OF STATEMENT" + "*".repeat(32);

    private static final String TRS = "<tr>";
    private static final String TRE = "</tr>";
    private static final String TDE = "</td>";
    private static final String L10 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">";
    private static final String L15 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#FFAF33;\">";
    private static final String L22_35 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#f2f2f2;\">";
    private static final String L30_42 =
            "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#33FFD1; text-align:center;\">";
    private static final String P16 = "<p style=\"font-size:16px\">";
    private static final List<String> HTML_HEAD = List.of(
            "<!DOCTYPE html>",
            "<html lang=\"en\">",
            "<head>",
            "<meta charset=\"utf-8\">",
            "<title>HTML Table Layout</title>",
            "</head>",
            "<body style=\"margin:0px;\">",
            "<table  align=\"center\" frame=\"box\" style=\"width:70%; font:12px Segoe UI,sans-serif;\">",
            TRS,
            L10);

    private StatementFormatter() {
    }

    public static Statement format(CustomerRecord cust, AccountRecord acct, List<StatementTransactionRecord> trans) {
        List<String> stmt = new ArrayList<>();
        List<String> html = new ArrayList<>();

        String stName = Cobol.pic(Cobol.delimited(cust.custFirstName(), " ") + " "
                + Cobol.delimited(cust.custMiddleName(), " ") + " "
                + Cobol.delimited(cust.custLastName(), " ") + " ", 75);
        String stAdd1 = Cobol.pic(cust.custAddrLine1(), 50);
        String stAdd2 = Cobol.pic(cust.custAddrLine2(), 50);
        String stAdd3 = Cobol.pic(Cobol.delimited(cust.custAddrLine3(), " ") + " "
                + Cobol.delimited(cust.custAddrStateCd(), " ") + " "
                + Cobol.delimited(cust.custAddrCountryCd(), " ") + " "
                + Cobol.delimited(cust.custAddrZip(), " ") + " ", 80);
        String stAcctId = Cobol.pic(Cobol.digits(acct.acctId(), 11), 20);
        String stCurrBal = Cobol.edited(acct.acctCurrBal(), 9, false);
        String stFicoScore = Cobol.pic(Cobol.digits(cust.custFicoCreditScore(), 3), 20);

        // 5000-CREATE-STATEMENT
        stmt.add(ST_LINE0);
        stmt.add(stName + " ".repeat(5));
        stmt.add(stAdd1 + " ".repeat(30));
        stmt.add(stAdd2 + " ".repeat(30));
        stmt.add(stAdd3);
        stmt.add(ST_LINE5);
        stmt.add(ST_LINE6);
        stmt.add(ST_LINE5);
        stmt.add("Account ID         :" + stAcctId + " ".repeat(40));
        stmt.add("Current Balance    :" + stCurrBal + " ".repeat(47));
        stmt.add("FICO Score         :" + stFicoScore + " ".repeat(40));
        stmt.add(ST_LINE5);
        stmt.add(ST_LINE11);
        stmt.add(ST_LINE5);
        stmt.add(ST_LINE13);
        stmt.add(ST_LINE5);

        // 5100-WRITE-HTML-HEADER
        html.addAll(HTML_HEAD);
        html.add("<h3>Statement for Account Number: " + stAcctId + "</h3>");
        html.addAll(List.of(TDE, TRE, TRS, L15,
                P16 + "Bank of XYZ</p>", "<p>410 Terry Ave N</p>", "<p>Seattle WA 99999</p>",
                TDE, TRE, TRS, L22_35));

        // 5200-WRITE-HTML-NMADBS
        html.add(P16 + Cobol.delimited(Cobol.pic(stName, 50), "  ") + "  </p>");
        html.add("<p>" + Cobol.delimited(stAdd1, "  ") + "  </p>");
        html.add("<p>" + Cobol.delimited(stAdd2, "  ") + "  </p>");
        html.add("<p>" + Cobol.delimited(stAdd3, "  ") + "  </p>");
        html.addAll(List.of(TDE, TRE, TRS, L30_42, P16 + "Basic Details</p>", TDE, TRE, TRS, L22_35));
        html.add("<p>Account ID         : " + stAcctId + "</p>");
        html.add("<p>Current Balance    : " + stCurrBal + "</p>");
        html.add("<p>FICO Score         : " + stFicoScore + "</p>");
        html.addAll(List.of(TDE, TRE, TRS, L30_42, P16 + "Transaction Summary</p>", TDE, TRE, TRS,
                headerCell(25, "left"), P16 + "Tran ID</p>", TDE,
                headerCell(55, "left"), P16 + "Tran Details</p>", TDE,
                headerCell(20, "right"), P16 + "Amount</p>", TDE, TRE));

        // 4000-TRNXFILE-GET / 6000-WRITE-TRANS
        BigDecimal total = BigDecimal.ZERO.setScale(2);
        for (StatementTransactionRecord t : trans) {
            String stTranId = Cobol.pic(t.trnxKey().trnxId(), 16);
            String stTranDt = Cobol.pic(t.trnxDesc(), 49);
            String stTranAmt = Cobol.edited(t.trnxAmt(), 9, true);
            stmt.add(stTranId + " " + stTranDt + "$" + stTranAmt);
            html.addAll(List.of(TRS,
                    detailCell(25, "left"), "<p>" + stTranId + "</p>", TDE,
                    detailCell(55, "left"), "<p>" + stTranDt + "</p>", TDE,
                    detailCell(20, "right"), "<p>" + stTranAmt + "</p>", TDE,
                    TRE));
            total = total.add(t.trnxAmt());
        }

        stmt.add(ST_LINE5);
        stmt.add("Total EXP:" + " ".repeat(56) + "$" + Cobol.edited(total, 9, true));
        stmt.add(ST_LINE15);
        html.addAll(List.of(TRS, L10, "<h3>End of Statement</h3>", TDE, TRE, "</table>", "</body>", "</html>"));

        return new Statement(
                stmt.stream().map(s -> Cobol.pic(s, STMT_LRECL)).toList(),
                html.stream().map(s -> Cobol.pic(s, HTML_LRECL)).toList());
    }

    private static String headerCell(int width, String align) {
        return cell(width, "#33FF5E", align);
    }

    private static String detailCell(int width, String align) {
        return cell(width, "#f2f2f2", align);
    }

    private static String cell(int width, String color, String align) {
        return "<td style=\"width:" + width + "%; padding:0px 5px; background-color:" + color
                + "; text-align:" + align + ";\">";
    }
}
