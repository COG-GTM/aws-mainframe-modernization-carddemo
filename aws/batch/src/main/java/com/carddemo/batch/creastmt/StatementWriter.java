package com.carddemo.batch.creastmt;

import com.carddemo.batch.record.Cobol;
import com.carddemo.batch.record.Edited;
import com.carddemo.batch.record.Fixed;
import com.carddemo.batch.record.Zoned;
import java.math.BigDecimal;
import java.util.List;

/**
 * {@code CBSTM03A} statement layout: 80-byte text records ({@code STATEMENT-LINES}) and 100-byte HTML records
 * ({@code HTML-LINES}), including the COBOL {@code STRING … DELIMITED BY ' '} / {@code '  '} truncation rules.
 */
public final class StatementWriter {

    static final int TEXT_WIDTH = 80;
    static final int HTML_WIDTH = 100;

    private static final String L08 =
            "<table  align=\"center\" frame=\"box\" style=\"width:70%; font:12px Segoe UI,sans-serif;\">";
    private static final String L10 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">";
    private static final String L15 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#FFAF33;\">";
    private static final String L22_35 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#f2f2f2;\">";
    private static final String L30_42 =
            "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#33FFD1; text-align:center;\">";
    private static final String L47 =
            "<td style=\"width:25%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">";
    private static final String L50 =
            "<td style=\"width:55%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">";
    private static final String L53 =
            "<td style=\"width:20%; padding:0px 5px; background-color:#33FF5E; text-align:right;\">";
    private static final String L58 =
            "<td style=\"width:25%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">";
    private static final String L61 =
            "<td style=\"width:55%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">";
    private static final String L64 =
            "<td style=\"width:20%; padding:0px 5px; background-color:#f2f2f2; text-align:right;\">";
    private static final String TRS = "<tr>";
    private static final String TRE = "</tr>";
    private static final String TDE = "</td>";

    public record Customer(String firstName, String middleName, String lastName, String addrLine1, String addrLine2,
            String addrLine3, String stateCd, String countryCd, String zip, Integer ficoScore) {
    }

    public record Line(String tranId, String description, BigDecimal amt) {
    }

    private final StringBuilder text = new StringBuilder();
    private final StringBuilder html = new StringBuilder();

    public String text() {
        return text.toString();
    }

    public String html() {
        return html.toString();
    }

    /** {@code 5000-CREATE-STATEMENT} + {@code 4000-TRNXFILE-GET} for one card cross-reference. */
    public void statement(long acctId, BigDecimal currBal, Customer c, List<Line> lines) {
        String stName = pad(word(c.firstName()) + " " + word(c.middleName()) + " " + word(c.lastName()) + " ", 75);
        String stAdd1 = pad(c.addrLine1(), 50);
        String stAdd2 = pad(c.addrLine2(), 50);
        String stAdd3 = pad(word(c.addrLine3()) + " " + word(c.stateCd()) + " " + word(c.countryCd()) + " "
                + word(c.zip()) + " ", 80);
        String stAcctId = pad(Zoned.formatUnsigned(acctId, 11), 20);
        String stCurrBal = Edited.format(currBal, "999999999.99-");
        String stFico = pad(c.ficoScore() == null ? "" : Zoned.formatUnsigned(c.ficoScore(), 3), 20);

        txt("*".repeat(31) + "START OF STATEMENT" + "*".repeat(31));
        htmlHeader(stAcctId);
        htmlNameAddressBasics(stName, stAdd1, stAdd2, stAdd3, stAcctId, stCurrBal, stFico);
        txt(stName);
        txt(stAdd1);
        txt(stAdd2);
        txt(stAdd3);
        txt("-".repeat(80));
        txt(" ".repeat(33) + "Basic Details");
        txt("-".repeat(80));
        txt("Account ID         :" + stAcctId);
        txt("Current Balance    :" + stCurrBal);
        txt("FICO Score         :" + stFico);
        txt("-".repeat(80));
        txt(" ".repeat(30) + "TRANSACTION SUMMARY ");
        txt("-".repeat(80));
        txt("Tran ID         " + "Tran Details    " + " ".repeat(35) + "  Tran Amount");
        txt("-".repeat(80));

        BigDecimal total = BigDecimal.ZERO;
        for (Line l : lines) {
            writeTransaction(l);
            total = Cobol.fit(total.add(l.amt()), 9, 2);
        }
        txt("-".repeat(80));
        txt("Total EXP:" + " ".repeat(56) + "$" + Edited.format(total, "ZZZZZZZZZ.99-"));
        txt("*".repeat(32) + "END OF STATEMENT" + "*".repeat(32));
        htm(TRS);
        htm(L10);
        htm("<h3>End of Statement</h3>");
        htm(TDE);
        htm(TRE);
        htm("</table>");
        htm("</body>");
        htm("</html>");
    }

    /** {@code 6000-WRITE-TRANS}. */
    private void writeTransaction(Line l) {
        String stTranId = pad(l.tranId(), 16);
        String stTranDt = pad(l.description(), 49);
        String stTranAmt = Edited.format(l.amt(), "ZZZZZZZZZ.99-");
        txt(stTranId + " " + stTranDt + "$" + stTranAmt);
        htm(TRS);
        htm(L58);
        htm("<p>" + stTranId + "</p>");
        htm(TDE);
        htm(L61);
        htm("<p>" + stTranDt + "</p>");
        htm(TDE);
        htm(L64);
        htm("<p>" + stTranAmt + "</p>");
        htm(TDE);
        htm(TRE);
    }

    /** {@code 5100-WRITE-HTML-HEADER}. */
    private void htmlHeader(String stAcctId) {
        htm("<!DOCTYPE html>");
        htm("<html lang=\"en\">");
        htm("<head>");
        htm("<meta charset=\"utf-8\">");
        htm("<title>HTML Table Layout</title>");
        htm("</head>");
        htm("<body style=\"margin:0px;\">");
        htm(L08);
        htm(TRS);
        htm(L10);
        htm("<h3>Statement for Account Number: " + stAcctId + "</h3>");
        htm(TDE);
        htm(TRE);
        htm(TRS);
        htm(L15);
        htm("<p style=\"font-size:16px\">Bank of XYZ</p>");
        htm("<p>410 Terry Ave N</p>");
        htm("<p>Seattle WA 99999</p>");
        htm(TDE);
        htm(TRE);
        htm(TRS);
        htm(L22_35);
    }

    /** {@code 5200-WRITE-HTML-NMADBS}. */
    private void htmlNameAddressBasics(String stName, String stAdd1, String stAdd2, String stAdd3, String stAcctId,
            String stCurrBal, String stFico) {
        htm("<p style=\"font-size:16px\">" + upToDoubleSpace(stName.substring(0, 50)) + "  </p>");
        htm("<p>" + upToDoubleSpace(stAdd1) + "  </p>");
        htm("<p>" + upToDoubleSpace(stAdd2) + "  </p>");
        htm("<p>" + upToDoubleSpace(stAdd3) + "  </p>");
        htm(TDE);
        htm(TRE);
        htm(TRS);
        htm(L30_42);
        htm("<p style=\"font-size:16px\">Basic Details</p>");
        htm(TDE);
        htm(TRE);
        htm(TRS);
        htm(L22_35);
        htm("<p>Account ID         : " + stAcctId + "</p>");
        htm("<p>Current Balance    : " + stCurrBal + "</p>");
        htm("<p>FICO Score         : " + stFico + "</p>");
        htm(TDE);
        htm(TRE);
        htm(TRS);
        htm(L30_42);
        htm("<p style=\"font-size:16px\">Transaction Summary</p>");
        htm(TDE);
        htm(TRE);
        htm(TRS);
        htm(L47);
        htm("<p style=\"font-size:16px\">Tran ID</p>");
        htm(TDE);
        htm(L50);
        htm("<p style=\"font-size:16px\">Tran Details</p>");
        htm(TDE);
        htm(L53);
        htm("<p style=\"font-size:16px\">Amount</p>");
        htm(TDE);
        htm(TRE);
    }

    private void txt(String s) {
        text.append(pad(s, TEXT_WIDTH)).append('\n');
    }

    private void htm(String s) {
        html.append(pad(s, HTML_WIDTH)).append('\n');
    }

    private static String pad(String s, int width) {
        return Fixed.pad(s == null ? "" : s, width);
    }

    /** {@code STRING x DELIMITED BY ' '}: the characters before the first space. */
    static String word(String s) {
        if (s == null) {
            return "";
        }
        int i = s.indexOf(' ');
        return i < 0 ? s : s.substring(0, i);
    }

    /** {@code STRING x DELIMITED BY '  '}: the characters before the first double space. */
    static String upToDoubleSpace(String s) {
        int i = s.indexOf("  ");
        return i < 0 ? s : s.substring(0, i);
    }
}
