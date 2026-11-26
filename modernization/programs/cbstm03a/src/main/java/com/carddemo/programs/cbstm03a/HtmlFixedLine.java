package com.carddemo.programs.cbstm03a;

import com.carddemo.programs.cbstm03a.support.Pic;

/** HTML-FIXED-LN PIC X(100) and its 88-level values (SET HTML-xxx TO TRUE). */
public enum HtmlFixedLine {
    HTML_L01("<!DOCTYPE html>"),
    HTML_L02("<html lang=\"en\">"),
    HTML_L03("<head>"),
    HTML_L04("<meta charset=\"utf-8\">"),
    HTML_L05("<title>HTML Table Layout</title>"),
    HTML_L06("</head>"),
    HTML_L07("<body style=\"margin:0px;\">"),
    HTML_L08("<table  align=\"center\" frame=\"box\" style=\"width:70%; font:12px Segoe UI,sans-serif;\">"),
    HTML_LTRS("<tr>"),
    HTML_LTRE("</tr>"),
    HTML_LTDS("<td>"),
    HTML_LTDE("</td>"),
    HTML_L10("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">"),
    HTML_L15("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#FFAF33;\">"),
    HTML_L16("<p style=\"font-size:16px\">Bank of XYZ</p>"),
    HTML_L17("<p>410 Terry Ave N</p>"),
    HTML_L18("<p>Seattle WA 99999</p>"),
    HTML_L22_35("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#f2f2f2;\">"),
    HTML_L30_42("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#33FFD1; text-align:center;\">"),
    HTML_L31("<p style=\"font-size:16px\">Basic Details</p>"),
    HTML_L43("<p style=\"font-size:16px\">Transaction Summary</p>"),
    HTML_L47("<td style=\"width:25%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">"),
    HTML_L48("<p style=\"font-size:16px\">Tran ID</p>"),
    HTML_L50("<td style=\"width:55%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">"),
    HTML_L51("<p style=\"font-size:16px\">Tran Details</p>"),
    HTML_L53("<td style=\"width:20%; padding:0px 5px; background-color:#33FF5E; text-align:right;\">"),
    HTML_L54("<p style=\"font-size:16px\">Amount</p>"),
    HTML_L58("<td style=\"width:25%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">"),
    HTML_L61("<td style=\"width:55%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">"),
    HTML_L64("<td style=\"width:20%; padding:0px 5px; background-color:#f2f2f2; text-align:right;\">"),
    HTML_L75("<h3>End of Statement</h3>"),
    HTML_L78("</table>"),
    HTML_L79("</body>"),
    HTML_L80("</html>");

    public static final int LENGTH = 100;

    private final String value;

    HtmlFixedLine(String value) {
        this.value = value;
    }

    /** The 100-byte HTML-FIXED-LN content after SET ... TO TRUE. */
    public String line() {
        return Pic.x(value, LENGTH);
    }

    public String value() {
        return value;
    }
}
