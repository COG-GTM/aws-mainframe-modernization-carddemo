package com.carddemo.report.domain;

import com.carddemo.recordio.layout.Transaction;

import java.math.BigDecimal;

/** The 133-byte print lines of CVTRA07Y, built as Strings and padded on output. */
public final class ReportLines {

    public static final int WIDTH = 133;

    private ReportLines() {
    }

    static String pad(String s, int width) {
        if (s.length() >= width) {
            return s.substring(0, width);
        }
        return s + " ".repeat(width - s.length());
    }

    /** REPORT-NAME-HEADER. */
    public static String nameHeader(ReportDateRange range) {
        return pad(pad("DALYREPT", 38) + pad("Daily Transaction Report", 41) + "Date Range: "
                + pad(range.startDate(), 10) + " to " + pad(range.endDate(), 10), WIDTH);
    }

    public static String blank() {
        return " ".repeat(WIDTH);
    }

    /** TRANSACTION-HEADER-1. */
    public static String columnHeader() {
        return pad(pad("Transaction ID", 17) + pad("Account ID", 12) + pad("Transaction Type", 19)
                + pad("Tran Category", 35) + pad("Tran Source", 14) + " " + pad("        Amount", 16), WIDTH);
    }

    /** TRANSACTION-HEADER-2. */
    public static String rule() {
        return "-".repeat(WIDTH);
    }

    /** TRANSACTION-DETAIL-REPORT; descriptions are truncated to X(15) and X(29) by the MOVEs. */
    public static String detail(Transaction t, String accountId, String typeDescription, String categoryDescription) {
        return pad(pad(t.id(), 16) + " " + pad(accountId, 11) + " " + pad(t.typeCode(), 2) + "-"
                + pad(typeDescription, 15) + " " + String.format("%04d", t.categoryCode()) + "-"
                + pad(categoryDescription, 29) + " " + pad(t.source(), 10) + "    "
                + CobolEditedAmount.minusEdited(t.amount()) + "  ", WIDTH);
    }

    public static String pageTotal(BigDecimal total) {
        return pad(pad("Page Total", 11) + ".".repeat(86) + CobolEditedAmount.plusEdited(total), WIDTH);
    }

    public static String accountTotal(BigDecimal total) {
        return pad(pad("Account Total", 13) + ".".repeat(84) + CobolEditedAmount.plusEdited(total), WIDTH);
    }

    public static String grandTotal(BigDecimal total) {
        return pad(pad("Grand Total", 11) + ".".repeat(86) + CobolEditedAmount.plusEdited(total), WIDTH);
    }
}
