package com.carddemo.programs.cbtrn03c.copybook;

import com.carddemo.programs.cbtrn03c.support.Fixed;
import com.carddemo.programs.cbtrn03c.support.PicEditor;

import java.math.BigDecimal;

/** CVTRA07Y - report line layouts for the daily transaction report (DALYREPT). */
public final class ReportLayouts {

    public static final String DETAIL_AMOUNT_PICTURE = "-ZZZ,ZZZ,ZZZ.ZZ";
    public static final String TOTAL_AMOUNT_PICTURE = "+ZZZ,ZZZ,ZZZ.ZZ";

    /** 01 TRANSACTION-HEADER-1 (114 bytes). */
    public static final String TRANSACTION_HEADER_1 =
            Fixed.pic("Transaction ID", 17)
                    + Fixed.pic("Account ID", 12)
                    + Fixed.pic("Transaction Type", 19)
                    + Fixed.pic("Tran Category", 35)
                    + Fixed.pic("Tran Source", 14)
                    + Fixed.spaces(1)
                    + Fixed.pic("        Amount", 16);

    /** 01 TRANSACTION-HEADER-2 PIC X(133) VALUE ALL '-'. */
    public static final String TRANSACTION_HEADER_2 = "-".repeat(133);

    private ReportLayouts() {
    }

    /**
     * 01 REPORT-NAME-HEADER (115 bytes): REPT-SHORT-NAME X(38) 'DALYREPT', REPT-LONG-NAME X(41),
     * REPT-DATE-HEADER X(12), REPT-START-DATE X(10), FILLER X(04) ' to ', REPT-END-DATE X(10).
     */
    public record ReportNameHeader(String reptStartDate, String reptEndDate) {
        public ReportNameHeader {
            reptStartDate = Fixed.pic(reptStartDate, 10);
            reptEndDate = Fixed.pic(reptEndDate, 10);
        }

        public static ReportNameHeader initial() {
            return new ReportNameHeader("", "");
        }

        public String format() {
            return Fixed.pic("DALYREPT", 38)
                    + Fixed.pic("Daily Transaction Report", 41)
                    + Fixed.pic("Date Range: ", 12)
                    + reptStartDate
                    + " to "
                    + reptEndDate;
        }
    }

    /**
     * 01 TRANSACTION-DETAIL-REPORT (114 bytes). FILLERs keep their VALUE clauses (' ' / '-').
     *
     * <pre>
     * TRAN-REPORT-TRANS-ID X(16), ' ', TRAN-REPORT-ACCOUNT-ID X(11), ' ', TRAN-REPORT-TYPE-CD X(02), '-',
     * TRAN-REPORT-TYPE-DESC X(15), ' ', TRAN-REPORT-CAT-CD 9(04), '-', TRAN-REPORT-CAT-DESC X(29), ' ',
     * TRAN-REPORT-SOURCE X(10), 4 spaces, TRAN-REPORT-AMT -ZZZ,ZZZ,ZZZ.ZZ, 2 spaces
     * </pre>
     */
    public record TransactionDetailReport(
            String tranReportTransId,
            String tranReportAccountId,
            String tranReportTypeCd,
            String tranReportTypeDesc,
            int tranReportCatCd,
            String tranReportCatDesc,
            String tranReportSource,
            BigDecimal tranReportAmt) {

        public TransactionDetailReport {
            tranReportTransId = Fixed.pic(tranReportTransId, 16);
            tranReportAccountId = Fixed.pic(tranReportAccountId, 11);
            tranReportTypeCd = Fixed.pic(tranReportTypeCd, 2);
            tranReportTypeDesc = Fixed.pic(tranReportTypeDesc, 15);
            tranReportCatDesc = Fixed.pic(tranReportCatDesc, 29);
            tranReportSource = Fixed.pic(tranReportSource, 10);
            tranReportAmt = tranReportAmt == null ? BigDecimal.ZERO : tranReportAmt;
        }

        /** INITIALIZE TRANSACTION-DETAIL-REPORT: named alphanumerics to spaces, numerics to zero. */
        public static TransactionDetailReport initialized() {
            return new TransactionDetailReport("", "", "", "", 0, "", "", BigDecimal.ZERO);
        }

        public String format() {
            return tranReportTransId
                    + " "
                    + tranReportAccountId
                    + " "
                    + tranReportTypeCd
                    + "-"
                    + tranReportTypeDesc
                    + " "
                    + Fixed.unsigned(tranReportCatCd, 4)
                    + "-"
                    + tranReportCatDesc
                    + " "
                    + tranReportSource
                    + Fixed.spaces(4)
                    + PicEditor.edit(tranReportAmt, DETAIL_AMOUNT_PICTURE)
                    + Fixed.spaces(2);
        }
    }

    /** 01 REPORT-PAGE-TOTALS: 'Page Total' X(11), ALL '.' X(86), REPT-PAGE-TOTAL +ZZZ,ZZZ,ZZZ.ZZ. */
    public static String reportPageTotals(BigDecimal reptPageTotal) {
        return Fixed.pic("Page Total", 11) + ".".repeat(86) + PicEditor.edit(reptPageTotal, TOTAL_AMOUNT_PICTURE);
    }

    /** 01 REPORT-ACCOUNT-TOTALS: 'Account Total' X(13), ALL '.' X(84), REPT-ACCOUNT-TOTAL +ZZZ,ZZZ,ZZZ.ZZ. */
    public static String reportAccountTotals(BigDecimal reptAccountTotal) {
        return Fixed.pic("Account Total", 13) + ".".repeat(84) + PicEditor.edit(reptAccountTotal, TOTAL_AMOUNT_PICTURE);
    }

    /** 01 REPORT-GRAND-TOTALS: 'Grand Total' X(11), ALL '.' X(86), REPT-GRAND-TOTAL +ZZZ,ZZZ,ZZZ.ZZ. */
    public static String reportGrandTotals(BigDecimal reptGrandTotal) {
        return Fixed.pic("Grand Total", 11) + ".".repeat(86) + PicEditor.edit(reptGrandTotal, TOTAL_AMOUNT_PICTURE);
    }
}
