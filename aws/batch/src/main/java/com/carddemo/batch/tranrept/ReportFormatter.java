package com.carddemo.batch.tranrept;

import com.carddemo.batch.record.Cobol;
import com.carddemo.batch.record.Edited;
import com.carddemo.batch.record.Fixed;
import com.carddemo.batch.record.Zoned;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * {@code CBTRN03C} report layout ({@code REPORT-NAME-HEADER}, {@code TRANSACTION-HEADER-1/2},
 * {@code TRANSACTION-DETAIL-REPORT}, {@code REPORT-PAGE-TOTALS}, {@code REPORT-ACCOUNT-TOTALS},
 * {@code REPORT-GRAND-TOTALS}) and paging ({@code WS-PAGE-SIZE = 20}, {@code WS-LINE-COUNTER} incremented for
 * every line except the grand total).
 *
 * <p>Deviation from the COBOL end-of-file branch: it re-adds the last record's {@code TRAN-AMT} to the page
 * total and never writes the last account's total. Here the last account total is written and nothing is
 * double counted (see README).
 */
public final class ReportFormatter {

    public static final int WIDTH = 133;
    public static final int PAGE_SIZE = 20;
    private static final String SEPARATOR = "-".repeat(WIDTH);
    private static final String AMT_PIC = "-ZZZ,ZZZ,ZZZ.ZZ";
    private static final String TOTAL_PIC = "+ZZZ,ZZZ,ZZZ.ZZ";

    private ReportFormatter() {
    }

    public record Detail(String tranId, String cardNum, String acctId, String typeCd, String typeDesc, int catCd,
            String catDesc, String source, BigDecimal amt) {
    }

    public record Report(String text, int lines, BigDecimal grandTotal) {
    }

    public static Report format(LocalDate start, LocalDate end, List<Detail> details) {
        State s = new State(start, end);
        String currentCard = null;
        boolean first = true;
        for (Detail d : details) {
            if (!d.cardNum().equals(currentCard)) {
                if (!first) {
                    s.accountTotals();
                }
                currentCard = d.cardNum();
            }
            if (first) {
                first = false;
                s.headers();
            }
            if (s.lineCounter % PAGE_SIZE == 0) {
                s.pageTotals();
                s.headers();
            }
            s.pageTotal = Cobol.fit(s.pageTotal.add(d.amt()), 9, 2);
            s.accountTotal = Cobol.fit(s.accountTotal.add(d.amt()), 9, 2);
            s.detail(d);
        }
        if (!first) {
            s.accountTotals();
        }
        s.pageTotals();
        s.write(Fixed.pad("Grand Total", 11) + ".".repeat(86) + Edited.format(s.grandTotal, TOTAL_PIC));
        return new Report(s.out.toString(), s.written, s.grandTotal);
    }

    private static final class State {
        final StringBuilder out = new StringBuilder();
        final String nameHeader;
        int lineCounter;
        int written;
        BigDecimal pageTotal = BigDecimal.ZERO;
        BigDecimal accountTotal = BigDecimal.ZERO;
        BigDecimal grandTotal = BigDecimal.ZERO;

        State(LocalDate start, LocalDate end) {
            nameHeader = Fixed.pad("DALYREPT", 38) + Fixed.pad("Daily Transaction Report", 41) + "Date Range: "
                    + start + " to " + end;
        }

        void write(String line) {
            out.append(Fixed.pad(line, WIDTH)).append('\n');
            written++;
        }

        void counted(String line) {
            write(line);
            lineCounter++;
        }

        /** {@code 1120-WRITE-HEADERS}. */
        void headers() {
            counted(nameHeader);
            counted("");
            counted(Fixed.pad("Transaction ID", 17) + Fixed.pad("Account ID", 12) + Fixed.pad("Transaction Type", 19)
                    + Fixed.pad("Tran Category", 35) + Fixed.pad("Tran Source", 14) + " " + "        Amount");
            counted(SEPARATOR);
        }

        /** {@code 1110-WRITE-PAGE-TOTALS}. */
        void pageTotals() {
            counted(Fixed.pad("Page Total", 11) + ".".repeat(86) + Edited.format(pageTotal, TOTAL_PIC));
            grandTotal = grandTotal.add(pageTotal);
            pageTotal = BigDecimal.ZERO;
            counted(SEPARATOR);
        }

        /** {@code 1120-WRITE-ACCOUNT-TOTALS}. */
        void accountTotals() {
            counted(Fixed.pad("Account Total", 13) + ".".repeat(84) + Edited.format(accountTotal, TOTAL_PIC));
            accountTotal = BigDecimal.ZERO;
            counted(SEPARATOR);
        }

        /** {@code 1120-WRITE-DETAIL}. */
        void detail(Detail d) {
            counted(Fixed.pad(d.tranId(), 16) + " " + Fixed.pad(d.acctId(), 11) + " " + Fixed.pad(d.typeCd(), 2)
                    + "-" + Fixed.pad(d.typeDesc(), 15) + " " + Zoned.formatUnsigned(d.catCd(), 4) + "-"
                    + Fixed.pad(d.catDesc(), 29) + " " + Fixed.pad(d.source(), 10) + "    "
                    + Edited.format(d.amt(), AMT_PIC) + "  ");
        }
    }
}
