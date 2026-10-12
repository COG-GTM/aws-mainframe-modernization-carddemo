package com.carddemo.report.domain;

import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionCategory;
import com.carddemo.recordio.layout.TransactionType;
import com.carddemo.report.ReportProperties.OutOfRangePolicy;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * CBTRN03C main loop and 1100-1120 paragraphs (lines 159-375) as a page/card control-break writer.
 *
 * <p>Behaviours reproduced exactly, all flagged in open-questions.md:
 * <ul>
 *   <li>Page size 20 counts every line written, headers and totals included
 *       ({@code FUNCTION MOD(WS-LINE-COUNTER, 20) = 0} is tested before each detail line).</li>
 *   <li>A card change writes an Account Total + rule before the new card's first detail; the
 *       <em>last</em> card's account total is never written.</li>
 *   <li>At end of file the amount of the last record read is added to the page and account totals a
 *       second time before Page Total and Grand Total are printed (lines 203-208).</li>
 *   <li>An out-of-range record ends the run (NEXT SENTENCE) — configurable via {@link OutOfRangePolicy}.</li>
 * </ul>
 */
public final class TransactionReportWriter {

    static final int PAGE_SIZE = 20;

    private final ReportDateRange range;
    private final ReportLookups lookups;
    private final OutOfRangePolicy outOfRangePolicy;
    private final List<String> lines = new ArrayList<>();

    private boolean firstTime = true;
    private long lineCounter;
    private BigDecimal pageTotal = BigDecimal.ZERO;
    private BigDecimal accountTotal = BigDecimal.ZERO;
    private BigDecimal grandTotal = BigDecimal.ZERO;
    private String currentCard = " ".repeat(16);
    private String currentAccountId = "";
    private Transaction lastRead;
    private boolean stopped;

    public TransactionReportWriter(ReportDateRange range, ReportLookups lookups, OutOfRangePolicy outOfRangePolicy) {
        this.range = range;
        this.lookups = lookups;
        this.outOfRangePolicy = outOfRangePolicy;
    }

    public List<String> write(List<Transaction> sortedTransactions) {
        for (Transaction t : sortedTransactions) {
            if (stopped) {
                break;
            }
            lastRead = t;
            if (!range.includes(t.processingDate())) {
                if (outOfRangePolicy == OutOfRangePolicy.STOP_LIKE_COBOL) {
                    stopped = true;
                    break;
                }
                continue;
            }
            report(t);
        }
        if (!stopped) {
            endOfFile();
        }
        return List.copyOf(lines);
    }

    private void report(Transaction t) {
        if (!currentCard.equals(t.cardNumber())) {
            if (!firstTime) {
                writeAccountTotals();
            }
            currentCard = t.cardNumber();
            CardXref xref = lookups.card(t.cardNumber());
            currentAccountId = xref.accountId();
        }
        TransactionType type = lookups.type(t.typeCode());
        TransactionCategory category = lookups.category(t.typeCode(), t.categoryCode());

        if (firstTime) {
            firstTime = false;
            writeHeaders();
        }
        if (lineCounter % PAGE_SIZE == 0) {
            writePageTotals();
            writeHeaders();
        }
        pageTotal = pageTotal.add(t.amount());
        accountTotal = accountTotal.add(t.amount());
        emit(ReportLines.detail(t, currentAccountId, type.description(), category.description()));
    }

    private void endOfFile() {
        if (lastRead == null) {
            // Empty input: the COBOL would ADD an uninitialised TRAN-AMT here (open question);
            // nothing sensible can be reproduced, so print the totals as zero.
            writePageTotals();
            emit(ReportLines.grandTotal(grandTotal));
            return;
        }
        // The amount of the last record read is counted again (lines 205-206).
        pageTotal = pageTotal.add(lastRead.amount());
        accountTotal = accountTotal.add(lastRead.amount());
        writePageTotals();
        emit(ReportLines.grandTotal(grandTotal));
    }

    private void writeHeaders() {
        emit(ReportLines.nameHeader(range));
        emit(ReportLines.blank());
        emit(ReportLines.columnHeader());
        emit(ReportLines.rule());
    }

    private void writePageTotals() {
        emit(ReportLines.pageTotal(pageTotal));
        grandTotal = grandTotal.add(pageTotal);
        pageTotal = BigDecimal.ZERO;
        emit(ReportLines.rule());
    }

    private void writeAccountTotals() {
        emit(ReportLines.accountTotal(accountTotal));
        accountTotal = BigDecimal.ZERO;
        emit(ReportLines.rule());
    }

    private void emit(String line) {
        lines.add(line);
        lineCounter++;
    }

    public boolean stoppedOnOutOfRangeRecord() {
        return stopped;
    }
}
