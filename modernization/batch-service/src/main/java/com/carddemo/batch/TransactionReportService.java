package com.carddemo.batch;

import com.carddemo.common.domain.CardTransaction;
import com.carddemo.common.domain.CardTransactionRepository;
import com.carddemo.common.domain.CardXref;
import com.carddemo.common.domain.CardXrefRepository;
import com.carddemo.common.domain.ReportRequest;
import com.carddemo.common.domain.ReportRequestRepository;
import com.carddemo.common.domain.TransactionCategory;
import com.carddemo.common.domain.TransactionCategoryId;
import com.carddemo.common.domain.TransactionCategoryRepository;
import com.carddemo.common.domain.TransactionType;
import com.carddemo.common.domain.TransactionTypeRepository;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * TRANREPT / CBTRN03C: transactions whose processed date falls in [start, end], in card order, with page totals
 * every 20 detail lines, an account total on each card break and a grand total.
 */
@Service
public class TransactionReportService {

    static final int PAGE_SIZE = 20;

    private final CardTransactionRepository transactions;
    private final CardXrefRepository xrefs;
    private final TransactionTypeRepository types;
    private final TransactionCategoryRepository categories;
    private final ReportRequestRepository requests;
    private final Clock clock;

    public TransactionReportService(CardTransactionRepository transactions, CardXrefRepository xrefs,
                                    TransactionTypeRepository types, TransactionCategoryRepository categories,
                                    ReportRequestRepository requests, Optional<Clock> clock) {
        this.transactions = transactions;
        this.xrefs = xrefs;
        this.types = types;
        this.categories = categories;
        this.requests = requests;
        this.clock = clock.orElse(Clock.systemDefaultZone());
    }

    @Transactional(readOnly = true)
    public JobResult run(LocalDate start, LocalDate end) {
        List<CardTransaction> rows = transactions.findByProcTsGreaterThanEqualAndProcTsLessThanOrderByCardNumAscTranIdAsc(
                start.atStartOfDay(), end.plusDays(1).atStartOfDay());
        StringBuilder out = new StringBuilder();
        header(out, start, end);
        BigDecimal page = BigDecimal.ZERO;
        BigDecimal account = BigDecimal.ZERO;
        BigDecimal grand = BigDecimal.ZERO;
        String currentCard = null;
        int lines = 0;
        for (CardTransaction t : rows) {
            if (currentCard != null && !currentCard.equals(t.getCardNum())) {
                total(out, "Account Total", account);
                account = BigDecimal.ZERO;
            }
            if (lines > 0 && lines % PAGE_SIZE == 0) {
                total(out, "Page Total", page);
                page = BigDecimal.ZERO;
                header(out, start, end);
            }
            currentCard = t.getCardNum();
            Long acctId = xrefs.findById(t.getCardNum()).map(CardXref::getAcctId).orElse(null);
            String type = types.findById(t.getTypeCd()).map(TransactionType::getDescription).orElse("");
            String cat = categories.findById(new TransactionCategoryId(t.getTypeCd(), t.getCatCd()))
                    .map(TransactionCategory::getDescription).orElse("");
            out.append(String.format("%-16s %011d %2s-%-15.15s %04d-%-29.29s %-10.10s %,16.2f%n", t.getTranId(),
                    acctId == null ? 0 : acctId, t.getTypeCd(), type, t.getCatCd(), cat, t.getSource(),
                    t.getAmount()));
            page = page.add(t.getAmount());
            account = account.add(t.getAmount());
            grand = grand.add(t.getAmount());
            lines++;
        }
        if (currentCard != null) {
            total(out, "Page Total", page);
            total(out, "Account Total", account);
        }
        total(out, "Grand Total", grand);
        return JobResult.of("TRANREPT", 0, out.toString(), "startDate", start.toString(), "endDate", end.toString(),
                "transactions", rows.size(), "grandTotal", grand);
    }

    /** Picks up CORPT00C requests queued by transaction-service (replaces the TDQ/internal-reader JCL submit). */
    @Transactional
    public int processSubmittedRequests() {
        int done = 0;
        for (ReportRequest r : requests.findByStatusOrderByIdAsc(ReportRequest.SUBMITTED)) {
            r.complete(run(r.getStartDate(), r.getEndDate()).output(), LocalDateTime.now(clock));
            done++;
        }
        return done;
    }

    private static void header(StringBuilder out, LocalDate start, LocalDate end) {
        out.append(String.format("%-40s Date Range: %s to %s%n", "DALYREPT  CardDemo Transaction Detail Report",
                start, end));
        out.append(String.format("%-16s %-11s %-18s %-34s %-10s %16s%n", "Transaction ID", "Account ID",
                "Transaction Type", "Tran Category", "Tran Source", "Amount"));
        out.append("-".repeat(110)).append(System.lineSeparator());
    }

    private static void total(StringBuilder out, String label, BigDecimal amount) {
        out.append(String.format("%-93s %,16.2f%n", label + " " + ".".repeat(Math.max(0, 80 - label.length())),
                amount));
    }
}
