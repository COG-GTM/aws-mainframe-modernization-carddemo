package com.carddemo.transaction.web;

import com.carddemo.transaction.cobol.Pic;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.web.TransactionDtos.TransactionDetail;
import com.carddemo.transaction.web.TransactionDtos.TransactionPage;
import com.carddemo.transaction.web.TransactionDtos.TransactionRow;
import org.springframework.dao.DataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Supplier;

/**
 * COTRN00C (transaction list, 10 rows per page) and COTRN01C (transaction view) over the TRANSACT table
 * shared with the batch posting job.
 */
@Service
@Transactional(readOnly = true)
public class TransactionInquiryService {

    static final int PAGE_SIZE = 10;

    private final TransactionRepository transactions;

    public TransactionInquiryService(TransactionRepository transactions) {
        this.transactions = transactions;
    }

    /** COTRN00C PROCESS-ENTER-KEY: STARTBR GTEQ on TRNIDINI (LOW-VALUES when blank) then PROCESS-PAGE-FORWARD. */
    public TransactionPage firstPage(String tranIdInput) {
        String start = "";
        if (!Pic.isBlank(tranIdInput)) {
            String tranId = Pic.x(tranIdInput, 16).stripTrailing();
            if (!Pic.isNumeric(tranId)) {
                throw new OnlineException(HttpStatus.BAD_REQUEST, "Tran ID must be Numeric ...");
            }
            start = Pic.x(tranId, 16);
        }
        String key = start;
        List<Transaction> rows = browse(() -> transactions.findTop11ByTranIdGreaterThanEqualOrderByTranIdAsc(key));
        return forwardPage(rows, null);
    }

    /** COTRN00C PROCESS-PF8-KEY: browse forward after CDEMO-CT00-TRNID-LAST. */
    public TransactionPage nextPage(String lastTranId) {
        String key = Pic.x(lastTranId, 16);
        List<Transaction> rows = browse(() -> transactions.findTop11ByTranIdGreaterThanOrderByTranIdAsc(key));
        if (rows.isEmpty()) {
            return new TransactionPage(List.of(), null, null, false, "You have reached the bottom of the page...");
        }
        return forwardPage(rows, null);
    }

    /** COTRN00C PROCESS-PF7-KEY / PROCESS-PAGE-BACKWARD: READPREV before CDEMO-CT00-TRNID-FIRST. */
    public TransactionPage previousPage(String firstTranId) {
        String key = Pic.x(firstTranId, 16);
        List<Transaction> rows = new ArrayList<>(browse(() -> transactions.findTop11ByTranIdLessThanOrderByTranIdDesc(key)));
        if (rows.isEmpty()) {
            return new TransactionPage(List.of(), null, null, true, "You are already at the top of the page...");
        }
        List<Transaction> page = new ArrayList<>(rows.subList(0, Math.min(PAGE_SIZE, rows.size())));
        Collections.reverse(page);
        return toPage(page, true, null);
    }

    /** COTRN01C PROCESS-ENTER-KEY + READ-TRANSACT-FILE. */
    public TransactionDetail view(String tranIdInput) {
        if (Pic.isBlank(tranIdInput)) {
            throw new OnlineException(HttpStatus.BAD_REQUEST, "Tran ID can NOT be empty...");
        }
        String key = Pic.x(tranIdInput, 16);
        return browse(() -> transactions.findById(key))
                .map(TransactionDetail::of)
                .orElseThrow(() -> new OnlineException(HttpStatus.NOT_FOUND, "Transaction ID NOT found..."));
    }

    /** COTRN02C COPY-LAST-TRAN-DATA source record: STARTBR HIGH-VALUES + READPREV. */
    public TransactionDetail last() {
        return browse(transactions::findTopByOrderByTranIdDesc)
                .map(TransactionDetail::of)
                .orElseThrow(() -> new OnlineException(HttpStatus.NOT_FOUND, "Transaction ID NOT found..."));
    }

    private TransactionPage forwardPage(List<Transaction> rows, String message) {
        boolean nextPage = rows.size() > PAGE_SIZE;
        return toPage(rows.subList(0, Math.min(PAGE_SIZE, rows.size())), nextPage, message);
    }

    private static TransactionPage toPage(List<Transaction> page, boolean nextPage, String message) {
        List<TransactionRow> out = page.stream().map(TransactionRow::of).toList();
        String first = out.isEmpty() ? null : out.get(0).tranId();
        String last = out.isEmpty() ? null : out.get(out.size() - 1).tranId();
        return new TransactionPage(out, first, last, nextPage, message);
    }

    private static <T> T browse(Supplier<T> op) {
        try {
            return op.get();
        } catch (DataAccessException e) {
            throw new OnlineException(HttpStatus.INTERNAL_SERVER_ERROR, "Unable to lookup transaction...");
        }
    }
}
