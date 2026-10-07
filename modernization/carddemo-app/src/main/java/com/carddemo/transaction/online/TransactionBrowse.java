package com.carddemo.transaction.online;

import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.data.KeysetPage;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionRepository;
import java.util.List;
import java.util.function.Supplier;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/** COTRN00C (CT00): ten transactions per page in TRAN-ID order, forward and backward (ADR-0011 keyset browse). */
@Component
public class TransactionBrowse {

    public static final String TRAN_ID_FIELD = "tranId";
    public static final String MSG_TRAN_ID_NOT_NUMERIC = "Tran ID must be Numeric ...";
    public static final String MSG_TOP_OF_PAGE = "You are at the top of the page...";
    public static final String MSG_REACHED_BOTTOM = "You have reached the bottom of the page...";
    public static final String MSG_REACHED_TOP = "You have reached the top of the page...";
    public static final String MSG_ALREADY_TOP = "You are already at the top of the page...";
    public static final String MSG_ALREADY_BOTTOM = "You are already at the bottom of the page...";
    public static final String MSG_LOOKUP_FAILED = "Unable to lookup transaction...";
    public static final String MSG_BAD_CURSOR = "Cursor must be a transaction id (up to 16 digits) from the page shown";

    private final TransactionRepository transactions;

    public TransactionBrowse(TransactionRepository transactions) {
        this.transactions = transactions;
    }

    /**
     * One screen of the list.
     *
     * @param rows        up to ten transactions in ascending TRAN-ID order
     * @param pageNumber  {@code PAGENUM} ({@code CDEMO-CT00-PAGE-NUM}); 0 when nothing was shown
     * @param hasPrevious PF7 would show an earlier page ({@code PAGE-NUM > 1})
     * @param hasNext     PF8 would show a later page ({@code NEXT-PAGE-YES})
     * @param message     {@code ERRMSG}, blank when none
     */
    public record Screen(List<Transaction> rows, int pageNumber, boolean hasPrevious, boolean hasNext,
            String message) {
    }

    /**
     * ENTER ({@code startTranId}, no cursor), PF8 ({@code after} = the last id shown) or PF7 ({@code before} = the
     * first id shown). {@code page} is the {@code PAGENUM} of the screen shown; it decides R-14 (PF7 on page 1).
     */
    @Transactional(readOnly = true)
    public Screen browse(String startTranId, String after, String before, Integer page) {
        if (before != null) {
            return backward(cursor(before, "before"), page);
        }
        if (after != null) {
            return forward(cursor(after, "after"), page == null ? 1 : page);
        }
        String key = startKey(startTranId);
        KeysetPage<Transaction> first = read(() -> transactions.browseFrom(key));
        if (first.isEmpty()) {
            return new Screen(List.of(), 0, false, false, MSG_TOP_OF_PAGE);
        }
        return new Screen(first.rows(), 1, false, first.more(), first.more() ? "" : MSG_REACHED_BOTTOM);
    }

    /** R-15/R-16/R-19: the rows after the last one shown; nothing after it → R-16 (rows left as they were). */
    private Screen forward(String lastShown, int page) {
        KeysetPage<Transaction> next = read(() -> transactions.nextPage(lastShown));
        if (next.isEmpty()) {
            return new Screen(List.of(), page, page > 1, false, MSG_ALREADY_BOTTOM);
        }
        return new Screen(next.rows(), page + 1, true, next.more(), next.more() ? "" : MSG_REACHED_BOTTOM);
    }

    /** R-13/R-14/R-20: PF7 on page 1 (or with nothing before) re-shows the page from the first id shown. */
    private Screen backward(String firstShown, Integer page) {
        KeysetPage<Transaction> previous = page != null && page <= 1 ? new KeysetPage<>(List.of(), false)
                : read(() -> transactions.previousPage(firstShown));
        if (previous.isEmpty()) {
            KeysetPage<Transaction> same = read(() -> transactions.browseFrom(firstShown));
            return new Screen(same.rows(), same.isEmpty() ? 0 : 1, false, same.more(), MSG_ALREADY_TOP);
        }
        int number = page == null ? (previous.more() ? 2 : 1) : Math.max(1, page - 1);
        return new Screen(previous.rows(), number, previous.more(), true,
                previous.more() ? "" : MSG_REACHED_TOP);
    }

    /**
     * R-9..R-11: blank → LOW-VALUES; numeric → exact-or-greater positioning on the id (left-padded with zeros to
     * the 16 digits ids have); anything else → {@code Tran ID must be Numeric ...}.
     */
    static String startKey(String startTranId) {
        if (ScreenInput.isSpacesOrLowValues(startTranId)) {
            return "";
        }
        String typed = ScreenInput.rightTrim(startTranId);
        if (!isDigits(typed, 16)) {
            throw new InvalidRequestException(TRAN_ID_FIELD, MSG_TRAN_ID_NOT_NUMERIC);
        }
        return "0".repeat(16 - typed.length()) + typed;
    }

    private static String cursor(String value, String field) {
        String typed = value.strip();
        if (!isDigits(typed, 16)) {
            throw new InvalidRequestException(field, MSG_BAD_CURSOR);
        }
        return "0".repeat(16 - typed.length()) + typed;
    }

    static boolean isDigits(String text, int maxLength) {
        return !text.isEmpty() && text.length() <= maxLength && text.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    /** R-18..R-20 other RESP: {@code Unable to lookup transaction...}. */
    private static <T> T read(Supplier<T> read) {
        try {
            return read.get();
        } catch (DataAccessException e) {
            throw AbendException.carddemo(MSG_LOOKUP_FAILED, e);
        }
    }
}
