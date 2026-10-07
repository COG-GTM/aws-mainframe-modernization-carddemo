package com.carddemo.card.online;

import com.carddemo.card.Card;
import com.carddemo.card.CardRepository;
import com.carddemo.common.AbendException;
import com.carddemo.common.data.KeysetPage;
import com.carddemo.common.online.CicsFileErrors;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * COCRDLIC's browse of CARDDAT ({@code 9000-READ-FORWARD}, {@code 9100-READ-BACKWARDS}, {@code 9500-FILTER-RECORDS})
 * on the ADR-0011 keyset pages of {@link CardRepository}: card-number order, seven rows, one look-ahead record.
 * Both filters apply when both are valid (a card filter with an account filter only shows that card if it belongs
 * to the account).
 */
@Service
public class CardBrowse {

    public static final String CARDDAT = "CARDDAT";
    public static final String MSG_NO_PREVIOUS_PAGES = "NO PREVIOUS PAGES TO DISPLAY";
    public static final String MSG_NO_MORE_PAGES = "NO MORE PAGES TO DISPLAY";
    public static final String MSG_NO_MORE_RECORDS = "NO MORE RECORDS TO SHOW";
    public static final String MSG_SELECT_ACTIONS = "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD";

    private final CardRepository cards;

    public CardBrowse(CardRepository cards) {
        this.cards = cards;
    }

    /**
     * One screen of the list.
     *
     * @param rows         the cards in display (ascending card number) order, at most seven
     * @param hasPrevious  PF7 would show an earlier page
     * @param hasNext      PF8 would show a later page ({@code CA-NEXT-PAGE-EXISTS})
     * @param message      {@code CCARD-ERROR-MSG} ({@code WS-ERROR-MSG}), blank when none
     * @param infoMessage  {@code INFOMSGO}, blank when none
     */
    public record Screen(List<Card> rows, boolean hasPrevious, boolean hasNext, String message,
            String infoMessage) {
    }

    /**
     * ENTER (no cursor, R-13/R-18), PF8 from the last card shown ({@code after}, R-9) or PF7 from the first card
     * shown ({@code before}, R-10/R-23). PF7 with nothing before the cursor re-reads the first page with
     * {@code NO PREVIOUS PAGES TO DISPLAY} (R-7); PF8 with nothing after it answers {@code NO MORE PAGES TO
     * DISPLAY} (R-27).
     */
    @Transactional(readOnly = true)
    public Screen browse(CardKeys filter, String after, String before) {
        if (before != null) {
            KeysetPage<Card> page = read(() -> previous(filter, before));
            if (page.isEmpty()) {
                KeysetPage<Card> first = read(() -> first(filter));
                return forwardScreen(first, false, MSG_NO_PREVIOUS_PAGES);
            }
            boolean hasNext = !read(() -> next(filter, page.last().getCardNum())).isEmpty();
            return new Screen(page.rows(), page.more(), hasNext, "", MSG_SELECT_ACTIONS);
        }
        if (after != null) {
            KeysetPage<Card> page = read(() -> next(filter, after));
            if (page.isEmpty()) {
                return new Screen(List.of(), false, false, MSG_NO_MORE_PAGES, "");
            }
            boolean hasPrevious = !read(() -> previous(filter, page.first().getCardNum())).isEmpty();
            return forwardScreen(page, hasPrevious, "");
        }
        return forwardScreen(read(() -> first(filter)), false, "");
    }

    /**
     * {@code ENDFILE} before or right after the seventh row sets {@code NO MORE RECORDS TO SHOW} (R-20, R-21); an
     * empty first page sets {@code WS-NO-RECORDS-FOUND}, which {@code 1400-SETUP-MESSAGE} then suppresses, so the
     * info line stays blank (R-21, R-27).
     */
    private static Screen forwardScreen(KeysetPage<Card> page, boolean hasPrevious, String message) {
        String error = message.isEmpty() && !page.more() ? MSG_NO_MORE_RECORDS : message;
        String info = page.isEmpty() ? "" : MSG_SELECT_ACTIONS;
        return new Screen(page.rows(), hasPrevious, page.more(), error, info);
    }

    private KeysetPage<Card> first(CardKeys f) {
        if (f.cardNum() != null) {
            return single(f, c -> true);
        }
        return f.acctId() != null ? cards.browseFrom(f.acctId(), "") : cards.browseFrom("");
    }

    private KeysetPage<Card> next(CardKeys f, String lastCardNumShown) {
        if (f.cardNum() != null) {
            return single(f, c -> c.getCardNum().compareTo(lastCardNumShown) > 0);
        }
        return f.acctId() != null ? cards.nextPage(f.acctId(), lastCardNumShown) : cards.nextPage(lastCardNumShown);
    }

    private KeysetPage<Card> previous(CardKeys f, String firstCardNumShown) {
        if (f.cardNum() != null) {
            return single(f, c -> c.getCardNum().compareTo(firstCardNumShown) < 0);
        }
        return f.acctId() != null ? cards.previousPage(f.acctId(), firstCardNumShown)
                : cards.previousPage(firstCardNumShown);
    }

    /** Card filter: at most one card (the key is unique), kept only if the account filter also matches. */
    private KeysetPage<Card> single(CardKeys f, Predicate<Card> inWindow) {
        Optional<Card> card = cards.findById(f.cardNum())
                .filter(c -> f.acctId() == null || c.getAcctId() == f.acctId())
                .filter(inWindow);
        return new KeysetPage<>(card.stream().toList(), false);
    }

    /** Other RESP: the browse ends with {@code WS-FILE-ERROR-MESSAGE} (R-22). */
    private static <T> T read(Supplier<T> read) {
        try {
            return read.get();
        } catch (DataAccessException e) {
            throw new AbendException(AbendException.CARDDEMO_ABEND_CODE, CicsFileErrors.message("READ", CARDDAT), e);
        }
    }
}
