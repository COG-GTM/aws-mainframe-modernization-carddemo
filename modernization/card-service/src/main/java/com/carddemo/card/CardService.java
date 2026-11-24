package com.carddemo.card;

import com.carddemo.common.domain.Card;
import com.carddemo.common.domain.CardRepository;
import com.carddemo.common.error.ApiException;
import com.carddemo.common.web.KeysetBrowser;
import com.carddemo.common.web.KeysetPage;
import jakarta.persistence.EntityManager;
import java.time.LocalDate;
import java.time.YearMonth;
import java.util.Locale;
import java.util.Objects;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COCRDLIC (CCLI, card list), COCRDSLC (CCDL, card detail), COCRDUPC (CCUP, card update). */
@Service
public class CardService {

    static final int PAGE_SIZE = 7;

    private final CardRepository cards;
    private final EntityManager em;

    public CardService(CardRepository cards, EntityManager em) {
        this.cards = cards;
        this.em = em;
    }

    static Long parseAccountFilter(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        if (!raw.matches("\\d{11}") || Long.parseLong(raw) == 0) {
            throw ApiException.badRequest("ACCOUNT_FILTER", "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER");
        }
        return Long.parseLong(raw);
    }

    static String parseCardFilter(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        if (!raw.matches("\\d{16}")) {
            throw ApiException.badRequest("CARD_FILTER", "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER");
        }
        return raw;
    }

    @Transactional(readOnly = true)
    public KeysetPage<CardView> list(String rawAcct, String rawCard, String after, String before, int page) {
        Long acctId = parseAccountFilter(rawAcct);
        String cardNum = parseCardFilter(rawCard);
        KeysetPage<CardView> result;
        try {
            result = KeysetBrowser.browse(PAGE_SIZE, page, null, after, before,
                    (from, n) -> cards.browseForward(from, acctId, cardNum, PageRequest.of(0, n))
                            .stream().map(CardView::of).toList(),
                    (from, n) -> cards.browseBackward(from, acctId, cardNum, PageRequest.of(0, n))
                            .stream().map(CardView::of).toList(),
                    CardView::cardNum);
        } catch (ApiException e) {
            if ("TOP_OF_PAGE".equals(e.getCode())) {
                throw ApiException.badRequest("TOP_OF_PAGE", "NO PREVIOUS PAGES TO DISPLAY");
            }
            if ("BOTTOM_OF_PAGE".equals(e.getCode())) {
                throw ApiException.badRequest("BOTTOM_OF_PAGE", "NO MORE PAGES TO DISPLAY");
            }
            throw e;
        }
        if (result.items().isEmpty()) {
            throw ApiException.notFound("NO_RECORDS", "NO RECORDS FOUND FOR THIS SEARCH CONDITION.");
        }
        return result;
    }

    @Transactional(readOnly = true)
    public CardView view(String rawCard, String rawAcct) {
        return CardView.of(find(rawCard, rawAcct));
    }

    private Card find(String rawCard, String rawAcct) {
        if (rawCard == null || !rawCard.matches("\\d{16}")) {
            throw ApiException.badRequest("CARD_INVALID", "Card number if supplied must be a 16 digit number");
        }
        Long acctId = rawAcct == null || rawAcct.isBlank() ? null : parseAccountFilter(rawAcct);
        Card card = cards.findById(rawCard).orElseThrow(() ->
                ApiException.notFound("CARD_NOT_FOUND", "Did not find cards for this search condition"));
        if (acctId != null && !acctId.equals(card.getAcctId())) {
            throw ApiException.notFound("CARD_NOT_FOUND", "Did not find cards for this search condition");
        }
        return card;
    }

    @Transactional
    public String update(String rawCard, CardUpdateRequest r) {
        Card card = find(rawCard, null);
        String name = r.embossedName() == null ? "" : r.embossedName().trim();
        if (name.isEmpty()) {
            throw ApiException.badRequest("VALIDATION", "Card name not provided");
        }
        if (!name.matches("[A-Za-z ]+")) {
            throw ApiException.badRequest("VALIDATION", "Card name can only contain alphabets and spaces");
        }
        String status = r.activeStatus() == null ? "" : r.activeStatus().trim().toUpperCase(Locale.ROOT);
        if (!status.equals("Y") && !status.equals("N")) {
            throw ApiException.badRequest("VALIDATION", "Card Active Status must be Y or N");
        }
        if (r.expiryMonth() == null || r.expiryMonth() < 1 || r.expiryMonth() > 12) {
            throw ApiException.badRequest("VALIDATION", "Card expiry month must be between 1 and 12");
        }
        if (r.expiryYear() == null || r.expiryYear() < 1950 || r.expiryYear() > 2099) {
            throw ApiException.badRequest("VALIDATION", "Invalid card expiry year");
        }
        if (r.version() != card.getVersion()) {
            throw new OptimisticLockingFailureException("Record changed by some one else. Please review");
        }
        // CCUP edits month/year only; the day is kept from the existing record (or month end if it no longer fits).
        LocalDate old = card.getExpirationDate();
        YearMonth ym = YearMonth.of(r.expiryYear(), r.expiryMonth());
        int day = old == null ? ym.lengthOfMonth() : Math.min(old.getDayOfMonth(), ym.lengthOfMonth());
        LocalDate expiry = ym.atDay(day);
        if (Objects.equals(card.getEmbossedName(), name) && Objects.equals(card.getActiveStatus(), status)
                && Objects.equals(old, expiry)) {
            throw ApiException.badRequest("NO_CHANGE", "No change detected with respect to values fetched.");
        }
        card.setEmbossedName(name);
        card.setActiveStatus(status);
        card.setExpirationDate(expiry);
        return "Changes committed to database";
    }
}
