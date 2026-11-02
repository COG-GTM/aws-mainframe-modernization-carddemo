package com.carddemo.card.service;

import com.carddemo.card.api.dto.CardUpdateRequest;
import com.carddemo.card.domain.Card;
import com.carddemo.card.domain.CardXref;
import com.carddemo.card.repository.CardRepository;
import com.carddemo.card.repository.CardXrefRepository;
import com.carddemo.common.error.BusinessRuleException;
import com.carddemo.common.error.InvalidFieldException;
import com.carddemo.common.error.NotFoundException;
import com.carddemo.common.error.RecordChangedException;
import java.time.LocalDate;
import java.time.YearMonth;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.regex.Pattern;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class CardService {

    /** 1230-EDIT-NAME: LIT-ALL-ALPHA-FROM converted to spaces must leave nothing. */
    private static final Pattern CARD_NAME = Pattern.compile("[A-Za-z ]*");

    private final CardRepository cards;
    private final CardXrefRepository xrefs;

    public CardService(CardRepository cards, CardXrefRepository xrefs) {
        this.cards = cards;
        this.xrefs = xrefs;
    }

    /** COCRDSLC 9100-GETCARD-BYACCTCARD: read CARDDAT by card number; NOTFND has the screen message. */
    @Transactional(readOnly = true)
    public Card get(String cardNumber) {
        return cards.findById(cardNumber).orElseThrow(() -> new NotFoundException(CardMessages.NOT_FOUND));
    }

    /**
     * COCRDLIC browses CARDDAT in key order and 9500-FILTER-RECORDS drops records whose account id or
     * card number differ from the filters that were typed in, so the two filters intersect. The
     * program does not branch on the user type: admin and user see the same rows. {@code customerId}
     * is not on the screen; it goes through CARDXREF and intersects with the other two.
     */
    @Transactional(readOnly = true)
    public Page<Card> list(Long accountId, String cardNumber, Long customerId, Pageable pageable) {
        Specification<Card> filter = Specification.where(null);
        if (accountId != null) {
            filter = filter.and((card, query, cb) -> cb.equal(card.get("acctId"), accountId));
        }
        if (cardNumber != null) {
            filter = filter.and((card, query, cb) -> cb.equal(card.get("cardNum"), cardNumber));
        }
        if (customerId != null) {
            List<String> cardNumbers = xrefs.findByCustId(customerId).stream()
                    .map(CardXref::getCardNum)
                    .toList();
            if (cardNumbers.isEmpty()) {
                return Page.empty(pageable);
            }
            filter = filter.and((card, query, cb) -> card.get("cardNum").in(cardNumbers));
        }
        return cards.findAll(filter, pageable);
    }

    /**
     * COCRDUPC. The edits (1230-1260) run as request validation. 9200-WRITE-PROCESSING reads the
     * record with {@code UPDATE} and 9300-CHECK-CHANGE-IN-REC rejects the rewrite when it differs from
     * the copy the user edited (here: the version the client read). 1200-EDIT-MAP-INPUTS compares the
     * new values with that copy in upper case and writes nothing when they are equal. The rewrite
     * keeps the expiry day, CVV and account id of the stored record. Deliberate difference: the
     * letters-and-spaces edit applies only to a changed name, so a stored name outside that alphabet
     * (sample card 7026637615032277, {@code Lucious O'Connell}) does not block a status or expiry
     * change; COBOL would make the user retype it.
     */
    @Transactional
    public Card update(String cardNumber, CardUpdateRequest request) {
        Card card = cards.findByCardNum(cardNumber)
                .orElseThrow(() -> new NotFoundException(CardMessages.NOT_FOUND));
        if (card.getVersion() != request.version()) {
            throw new RecordChangedException();
        }
        String newName = request.embossedName().stripTrailing();
        if (!sameIgnoringCase(newName, card.getEmbossedName()) && !CARD_NAME.matcher(newName).matches()) {
            throw new InvalidFieldException("embossedName", CardMessages.NAME_MUST_BE_ALPHA);
        }
        LocalDate newExpiry = expiry(card.getExpirationDate(), request.expirationYear(), request.expirationMonth());
        if (sameIgnoringCase(newName, card.getEmbossedName())
                && sameIgnoringCase(request.activeStatus(), card.getActiveStatus())
                && card.getExpirationDate() != null
                && YearMonth.from(card.getExpirationDate()).equals(YearMonth.from(newExpiry))) {
            throw new BusinessRuleException(CardMessages.NO_CHANGES_DETECTED);
        }
        card.setEmbossedName(newName);
        card.setExpirationDate(newExpiry);
        card.setActiveStatus(request.activeStatus());
        return cards.saveAndFlush(card);
    }

    /**
     * The new expiry is the typed year and month with the stored day (CCUP-NEW-EXPDAY comes from the
     * hidden EXPDAY field). Hypothesis: when that day does not exist in the new month, the COBOL would
     * write an invalid date string; here it becomes the last day of the month. A card with no stored
     * expiry also gets the last day of the month.
     */
    static LocalDate expiry(LocalDate stored, int year, int month) {
        YearMonth yearMonth = YearMonth.of(year, month);
        int day = stored == null ? yearMonth.lengthOfMonth() : Math.min(stored.getDayOfMonth(), yearMonth.lengthOfMonth());
        return yearMonth.atDay(day);
    }

    private static boolean sameIgnoringCase(String typed, String stored) {
        return Objects.equals(upper(typed), upper(stored));
    }

    private static String upper(String value) {
        return value == null ? "" : value.stripTrailing().toUpperCase(Locale.ROOT);
    }

    /** CARDXREF read by card number, paragraph 1500-A-LOOKUP-XREF of CBTRN02C. */
    @Transactional(readOnly = true)
    public CardXref xrefByCard(String cardNumber) {
        return xrefs.findById(cardNumber)
                .orElseThrow(() -> new NotFoundException("No cross reference for card " + cardNumber));
    }

    /** CARDXREF read through the XREFAIX alternate index, paragraph 1110-GET-XREF-DATA of CBACT04C. */
    @Transactional(readOnly = true)
    public CardXref xrefByAccount(long accountId) {
        return xrefs.findFirstByAcctIdOrderByCardNum(accountId)
                .orElseThrow(() -> new NotFoundException("No cross reference for account " + accountId));
    }
}
