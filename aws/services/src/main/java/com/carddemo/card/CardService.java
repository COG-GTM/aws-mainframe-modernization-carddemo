package com.carddemo.card;

import com.carddemo.card.CardDtos.CardDetail;
import com.carddemo.card.CardDtos.CardSummary;
import com.carddemo.card.CardDtos.CardUpdateRequest;
import com.carddemo.common.ApiException;
import com.carddemo.common.ErrorCode;
import com.carddemo.common.LegacyMessages;
import com.carddemo.common.PageQuery;
import com.carddemo.common.PageResponse;
import com.carddemo.common.Text;
import com.carddemo.common.ValidationErrors;
import java.time.LocalDate;
import java.time.YearMonth;
import java.util.List;
import org.springframework.dao.PessimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * COCRDLIC (list), COCRDSLC (detail) and COCRDUPC (update). The legacy list has no per-user ownership
 * scope (users and admins browse all of CARDDAT); acctId/cardNum are optional filters for both roles.
 */
@Service
public class CardService {

    static final int DEFAULT_PAGE_SIZE = 7;
    static final String LIST_PROGRAM = "COCRDLIC";
    static final String DETAIL_PROGRAM = "COCRDSLC";
    static final String UPDATE_PROGRAM = "COCRDUPC";

    private final CardRepository cards;

    public CardService(CardRepository cards) {
        this.cards = cards;
    }

    @Transactional(readOnly = true)
    public PageResponse<CardSummary> list(String acctIdIn, String cardNumIn, String startKey, String direction,
            Integer pageSize) {
        ValidationErrors errors = new ValidationErrors(LIST_PROGRAM);
        Long acctId = null;
        String cardNum = null;
        if (!Text.isBlank(acctIdIn)) {
            String v = acctIdIn.strip();
            if (v.length() > 11 || !Text.isDigits(v) || Text.isAllZero(v)) {
                errors.add("acctId", "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER");
            } else {
                acctId = Long.parseLong(v);
            }
        }
        if (!Text.isBlank(cardNumIn)) {
            String v = cardNumIn.strip();
            if (v.length() != 16 || !Text.isDigits(v) || Text.isAllZero(v)) {
                errors.add("cardNum", "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER");
            } else {
                cardNum = v;
            }
        }
        errors.throwIfAny();
        PageQuery query = PageQuery.of(startKey, direction, pageSize, DEFAULT_PAGE_SIZE, LIST_PROGRAM);
        PageResponse<CardRecord> page = cards.page(acctId, cardNum, query);
        List<CardSummary> items = page.items().stream()
                .map(c -> new CardSummary(c.cardNum(), c.acctId(), c.activeStatus()))
                .toList();
        String message = null;
        if (items.isEmpty()) {
            message = query.startKey() == null ? "NO RECORDS FOUND FOR THIS SEARCH CONDITION."
                    : query.direction() == PageQuery.Direction.NEXT ? "NO MORE PAGES TO DISPLAY"
                            : "NO PREVIOUS PAGES TO DISPLAY";
        }
        return new PageResponse<>(items, page.firstKey(), page.lastKey(), page.hasNext(), page.hasPrev(), message);
    }

    @Transactional(readOnly = true)
    public CardDetail detail(String cardNumIn, String acctIdIn) {
        String cardNum = parseCardNum(cardNumIn, DETAIL_PROGRAM);
        Long acctId = null;
        if (!Text.isBlank(acctIdIn)) {
            String v = acctIdIn.strip();
            if (v.length() > 11 || !Text.isDigits(v) || Text.isAllZero(v)) {
                throw ApiException.validation(DETAIL_PROGRAM, "acctId",
                        "Account number must be a non zero 11 digit number");
            }
            acctId = Long.parseLong(v);
        }
        CardRecord card = cards.findById(cardNum)
                .orElseThrow(() -> ApiException.notFound(DETAIL_PROGRAM, "Did not find cards for this search condition"));
        if (acctId != null && card.acctId() != acctId) {
            throw ApiException.notFound(DETAIL_PROGRAM, "Did not find cards for this search condition");
        }
        return toDetail(card, null);
    }

    @Transactional
    public CardDetail update(String cardNumIn, CardUpdateRequest request) {
        if (request == null) {
            throw new ApiException(ErrorCode.INVALID_REQUEST, "No input received", UPDATE_PROGRAM);
        }
        ValidationErrors keyErrors = new ValidationErrors(UPDATE_PROGRAM);
        if (Text.isBlank(request.acctId())) {
            keyErrors.add("acctId", "Account number not provided");
        } else if (request.acctId().strip().length() > 11 || !Text.isDigits(request.acctId().strip())
                || Text.isAllZero(request.acctId().strip())) {
            keyErrors.add("acctId", "Account number must be a non zero 11 digit number");
        }
        keyErrors.throwIfAny();
        String cardNum = parseCardNum(cardNumIn, UPDATE_PROGRAM);
        long acctId = Long.parseLong(request.acctId().strip());
        if (request.version() == null) {
            throw ApiException.validation(UPDATE_PROGRAM, "version", "version is required");
        }

        CardRecord current = cards.findById(cardNum)
                .filter(c -> c.acctId() == acctId)
                .orElseThrow(() -> ApiException.notFound(UPDATE_PROGRAM, "Did not find cards for this search condition"));

        String name = Text.trimToEmpty(request.embossedName());
        String status = Text.upperTrim(request.activeStatus());
        String[] exp = Text.trimToEmpty(request.expirationDate()).split("-", -1);
        String expYear = exp.length > 0 ? exp[0] : "";
        String expMonth = exp.length > 1 ? exp[1] : "";

        String oldYear = current.expirationDate() == null ? "" : String.valueOf(current.expirationDate().getYear());
        String oldMonth = current.expirationDate() == null ? ""
                : String.format("%02d", current.expirationDate().getMonthValue());
        if (name.equalsIgnoreCase(Text.trimToEmpty(current.embossedName()))
                && status.equals(Text.upperTrim(current.activeStatus()))
                && expYear.equals(oldYear) && normalizeMonth(expMonth).equals(oldMonth)) {
            throw ApiException.businessRule(UPDATE_PROGRAM, LegacyMessages.NO_CHANGE);
        }

        ValidationErrors errors = new ValidationErrors(UPDATE_PROGRAM);
        if (name.isEmpty() || Text.isAllZero(name)) {
            errors.add("embossedName", "Card name not provided");
        } else if (!Text.isAlphaOrSpace(name)) {
            errors.add("embossedName", "Card name can only contain alphabets and spaces");
        } else if (name.length() > 50) {
            errors.add("embossedName", "Card name can not be longer than 50 characters");
        }
        if (!status.equals("Y") && !status.equals("N")) {
            errors.add("activeStatus", "Card Active Status must be Y or N");
        }
        int month = Text.isDigits(expMonth) && expMonth.length() <= 2 ? Integer.parseInt(expMonth) : 0;
        if (month < 1 || month > 12) {
            errors.add("expirationDate", "Card expiry month must be between 1 and 12");
        }
        int year = Text.isDigits(expYear) && expYear.length() == 4 ? Integer.parseInt(expYear) : 0;
        if (year < 1950 || year > 2099) {
            errors.add("expirationDate", "Invalid card expiry year");
        }
        errors.throwIfAny();

        CardRecord locked;
        try {
            locked = cards.lockNoWait(cardNum).orElseThrow(
                    () -> new ApiException(ErrorCode.LOCKED, LegacyMessages.COULD_NOT_LOCK, UPDATE_PROGRAM));
        } catch (PessimisticLockingFailureException ex) {
            throw new ApiException(ErrorCode.LOCKED, LegacyMessages.COULD_NOT_LOCK, UPDATE_PROGRAM);
        }
        if (locked.version() != request.version()) {
            throw ApiException.concurrentUpdate(UPDATE_PROGRAM);
        }
        YearMonth ym = YearMonth.of(year, month);
        int oldDay = locked.expirationDate() == null ? 1 : locked.expirationDate().getDayOfMonth();
        LocalDate expirationDate = ym.atDay(Math.min(oldDay, ym.lengthOfMonth()));
        CardRecord updated = new CardRecord(cardNum, locked.acctId(), locked.cvvCd(), name, expirationDate, status,
                locked.version());
        if (cards.update(updated) != 1) {
            throw new ApiException(ErrorCode.INTERNAL_ERROR, LegacyMessages.UPDATE_FAILED, UPDATE_PROGRAM);
        }
        CardRecord reloaded = cards.findById(cardNum).orElseThrow();
        return toDetail(reloaded, LegacyMessages.CHANGES_COMMITTED);
    }

    private static String normalizeMonth(String month) {
        return month.length() == 1 ? "0" + month : month;
    }

    static String parseCardNum(String value, String program) {
        if (Text.isBlank(value)) {
            throw ApiException.validation(program, "cardNum", "Card number not provided");
        }
        String v = value.strip();
        if (v.length() != 16 || !Text.isDigits(v) || Text.isAllZero(v)) {
            throw ApiException.validation(program, "cardNum", "Card number if supplied must be a 16 digit number");
        }
        return v;
    }

    private static CardDetail toDetail(CardRecord c, String message) {
        return new CardDetail(c.cardNum(), c.acctId(), c.cvvCd(), c.embossedName(),
                c.expirationDate() == null ? "" : c.expirationDate().toString(), c.activeStatus(), c.version(),
                message);
    }
}
