package com.carddemo.card.online;

import com.carddemo.card.Card;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardRepository;
import com.carddemo.card.CardStatus;
import com.carddemo.common.AbendException;
import com.carddemo.common.FieldEditException;
import com.carddemo.common.PanMask;
import com.carddemo.common.Versions;
import com.carddemo.common.online.ScreenInput;
import java.util.List;
import java.util.function.Supplier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * COCRDUPC {@code 2000-DECIDE-ACTION} and {@code 9200-WRITE-PROCESSING} for one stateless request (ADR-0010):
 * read, version check against the version that was displayed, change detection, the field edits, then
 * {@code confirm=false} (ENTER: validated, nothing written) or {@code confirm=true} (PF5: lock, re-check,
 * {@code REWRITE}).
 */
@Service
public class CardUpdateService {

    private static final Logger log = LoggerFactory.getLogger(CardUpdateService.class);

    public static final String MSG_LOCK_FAILED = "Could not lock record for update";
    public static final String MSG_UPDATE_FAILED = "Update of record failed";

    /** {@code CCUP-CHANGE-ACTION} after the request, with the info line COCRDUPC shows for it (R-32). */
    public enum State {
        SHOW("Details of selected card shown above"),
        VALIDATED("Changes validated.Press F5 to save"),
        COMMITTED("Changes committed to database");

        private final String infoMessage;

        State(String infoMessage) {
            this.infoMessage = infoMessage;
        }

        public String infoMessage() {
            return infoMessage;
        }
    }

    public record Outcome(State state, String message, Card card) {
    }

    private final CardLookup lookup;
    private final CardRepository cards;

    public CardUpdateService(CardLookup lookup, CardRepository cards) {
        this.lookup = lookup;
        this.cards = cards;
    }

    @Transactional
    public Outcome update(CardKeys keys, CardChanges typed, long version, boolean confirm,
            boolean restrictToAccount) {
        Card card = lookup.find(keys, restrictToAccount);
        Versions.requireCurrent(Card.class, PanMask.mask(card.getCardNum()), version, card.getVersion());

        if (CardUpdateEdits.noChanges(CardChanges.fetched(card), typed)) {
            return new Outcome(State.SHOW, CardUpdateEdits.MSG_NO_CHANGES, card);
        }
        List<CardUpdateEdits.FieldError> errors = CardUpdateEdits.edit(typed);
        if (!errors.isEmpty()) {
            throw new FieldEditException(errors.get(0).field(), errors.get(0).message(),
                    errors.stream().map(CardUpdateEdits.FieldError::field).toList());
        }
        if (!confirm) {
            return new Outcome(State.VALIDATED, "", card);
        }

        long locked = write(() -> cards.lockVersion(card.getCardNum()), MSG_LOCK_FAILED)
                .orElseThrow(() -> new AbendException(AbendException.CARDDEMO_ABEND_CODE, MSG_LOCK_FAILED));
        Versions.requireCurrent(Card.class, PanMask.mask(card.getCardNum()), version, locked);
        card.update(rewrite(card, typed));
        write(() -> cards.saveAndFlush(card), MSG_UPDATE_FAILED);
        log.info("COCRDUPC REWRITE card {} account {} version {}", PanMask.mask(card.getCardNum()),
                card.getAcctId(), card.getVersion());
        return new Outcome(State.COMMITTED, "", card);
    }

    /**
     * {@code CARD-UPDATE-RECORD} (R-30): name as typed (not upper-cased), {@code YYYY-MM-DD} from the typed year
     * and month and the day that was read, status as typed; CVV and keys unchanged. COBOL moves the typed account
     * id into {@code CARD-UPDATE-ACCT-ID}; here the account of the card is kept (the key fields are protected once
     * details are fetched, R-33).
     */
    private static CardRecord rewrite(Card card, CardChanges typed) {
        String expiry = typed.expiryYear().strip() + "-" + CardUpdateEdits.month(typed.expiryMonth()) + "-"
                + CardChanges.expiryDay(card);
        return new CardRecord(card.getCardNum(), card.getAcctId(), card.getCvvCd(),
                ScreenInput.rightTrim(typed.embossedName()), expiry, CardStatus.fromCode(typed.activeStatus().strip()));
    }

    private static <T> T write(Supplier<T> io, String failure) {
        try {
            return io.get();
        } catch (ObjectOptimisticLockingFailureException e) {
            throw e;
        } catch (DataAccessException e) {
            throw new AbendException(AbendException.CARDDEMO_ABEND_CODE, failure, e);
        }
    }
}
