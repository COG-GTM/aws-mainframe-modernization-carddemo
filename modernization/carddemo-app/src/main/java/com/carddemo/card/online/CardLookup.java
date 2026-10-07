package com.carddemo.card.online;

import com.carddemo.card.Card;
import com.carddemo.card.CardRepository;
import com.carddemo.common.AbendException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.online.CicsFileErrors;
import java.util.Optional;
import java.util.function.Supplier;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * {@code 9000-READ-DATA} of COCRDSLC/COCRDUPC: {@code 9100-GETCARD-BYACCTCARD} reads CARDDAT by card number;
 * {@code 9150-GETCARD-BYACCT} reads the CARDAIX path by account id (lowest card number of the account).
 */
@Service
public class CardLookup {

    public static final String CARDDAT = "CARDDAT";
    public static final String CARDAIX = "CARDAIX";
    public static final String MSG_DISPLAYING = "   Displaying requested details";
    public static final String MSG_NOT_FOUND = "Did not find cards for this search condition";
    public static final String MSG_ACCOUNT_NOT_FOUND = "Did not find this account in cards database";

    private final CardRepository cards;

    public CardLookup(CardRepository cards) {
        this.cards = cards;
    }

    /**
     * {@code READ FILE('CARDDAT') RIDFLD(CC-CARD-NUM)}. COBOL does not cross-check the typed account against
     * {@code CARD-ACCT-ID} (COCRDSLC R-16); {@code restrictToAccount} adds that check for a USER (ADR-0020), with the
     * NOTFND answer so a card of another account is indistinguishable from a missing one.
     */
    @Transactional(readOnly = true)
    public Card byCardNumber(CardKeys keys, boolean restrictToAccount) {
        return find(keys, restrictToAccount);
    }

    /** {@link #byCardNumber} inside the caller's transaction (the entity stays managed). */
    Card find(CardKeys keys, boolean restrictToAccount) {
        return read(CARDDAT, () -> cards.findById(keys.cardNum()))
                .filter(c -> !restrictToAccount || c.getAcctId() == keys.acctId())
                .orElseThrow(() -> new RecordNotFoundException(MSG_NOT_FOUND));
    }

    /** {@code 9150-GETCARD-BYACCT}: {@code READ FILE('CARDAIX') RIDFLD(WS-CARD-RID-ACCT-ID)}. */
    @Transactional(readOnly = true)
    public Card byAccount(long acctId) {
        Optional<Card> card = read(CARDAIX, () -> cards.findFirstByAcctIdOrderByCardNumAsc(acctId));
        return card.orElseThrow(() -> new RecordNotFoundException(MSG_ACCOUNT_NOT_FOUND));
    }

    /** Other RESP: {@code WS-FILE-ERROR-MESSAGE} (COCRDSLC R-18, COCRDUPC R-27). */
    static <T> T read(String dataset, Supplier<T> read) {
        try {
            return read.get();
        } catch (DataAccessException e) {
            throw new AbendException(AbendException.CARDDEMO_ABEND_CODE, CicsFileErrors.message("READ", dataset), e);
        }
    }
}
