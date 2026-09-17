package com.carddemo.card.service;

import com.carddemo.card.api.dto.CardUpdateRequest;
import com.carddemo.card.domain.Card;
import com.carddemo.card.domain.CardXref;
import com.carddemo.card.repository.CardRepository;
import com.carddemo.card.repository.CardXrefRepository;
import com.carddemo.common.error.BusinessRuleException;
import com.carddemo.common.error.NotFoundException;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class CardService {

    private final CardRepository cards;
    private final CardXrefRepository xrefs;

    public CardService(CardRepository cards, CardXrefRepository xrefs) {
        this.cards = cards;
        this.xrefs = xrefs;
    }

    /** COCRDSLC: read CARDDATA by card number. */
    @Transactional(readOnly = true)
    public Card get(String cardNumber) {
        return cards.findById(cardNumber)
                .orElseThrow(() -> new NotFoundException("Card " + cardNumber + " not found"));
    }

    /**
     * COCRDLIC. The screen browses CARDDATA, optionally filtered by account id through the CARDAIX
     * alternate index; filtering by customer goes through CARDXREF first, as the COBOL does.
     */
    @Transactional(readOnly = true)
    public Page<Card> list(Long accountId, Long customerId, Pageable pageable) {
        if (customerId != null) {
            List<String> cardNumbers = xrefs.findByCustId(customerId).stream()
                    .map(CardXref::getCardNum)
                    .toList();
            if (cardNumbers.isEmpty()) {
                return Page.empty(pageable);
            }
            return cards.findByCardNumIn(cardNumbers, pageable);
        }
        if (accountId != null) {
            return cards.findByAcctId(accountId, pageable);
        }
        return cards.findAll(pageable);
    }

    /** COCRDUPC: read for update, apply the edits, rewrite. */
    @Transactional
    public Card update(String cardNumber, CardUpdateRequest request) {
        Card card = get(cardNumber);
        if (request.expirationDate().getYear() > 2099) {
            throw new BusinessRuleException("Expiration year must be 2099 or earlier");
        }
        card.setEmbossedName(request.embossedName().toUpperCase());
        card.setExpirationDate(request.expirationDate());
        card.setActiveStatus(request.activeStatus().toUpperCase());
        if (request.cvv() != null) {
            card.setCvvCd(request.cvv());
        }
        return cards.save(card);
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
