package com.carddemo.card.api;

import com.carddemo.card.api.dto.CardResponse;
import com.carddemo.card.api.dto.CardUpdateRequest;
import com.carddemo.card.api.dto.CardXrefResponse;
import com.carddemo.card.service.CardService;
import com.carddemo.common.api.PageResponse;
import jakarta.validation.Valid;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/cards")
public class CardController {

    private final CardService cardService;

    public CardController(CardService cardService) {
        this.cardService = cardService;
    }

    /** CICS CCLI / COCRDLIC. */
    @GetMapping
    public PageResponse<CardResponse> list(@RequestParam(required = false) Long accountId,
                                           @RequestParam(required = false) Long customerId,
                                           @RequestParam(defaultValue = "0") int page,
                                           @RequestParam(defaultValue = "20") int size) {
        return PageResponse.of(cardService
                .list(accountId, customerId, PageRequest.of(page, size, Sort.by("cardNum")))
                .map(CardResponse::from));
    }

    /** CICS CCDL / COCRDSLC. */
    @GetMapping("/{cardNumber}")
    public CardResponse get(@PathVariable String cardNumber) {
        return CardResponse.from(cardService.get(cardNumber));
    }

    /** CICS CCUP / COCRDUPC. */
    @PutMapping("/{cardNumber}")
    public CardResponse update(@PathVariable String cardNumber, @Valid @RequestBody CardUpdateRequest request) {
        return CardResponse.from(cardService.update(cardNumber, request));
    }

    /** CARDXREF lookup by card number (CBTRN02C 1500-A-LOOKUP-XREF). */
    @GetMapping("/{cardNumber}/xref")
    public CardXrefResponse xref(@PathVariable String cardNumber) {
        return CardXrefResponse.from(cardService.xrefByCard(cardNumber));
    }
}
