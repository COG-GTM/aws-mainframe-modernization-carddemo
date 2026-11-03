package com.carddemo.card.api;

import com.carddemo.card.api.dto.CardXrefResponse;
import com.carddemo.card.service.CardService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/** Access path of the XREFAIX alternate index, used by the interest calculation job. */
@RestController
@RequestMapping("/api/v1/card-xrefs")
public class CardXrefController {

    private final CardService cardService;

    public CardXrefController(CardService cardService) {
        this.cardService = cardService;
    }

    @GetMapping("/by-account/{accountId}")
    public CardXrefResponse byAccount(@PathVariable long accountId) {
        return CardXrefResponse.from(cardService.xrefByAccount(accountId));
    }
}
