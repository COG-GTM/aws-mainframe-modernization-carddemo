package com.carddemo.card;

import com.carddemo.card.CardDtos.CardDetail;
import com.carddemo.card.CardDtos.CardSummary;
import com.carddemo.card.CardDtos.CardUpdateRequest;
import com.carddemo.common.PageResponse;
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

    private final CardService service;

    public CardController(CardService service) {
        this.service = service;
    }

    @GetMapping
    public PageResponse<CardSummary> list(@RequestParam(required = false) String acctId,
            @RequestParam(required = false) String cardNum, @RequestParam(required = false) String startKey,
            @RequestParam(required = false) String direction, @RequestParam(required = false) Integer pageSize) {
        return service.list(acctId, cardNum, startKey, direction, pageSize);
    }

    @GetMapping("/{cardNum}")
    public CardDetail detail(@PathVariable String cardNum, @RequestParam(required = false) String acctId) {
        return service.detail(cardNum, acctId);
    }

    @PutMapping("/{cardNum}")
    public CardDetail update(@PathVariable String cardNum, @RequestBody(required = false) CardUpdateRequest request) {
        return service.update(cardNum, request);
    }
}
