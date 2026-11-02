package com.carddemo.card.api;

import com.carddemo.card.api.dto.CardResponse;
import com.carddemo.card.api.dto.CardUpdateRequest;
import com.carddemo.card.api.dto.CardXrefResponse;
import com.carddemo.card.service.CardMessages;
import com.carddemo.card.service.CardService;
import com.carddemo.common.api.PageResponse;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Pattern;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Validated
@RequestMapping("/api/v1/cards")
public class CardController {

    private static final int MAX_PAGE_SIZE = 200;

    private final CardService cardService;

    public CardController(CardService cardService) {
        this.cardService = cardService;
    }

    /**
     * CICS CCLI / COCRDLIC. {@code accountId} and {@code cardNumber} are the screen's two optional
     * filters (2210-EDIT-ACCOUNT, 2220-EDIT-CARD; all zeros counts as blank) and intersect as in
     * 9500-FILTER-RECORDS. Rows come in card number order, the CARDDAT key the browse reads in.
     */
    @GetMapping
    public PageResponse<CardResponse> list(
            @RequestParam(required = false)
            @Pattern(regexp = "(\\d{11})?", message = CardMessages.ACCOUNT_FILTER_NOT_NUMERIC) String accountId,
            @RequestParam(required = false)
            @Pattern(regexp = "(\\d{16})?", message = CardMessages.CARD_FILTER_NOT_NUMERIC) String cardNumber,
            @RequestParam(required = false) Long customerId,
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(MAX_PAGE_SIZE) int size) {
        return PageResponse.of(cardService
                .list(accountFilter(accountId), cardFilter(cardNumber), customerId,
                        PageRequest.of(page, size, Sort.by("cardNum")))
                .map(CardResponse::from));
    }

    /** CICS CCDL / COCRDSLC: 9100-GETCARD-BYACCTCARD reads CARDDAT by card number. */
    @GetMapping("/{cardNumber}")
    public CardResponse get(@PathVariable @CardNumber String cardNumber) {
        return CardResponse.from(cardService.get(cardNumber));
    }

    /** CICS CCUP / COCRDUPC: edits 1230-1260, change check, READ UPDATE + REWRITE (9200). */
    @PutMapping("/{cardNumber}")
    public CardResponse update(@PathVariable @CardNumber String cardNumber,
                               @Valid @RequestBody CardUpdateRequest request) {
        return CardResponse.from(cardService.update(cardNumber, request));
    }

    /** CARDXREF lookup by card number (CBTRN02C 1500-A-LOOKUP-XREF). */
    @GetMapping("/{cardNumber}/xref")
    public CardXrefResponse xref(@PathVariable String cardNumber) {
        return CardXrefResponse.from(cardService.xrefByCard(cardNumber));
    }

    private static Long accountFilter(String accountId) {
        return accountId == null || isZeros(accountId) ? null : Long.valueOf(accountId);
    }

    private static String cardFilter(String cardNumber) {
        return cardNumber == null || isZeros(cardNumber) ? null : cardNumber;
    }

    private static boolean isZeros(String value) {
        return value.chars().allMatch(c -> c == '0');
    }
}
