package com.carddemo.card;

import com.carddemo.common.web.KeysetPage;
import java.util.Map;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/cards")
public class CardController {

    private final CardService service;

    public CardController(CardService service) {
        this.service = service;
    }

    /** COCRDLIC (CCLI): 7 rows per page, optional account / card filters. */
    @GetMapping
    public KeysetPage<CardView> list(@RequestParam(required = false) String acctId,
                                     @RequestParam(required = false) String cardNum,
                                     @RequestParam(required = false) String after,
                                     @RequestParam(required = false) String before,
                                     @RequestParam(defaultValue = "1") int page) {
        return service.list(acctId, cardNum, after, before, page);
    }

    /** COCRDSLC (CCDL). */
    @GetMapping("/{cardNum}")
    public CardView view(@PathVariable String cardNum, @RequestParam(required = false) String acctId) {
        return service.view(cardNum, acctId);
    }

    /** COCRDUPC (CCUP). */
    @PutMapping("/{cardNum}")
    public Map<String, String> update(@PathVariable String cardNum, @RequestBody CardUpdateRequest request) {
        return Map.of("message", service.update(cardNum, request));
    }
}
