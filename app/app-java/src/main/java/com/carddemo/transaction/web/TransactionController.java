package com.carddemo.transaction.web;

import com.carddemo.transaction.web.TransactionDtos.AddTransactionRequest;
import com.carddemo.transaction.web.TransactionDtos.AddTransactionResponse;
import com.carddemo.transaction.web.TransactionDtos.TransactionDetail;
import com.carddemo.transaction.web.TransactionDtos.TransactionPage;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * REST replacement for the CICS transactions CT00 (COTRN00C), CT01 (COTRN01C) and CT02 (COTRN02C).
 */
@RestController
@RequestMapping("/api/transactions")
public class TransactionController {

    private final TransactionInquiryService inquiry;
    private final TransactionAddService add;

    public TransactionController(TransactionInquiryService inquiry, TransactionAddService add) {
        this.inquiry = inquiry;
        this.add = add;
    }

    /**
     * COTRN00C. {@code tranId} = ENTER with TRNIDIN, {@code after} = PF8 (last id on screen),
     * {@code before} = PF7 (first id on screen).
     */
    @GetMapping
    public TransactionPage list(@RequestParam(required = false) String tranId,
                                @RequestParam(required = false) String after,
                                @RequestParam(required = false) String before) {
        if (after != null) {
            return inquiry.nextPage(after);
        }
        if (before != null) {
            return inquiry.previousPage(before);
        }
        return inquiry.firstPage(tranId);
    }

    /** COTRN02C PF5 source: the most recent transaction. */
    @GetMapping("/last")
    public TransactionDetail last() {
        return inquiry.last();
    }

    /** COTRN01C. */
    @GetMapping("/{tranId}")
    public TransactionDetail view(@PathVariable String tranId) {
        return inquiry.view(tranId);
    }

    /** COTRN02C. */
    @PostMapping
    public ResponseEntity<AddTransactionResponse> add(@RequestBody AddTransactionRequest request) {
        return ResponseEntity.status(HttpStatus.CREATED).body(add.processEnterKey(request));
    }
}
