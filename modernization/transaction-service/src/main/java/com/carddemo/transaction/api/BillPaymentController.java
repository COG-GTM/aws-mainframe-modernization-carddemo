package com.carddemo.transaction.api;

import com.carddemo.transaction.api.dto.BillPaymentRequest;
import com.carddemo.transaction.api.dto.TransactionResponse;
import com.carddemo.transaction.service.TransactionCommandService;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

/** CICS CB00 / COBIL00C. */
@RestController
@RequestMapping("/api/v1/bill-payments")
public class BillPaymentController {

    private final TransactionCommandService commandService;

    public BillPaymentController(TransactionCommandService commandService) {
        this.commandService = commandService;
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public TransactionResponse pay(@Valid @RequestBody BillPaymentRequest request) {
        return TransactionResponse.from(commandService.payBill(request.accountId()));
    }
}
