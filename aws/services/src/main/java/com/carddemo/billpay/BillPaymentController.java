package com.carddemo.billpay;

import com.carddemo.billpay.BillPaymentService.BalanceView;
import com.carddemo.billpay.BillPaymentService.PaymentResult;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/bill-payments")
public class BillPaymentController {

    private final BillPaymentService service;

    public BillPaymentController(BillPaymentService service) {
        this.service = service;
    }

    public record BillPaymentRequest(String acctId) {
    }

    @GetMapping("/{acctId}")
    public BalanceView balance(@PathVariable String acctId) {
        return service.balance(acctId);
    }

    @PostMapping
    public ResponseEntity<PaymentResult> pay(@RequestBody(required = false) BillPaymentRequest request) {
        return ResponseEntity.status(HttpStatus.CREATED).body(service.pay(request == null ? null : request.acctId()));
    }
}
