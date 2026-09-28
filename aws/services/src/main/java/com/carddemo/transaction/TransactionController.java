package com.carddemo.transaction;

import com.carddemo.common.PageResponse;
import com.carddemo.transaction.TransactionDtos.CreateTransactionRequest;
import com.carddemo.transaction.TransactionDtos.CreateTransactionResponse;
import com.carddemo.transaction.TransactionDtos.TransactionDetail;
import com.carddemo.transaction.TransactionDtos.TransactionSummary;
import java.net.URI;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/transactions")
public class TransactionController {

    private final TransactionService service;

    public TransactionController(TransactionService service) {
        this.service = service;
    }

    @GetMapping
    public PageResponse<TransactionSummary> list(@RequestParam(required = false) String startKey,
            @RequestParam(required = false) String direction, @RequestParam(required = false) Integer pageSize) {
        return service.list(startKey, direction, pageSize);
    }

    @GetMapping("/{tranId}")
    public TransactionDetail detail(@PathVariable String tranId) {
        return service.detail(tranId);
    }

    @PostMapping
    public ResponseEntity<CreateTransactionResponse> create(
            @RequestBody(required = false) CreateTransactionRequest request) {
        CreateTransactionResponse created = service.create(request);
        return ResponseEntity.created(URI.create("/api/v1/transactions/" + created.tranId())).body(created);
    }
}
