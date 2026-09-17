package com.carddemo.transaction.api;

import com.carddemo.common.api.PageResponse;
import com.carddemo.transaction.api.dto.TransactionCreateRequest;
import com.carddemo.transaction.api.dto.TransactionResponse;
import com.carddemo.transaction.service.TransactionCommandService;
import com.carddemo.transaction.service.TransactionQueryService;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RestController;

/** CICS CT00/CT01/CT02 / COTRN00C, COTRN01C, COTRN02C. */
@RestController
@Validated
@RequestMapping("/api/v1/transactions")
public class TransactionController {

    private static final int MAX_PAGE_SIZE = 200;

    private final TransactionQueryService queryService;
    private final TransactionCommandService commandService;

    public TransactionController(TransactionQueryService queryService,
                                 TransactionCommandService commandService) {
        this.queryService = queryService;
        this.commandService = commandService;
    }

    @GetMapping
    public PageResponse<TransactionResponse> list(@RequestParam(required = false) String cardNumber,
                                                  @RequestParam(defaultValue = "0") @Min(0) int page,
                                                  @RequestParam(defaultValue = "20") @Min(1) @Max(MAX_PAGE_SIZE) int size) {
        return PageResponse.of(queryService
                .list(cardNumber, PageRequest.of(page, size, Sort.by("tranId")))
                .map(TransactionResponse::from));
    }

    @GetMapping("/{transactionId}")
    public TransactionResponse get(@PathVariable String transactionId) {
        return TransactionResponse.from(queryService.get(transactionId));
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public TransactionResponse add(@Valid @RequestBody TransactionCreateRequest request) {
        return TransactionResponse.from(commandService.add(request));
    }
}
