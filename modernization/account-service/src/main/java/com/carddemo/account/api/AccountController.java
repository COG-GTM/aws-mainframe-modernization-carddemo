package com.carddemo.account.api;

import com.carddemo.account.api.dto.AccountResponse;
import com.carddemo.account.api.dto.AccountUpdateRequest;
import com.carddemo.account.api.dto.AccountViewResponse;
import com.carddemo.account.api.dto.InterestSettlementRequest;
import com.carddemo.account.api.dto.PostingRequest;
import com.carddemo.account.api.dto.PostingResponse;
import com.carddemo.account.service.AccountService;
import com.carddemo.account.service.AccountViewService;
import com.carddemo.common.api.PageResponse;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Validated
@RequestMapping("/api/v1/accounts")
public class AccountController {

    private static final int MAX_PAGE_SIZE = 200;

    private final AccountService accountService;
    private final AccountViewService accountViewService;

    public AccountController(AccountService accountService, AccountViewService accountViewService) {
        this.accountService = accountService;
        this.accountViewService = accountViewService;
    }

    /** The ACCTDAT record alone, read by transaction-service and the batch jobs. */
    @GetMapping("/{accountId}")
    public AccountResponse get(@PathVariable long accountId) {
        return AccountResponse.from(accountService.get(accountId));
    }

    /** Replaces the READACCT batch dump (CBACT01C) and the account browse of the admin screens. */
    @GetMapping
    public PageResponse<AccountResponse> list(@RequestParam(defaultValue = "0") @Min(0) int page,
                                              @RequestParam(defaultValue = "20") @Min(1) @Max(MAX_PAGE_SIZE) int size) {
        return PageResponse.of(accountService.list(PageRequest.of(page, size, Sort.by("acctId")))
                .map(AccountResponse::from));
    }

    /** CICS CAVW / COACTVWC: account, cross reference and customer as one screen. */
    @GetMapping("/{accountId}/view")
    public AccountViewResponse view(@PathVariable String accountId) {
        return accountViewService.view(accountId);
    }

    /** CICS CAUP / COACTUPC: edits, then rewrites the account and its customer. */
    @PutMapping("/{accountId}")
    public AccountViewResponse update(@PathVariable String accountId,
                                      @Valid @RequestBody AccountUpdateRequest request) {
        return accountViewService.update(accountId, request);
    }

    /**
     * Account side of CBTRN02C, called by the posting batch job of transaction-service. Idempotent
     * per {@code transactionId}: a retry returns the first result; the same id with a different
     * account, amount or date is 409.
     */
    @PostMapping("/{accountId}/postings")
    public ResponseEntity<PostingResponse> post(@PathVariable long accountId,
                                                @Valid @RequestBody PostingRequest request) {
        PostingResponse response = PostingResponse.from(
                accountService.post(accountId, request.transactionId(), request.amount(),
                        request.transactionDate(), request.typeOrDefault()));
        return response.posted()
                ? ResponseEntity.ok(response)
                : ResponseEntity.unprocessableEntity().body(response);
    }

    /** Account break of CBACT04C, called by the interest batch job of transaction-service. */
    @PostMapping("/{accountId}/interest-settlements")
    public AccountResponse settleInterest(@PathVariable long accountId,
                                          @Valid @RequestBody InterestSettlementRequest request) {
        return AccountResponse.from(accountService.settleInterest(accountId, request.totalInterest()));
    }
}
