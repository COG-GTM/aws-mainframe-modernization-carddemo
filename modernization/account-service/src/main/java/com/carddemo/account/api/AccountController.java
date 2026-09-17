package com.carddemo.account.api;

import com.carddemo.account.api.dto.AccountResponse;
import com.carddemo.account.api.dto.AccountUpdateRequest;
import com.carddemo.account.api.dto.InterestSettlementRequest;
import com.carddemo.account.api.dto.PostingRequest;
import com.carddemo.account.api.dto.PostingResponse;
import com.carddemo.account.service.AccountService;
import com.carddemo.common.api.PageResponse;
import jakarta.validation.Valid;
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
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/accounts")
public class AccountController {

    private final AccountService accountService;

    public AccountController(AccountService accountService) {
        this.accountService = accountService;
    }

    /** CICS CAVW / COACTVWC. */
    @GetMapping("/{accountId}")
    public AccountResponse get(@PathVariable long accountId) {
        return AccountResponse.from(accountService.get(accountId));
    }

    /** Replaces the READACCT batch dump (CBACT01C) and the account browse of the admin screens. */
    @GetMapping
    public PageResponse<AccountResponse> list(@RequestParam(defaultValue = "0") int page,
                                              @RequestParam(defaultValue = "20") int size) {
        return PageResponse.of(accountService.list(PageRequest.of(page, size, Sort.by("acctId")))
                .map(AccountResponse::from));
    }

    /** CICS CAUP / COACTUPC. */
    @PutMapping("/{accountId}")
    public AccountResponse update(@PathVariable long accountId, @Valid @RequestBody AccountUpdateRequest request) {
        return AccountResponse.from(accountService.update(accountId, request));
    }

    /** Account side of CBTRN02C, called by the posting batch job of transaction-service. */
    @PostMapping("/{accountId}/postings")
    public ResponseEntity<PostingResponse> post(@PathVariable long accountId,
                                                @Valid @RequestBody PostingRequest request) {
        PostingResponse response = PostingResponse.from(
                accountService.post(accountId, request.amount(), request.transactionDate()));
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
