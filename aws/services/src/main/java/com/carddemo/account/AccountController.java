package com.carddemo.account;

import com.carddemo.account.AccountDtos.AccountUpdateRequest;
import com.carddemo.account.AccountDtos.AccountView;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/accounts")
public class AccountController {

    private final AccountService service;

    public AccountController(AccountService service) {
        this.service = service;
    }

    @GetMapping("/{acctId}")
    public AccountView view(@PathVariable String acctId) {
        return service.view(acctId);
    }

    @PutMapping("/{acctId}")
    public AccountView update(@PathVariable String acctId,
            @RequestBody(required = false) AccountUpdateRequest request) {
        return service.update(acctId, request);
    }
}
