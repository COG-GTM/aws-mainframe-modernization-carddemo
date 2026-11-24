package com.carddemo.account;

import java.util.Map;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/accounts")
public class AccountController {

    private final AccountService service;

    public AccountController(AccountService service) {
        this.service = service;
    }

    /** COACTVWC (CAVW). */
    @GetMapping("/{acctId}")
    public AccountDetails view(@PathVariable String acctId) {
        return service.view(acctId);
    }

    /** COACTUPC (CAUP). */
    @PutMapping("/{acctId}")
    public Map<String, String> update(@PathVariable String acctId, @RequestBody AccountUpdateRequest request) {
        return Map.of("message", service.update(acctId, request));
    }
}
