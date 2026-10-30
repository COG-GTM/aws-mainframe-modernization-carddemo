package com.carddemo.authfrds.api;

import com.carddemo.authfrds.adapters.ims.InMemoryPendingAuthorizationStore;
import com.carddemo.authfrds.domain.AuthKey;
import com.carddemo.authfrds.domain.AuthorizationDetail;
import com.carddemo.authfrds.domain.AuthorizationFraudService;
import com.carddemo.authfrds.domain.AuthorizationFraudService.ToggleResult;
import com.carddemo.authfrds.domain.ims.PendingAuthorizationSummary;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
@Tag(name = "2. Authorization view + PF5 fraud toggle (COPAUS1C, IMS STUB)")
public class AuthorizationController {

    public record AuthorizationView(String authKey, AuthorizationDetail authorization) {
    }

    private final AuthorizationFraudService service;
    private final InMemoryPendingAuthorizationStore ims;

    public AuthorizationController(AuthorizationFraudService service, InMemoryPendingAuthorizationStore ims) {
        this.service = service;
        this.ims = ims;
    }

    @GetMapping("/accounts")
    @Operation(summary = "IMS stub: accounts that have a PAUTSUM0 root segment")
    public List<PendingAuthorizationSummary> accounts() {
        return ims.summaries();
    }

    @GetMapping("/accounts/{acctId}/authorizations")
    @Operation(summary = "IMS stub: PAUTDTL1 children of the account in PAUT9CTS order")
    public List<AuthorizationView> list(@PathVariable long acctId) {
        return ims.listDetails(acctId).stream().map(d -> new AuthorizationView(d.key().toString(), d)).toList();
    }

    @GetMapping("/accounts/{acctId}/authorizations/{authKey}")
    @Operation(summary = "COPAUS1C ENTER: READ-AUTH-RECORD (GU PAUTSUM0, GNP PAUTDTL1 by key)")
    public AuthorizationView view(@PathVariable long acctId, @PathVariable String authKey) {
        AuthorizationDetail d = service.view(acctId, AuthKey.parse(authKey));
        return new AuthorizationView(d.key().toString(), d);
    }

    @GetMapping("/accounts/{acctId}/authorizations/{authKey}/next")
    @Operation(summary = "COPAUS1C PF8: next authorization; 404 'Already at the last Authorization...'")
    public AuthorizationView next(@PathVariable long acctId, @PathVariable String authKey) {
        AuthorizationDetail d = service.next(acctId, AuthKey.parse(authKey));
        return new AuthorizationView(d.key().toString(), d);
    }

    @PostMapping("/accounts/{acctId}/authorizations/{authKey}/fraud")
    @Operation(summary = "COPAUS1C PF5 MARK-AUTH-FRAUD: toggle F/R, LINK COPAUS2C (Db2), REPL IMS, SYNCPOINT or ROLLBACK")
    public ToggleResult toggle(@PathVariable long acctId, @PathVariable String authKey,
                               @RequestParam(required = false) Long custId) {
        return service.toggleFraud(acctId, custId, AuthKey.parse(authKey));
    }
}
