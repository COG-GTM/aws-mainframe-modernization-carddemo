package com.carddemo.authfrds.api;

import com.carddemo.authfrds.adapters.ims.InMemoryPendingAuthorizationStore;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.Map;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/** BR-AF-041: IMS stub test hooks. Only registered under the {@code testing} profile (off by default). */
@RestController
@Profile("testing")
@RequestMapping("/api/v1/ims-stub")
@Tag(name = "9. IMS STUB test hooks (profile 'testing' only)")
public class ImsStubTestController {

    private final InMemoryPendingAuthorizationStore ims;

    public ImsStubTestController(InMemoryPendingAuthorizationStore ims) {
        this.ims = ims;
    }

    @PostMapping("/fail-next-repl")
    @Operation(summary = "IMS STUB CONTROL (test hook, not legacy): make the next REPL return this DL/I status")
    public Map<String, String> failNextRepl(@RequestParam(defaultValue = "AI") String status) {
        ims.failNextReplace(status);
        return Map.of("nextReplStatus", status);
    }

    @PostMapping("/reset")
    @Operation(summary = "IMS STUB CONTROL (test hook, not legacy): reload the seed from the legacy IMS unload")
    public Map<String, Integer> reset() {
        ims.reset();
        return Map.of("accounts", ims.summaries().size());
    }
}
