package com.carddemo.zerotrust.posting;

import com.carddemo.zerotrust.audit.AuditAction;
import com.carddemo.zerotrust.audit.AuditEntry;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.policy.AccessRequest;
import com.carddemo.zerotrust.policy.Decision;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.Subject;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Read-only access to the audit log, gated by {@code read-audit}. Every read — allowed
 * or denied — is itself audited. There is no mutation API here by design.
 */
public final class AuditReader {

    private final PolicyDecisionPoint pdp;
    private final HashChainedAuditLog audit;
    private final Path logPath;

    public AuditReader(PolicyDecisionPoint pdp, HashChainedAuditLog audit, Path logPath) {
        this.pdp = pdp;
        this.audit = audit;
        this.logPath = logPath;
    }

    public List<AuditEntry> read(Subject subject) {
        Decision decision = pdp.decide(AccessRequest.of(subject, "read-audit"));
        audit.append(subject.id(), subject.kind(), AuditAction.AUDIT_READ,
                decision.allowed() ? "ALLOW" : "DENY", logPath.getFileName().toString(),
                decision.allowed()
                        ? "authz=ALLOW role=" + decision.grantingRole().orElse("")
                        : "authz=" + decision.reason());
        if (!decision.allowed()) {
            throw new AccessDeniedException("authorization denied: " + decision.reason());
        }
        try {
            return Files.readAllLines(logPath).stream()
                    .filter(l -> !l.isBlank())
                    .map(AuditEntry::fromJson)
                    .toList();
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read audit log " + logPath, e);
        }
    }
}
