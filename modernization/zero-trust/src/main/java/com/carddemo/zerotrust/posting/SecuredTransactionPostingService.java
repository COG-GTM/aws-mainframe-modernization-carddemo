package com.carddemo.zerotrust.posting;

import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.PostingOutcome;
import com.carddemo.posting.domain.TransactionPostingService;
import com.carddemo.zerotrust.audit.AuditAction;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.identity.AccessTokenVerifier;
import com.carddemo.zerotrust.identity.TokenRejectedException;
import com.carddemo.zerotrust.identity.VerifiedWorkload;
import com.carddemo.zerotrust.masking.DataMasker;
import com.carddemo.zerotrust.policy.AccessRequest;
import com.carddemo.zerotrust.policy.Decision;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.Subject;
import com.carddemo.zerotrust.policy.SubjectKind;

import java.util.function.Function;

/**
 * Composition wrapper around the posting service: every call verifies the access token,
 * authorizes {@code run-posting} against the policy decision point, and only then invokes
 * the delegate. On any denial the delegate is never reached, so the ledger is untouched,
 * and the denial is audited.
 *
 * <p>The per-item ALLOW is not logged as a separate AUTHZ_DECISION entry; it is folded
 * into the TRANSACTION_POSTED / TRANSACTION_REJECTED record ({@code authz=ALLOW role=…
 * jti=…}) so the audit chain gains one entry per transaction, not two.
 */
public final class SecuredTransactionPostingService {

    public static final String PERMISSION = "run-posting";

    private final AccessTokenVerifier verifier;
    private final PolicyDecisionPoint pdp;
    private final HashChainedAuditLog audit;
    private final DataMasker masker;

    public SecuredTransactionPostingService(AccessTokenVerifier verifier, PolicyDecisionPoint pdp,
                                            HashChainedAuditLog audit, DataMasker masker) {
        this.verifier = verifier;
        this.pdp = pdp;
        this.audit = audit;
        this.masker = masker;
    }

    /**
     * Verify the token and authorize {@code run-posting} for its workload. Returns the
     * verified workload and granting role on success; on failure the denial is audited
     * and {@link AccessDeniedException} is thrown.
     */
    public VerifiedCaller authorize(String accessToken, String recordKey) {
        VerifiedWorkload workload;
        try {
            workload = verifier.verify(accessToken);
        } catch (TokenRejectedException e) {
            deny("unknown", recordKey, "token rejected: " + e.getMessage());
            throw new AccessDeniedException("token rejected: " + e.getMessage());
        }
        Decision decision = pdp.decide(AccessRequest.of(
                Subject.workload(workload.id()), PERMISSION));
        if (!decision.allowed()) {
            deny(workload.id(), recordKey, "authz=" + decision.reason()
                    + " jti=" + workload.jti());
            throw new AccessDeniedException("authorization denied: " + decision.reason());
        }
        return new VerifiedCaller(workload, decision.grantingRole().orElse(""));
    }

    /** Audit the outcome of a call that was allowed and ran. */
    public void auditOutcome(VerifiedCaller caller, PostingOutcome outcome, String recordKey) {
        String authz = "authz=ALLOW role=" + caller.grantingRole()
                + " jti=" + caller.workload().jti();
        switch (outcome) {
            case PostingOutcome.Posted p -> audit.append(caller.workload().id(),
                    SubjectKind.WORKLOAD, AuditAction.TRANSACTION_POSTED, "POSTED", recordKey,
                    "card=" + masker.maskPan(p.transaction().cardNumber())
                            + " amount=" + p.transaction().amount().toPlainString() + " " + authz);
            case PostingOutcome.Rejected r -> audit.append(caller.workload().id(),
                    SubjectKind.WORKLOAD, AuditAction.TRANSACTION_REJECTED, "REJECTED", recordKey,
                    "code=" + r.reason().code() + " text=" + r.reason().description()
                            + " card=" + masker.maskPan(r.transaction().cardNumber())
                            + " amount=" + r.transaction().amount().toPlainString() + " " + authz);
        }
    }

    /** Token + authorization + delegate + audit, on every call. */
    public PostingOutcome process(String accessToken, DailyTransaction item,
                                  Function<DailyTransaction, PostingOutcome> delegate) {
        String recordKey = item.transaction().id();
        VerifiedCaller caller = authorize(accessToken, recordKey);
        PostingOutcome outcome = delegate.apply(item);
        auditOutcome(caller, outcome, recordKey);
        return outcome;
    }

    public PostingOutcome process(String accessToken, DailyTransaction item,
                                  TransactionPostingService delegate) {
        return process(accessToken, item, delegate::process);
    }

    private void deny(String actor, String recordKey, String detail) {
        audit.append(actor, SubjectKind.WORKLOAD, AuditAction.AUTHZ_DECISION, "DENY",
                recordKey, detail);
    }

    /** A workload that passed token verification and authorization. */
    public record VerifiedCaller(VerifiedWorkload workload, String grantingRole) {
    }
}
