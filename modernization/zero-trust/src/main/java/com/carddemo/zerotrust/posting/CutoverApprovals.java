package com.carddemo.zerotrust.posting;

import com.carddemo.zerotrust.audit.AuditAction;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.policy.AccessRequest;
import com.carddemo.zerotrust.policy.Decision;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.Subject;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Submit/approve workflow for releasing a posted run's outbound payment-processing
 * interface file to the downstream accounting system. Submission requires
 * {@code submit-cutover}; approval requires {@code approve-cutover} plus separation of
 * duties against the submitter recorded on the submission. Approving a run with no
 * submission is denied (reason {@code NO_MATCHING_ROLE} — there is nothing to approve),
 * and a second approval is denied. Every attempt is audited.
 */
public final class CutoverApprovals {

    private final PolicyDecisionPoint pdp;
    private final HashChainedAuditLog audit;
    private final Map<String, Submission> submissions = new ConcurrentHashMap<>();

    public CutoverApprovals(PolicyDecisionPoint pdp, HashChainedAuditLog audit) {
        this.pdp = pdp;
        this.audit = audit;
    }

    public Decision submit(Subject subject, String runId, String controlTotals) {
        Decision decision = pdp.decide(AccessRequest.of(subject, "submit-cutover"));
        String detail = detail(decision) + " totals=" + controlTotals;
        if (decision.allowed()) {
            // Atomic: a run that already has a submission (pending or approved) cannot be
            // resubmitted — the original submitter is preserved.
            if (submissions.putIfAbsent(runId,
                    new Submission(subject.id(), controlTotals, null)) != null) {
                decision = Decision.deny("ALREADY_SUBMITTED");
                detail = "authz=DENY already submitted";
            }
        }
        audit.append(subject.id(), subject.kind(), AuditAction.CUTOVER_SUBMITTED,
                decision.allowed() ? "ALLOW" : "DENY", runId, detail);
        return decision;
    }

    public Decision approve(Subject approver, String runId) {
        // The submission lookup, the policy decision, and recording the approval are one
        // atomic compute, so a racing submit or second approve cannot slip between them.
        var outcome = new java.util.concurrent.atomic.AtomicReference<Decision>();
        var auditDetail = new java.util.concurrent.atomic.AtomicReference<String>();
        submissions.compute(runId, (id, existing) -> {
            if (existing == null) {
                // No submission exists for this run: the policy cannot grant anything, so
                // the attempt is denied and audited as NO_MATCHING_ROLE (see javadoc).
                outcome.set(Decision.deny("NO_MATCHING_ROLE"));
                auditDetail.set("authz=NO_MATCHING_ROLE no submission for run");
                return null;
            }
            if (existing.approvedBy() != null) {
                outcome.set(Decision.deny("NO_MATCHING_ROLE"));
                auditDetail.set("already approved by " + existing.approvedBy());
                return existing;
            }
            Decision decision = pdp.decide(new AccessRequest(approver, "approve-cutover",
                    Map.of("submittedBy", existing.submittedBy())));
            outcome.set(decision);
            auditDetail.set(detail(decision) + " submittedBy=" + existing.submittedBy());
            return decision.allowed()
                    ? new Submission(existing.submittedBy(), existing.controlTotals(), approver.id())
                    : existing;
        });
        Decision decision = outcome.get();
        audit.append(approver.id(), approver.kind(), AuditAction.CUTOVER_APPROVED,
                decision.allowed() ? "ALLOW" : "DENY", runId, auditDetail.get());
        return decision;
    }

    private static String detail(Decision decision) {
        return decision.allowed()
                ? "authz=ALLOW role=" + decision.grantingRole().orElse("")
                : "authz=" + decision.reason();
    }

    private record Submission(String submittedBy, String controlTotals, String approvedBy) {
    }
}
