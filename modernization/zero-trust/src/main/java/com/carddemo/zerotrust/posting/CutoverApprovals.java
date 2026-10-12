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
        audit.append(subject.id(), subject.kind(), AuditAction.CUTOVER_SUBMITTED,
                decision.allowed() ? "ALLOW" : "DENY", runId,
                detail(decision) + " totals=" + controlTotals);
        if (decision.allowed()) {
            submissions.put(runId, new Submission(subject.id(), controlTotals, null));
        }
        return decision;
    }

    public Decision approve(Subject approver, String runId) {
        Submission submission = submissions.get(runId);
        if (submission == null) {
            // No submission exists for this run: the policy cannot grant anything, so the
            // attempt is denied and audited as NO_MATCHING_ROLE (see class javadoc).
            audit.append(approver.id(), approver.kind(), AuditAction.CUTOVER_APPROVED,
                    "DENY", runId, "authz=NO_MATCHING_ROLE no submission for run");
            return Decision.deny("NO_MATCHING_ROLE");
        }
        if (submission.approvedBy() != null) {
            audit.append(approver.id(), approver.kind(), AuditAction.CUTOVER_APPROVED,
                    "DENY", runId, "already approved by " + submission.approvedBy());
            return Decision.deny("NO_MATCHING_ROLE");
        }
        Decision decision = pdp.decide(new AccessRequest(approver, "approve-cutover",
                Map.of("submittedBy", submission.submittedBy())));
        audit.append(approver.id(), approver.kind(), AuditAction.CUTOVER_APPROVED,
                decision.allowed() ? "ALLOW" : "DENY", runId,
                detail(decision) + " submittedBy=" + submission.submittedBy());
        if (decision.allowed()) {
            submissions.put(runId, new Submission(submission.submittedBy(),
                    submission.controlTotals(), approver.id()));
        }
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
