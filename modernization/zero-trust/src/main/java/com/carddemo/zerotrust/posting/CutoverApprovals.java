package com.carddemo.zerotrust.posting;

import com.carddemo.zerotrust.audit.AuditAction;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.policy.AccessRequest;
import com.carddemo.zerotrust.policy.Decision;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.Subject;

import java.util.HashMap;
import java.util.Map;

/**
 * Submit/approve workflow for releasing a posted run's outbound payment-processing
 * interface file to the downstream accounting system. Submission requires
 * {@code submit-cutover}; approval requires {@code approve-cutover} plus separation of
 * duties against the submitter recorded on the submission. Approving a run with no
 * submission is denied (reason {@code NO_MATCHING_ROLE} — there is nothing to approve),
 * a resubmission is denied ({@code ALREADY_SUBMITTED}), and a second approval is denied.
 * Every attempt is audited.
 *
 * <p>Ordering guarantee: the audit entry is appended <em>before</em> the state it describes
 * takes effect, inside the same synchronized section. If the append throws, the map is left
 * unchanged, so an unaudited submission or approval can never persist.
 */
public final class CutoverApprovals {

    private final PolicyDecisionPoint pdp;
    private HashChainedAuditLog audit;
    private final Map<String, Submission> submissions = new HashMap<>();

    public CutoverApprovals(PolicyDecisionPoint pdp, HashChainedAuditLog audit) {
        this.pdp = pdp;
        this.audit = audit;
    }

    public synchronized Decision submit(Subject subject, String runId, String controlTotals) {
        Decision decision = pdp.decide(AccessRequest.of(subject, "submit-cutover"));
        String detail = detail(decision) + " totals=" + controlTotals;
        Submission submission = null;
        if (decision.allowed()) {
            if (submissions.containsKey(runId)) {
                decision = Decision.deny("ALREADY_SUBMITTED");
                detail = "authz=DENY already submitted";
            } else {
                submission = new Submission(subject.id(), controlTotals, null);
            }
        }
        audit.append(subject.id(), subject.kind(), AuditAction.CUTOVER_SUBMITTED,
                decision.allowed() ? "ALLOW" : "DENY", runId, detail);
        if (submission != null) {
            submissions.put(runId, submission);
        }
        return decision;
    }

    public synchronized Decision approve(Subject approver, String runId) {
        Submission existing = submissions.get(runId);
        Decision decision;
        String detail;
        Submission next = existing;
        if (existing == null) {
            // No submission exists for this run: the policy cannot grant anything, so the
            // attempt is denied and audited as NO_MATCHING_ROLE (see class javadoc).
            decision = Decision.deny("NO_MATCHING_ROLE");
            detail = "authz=NO_MATCHING_ROLE no submission for run";
        } else if (existing.approvedBy() != null) {
            decision = Decision.deny("NO_MATCHING_ROLE");
            detail = "already approved by " + existing.approvedBy();
        } else {
            decision = pdp.decide(new AccessRequest(approver, "approve-cutover",
                    Map.of("submittedBy", existing.submittedBy())));
            detail = detail(decision) + " submittedBy=" + existing.submittedBy();
            if (decision.allowed()) {
                next = new Submission(existing.submittedBy(), existing.controlTotals(),
                        approver.id());
            }
        }
        audit.append(approver.id(), approver.kind(), AuditAction.CUTOVER_APPROVED,
                decision.allowed() ? "ALLOW" : "DENY", runId, detail);
        if (next != existing) {
            submissions.put(runId, next);
        }
        return decision;
    }

    private static String detail(Decision decision) {
        return decision.allowed()
                ? "authz=ALLOW role=" + decision.grantingRole().orElse("")
                : "authz=" + decision.reason();
    }

    /** Package-private, for tests that swap in a fresh log after a failure. */
    synchronized void swapAuditLog(HashChainedAuditLog audit) {
        this.audit = audit;
    }

    /** Package-visible submission state, for tests. */
    synchronized String approvedBy(String runId) {
        Submission s = submissions.get(runId);
        return s == null ? null : s.approvedBy();
    }

    private record Submission(String submittedBy, String controlTotals, String approvedBy) {
    }
}
