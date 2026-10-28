package com.carddemo.zerotrust.posting;

import com.carddemo.zerotrust.audit.AuditChainVerifier;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.audit.VerificationResult;
import com.carddemo.zerotrust.policy.AccessRequest;
import com.carddemo.zerotrust.policy.Decision;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.PolicyLoader;
import com.carddemo.zerotrust.policy.Subject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Clock;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/** Separation of duties on the cutover submit → approve flow. */
class CutoverApprovalsTest {

    @TempDir
    Path dir;

    private static final Subject ADMIN001 = Subject.human("ADMIN001", "A");
    private static final Subject ADMIN002 = Subject.human("ADMIN002", "A");
    private static final Subject USER0001 = Subject.human("USER0001", "U");

    private CutoverApprovals approvals;

    private CutoverApprovals approvals(Path auditPath) {
        PolicyDecisionPoint pdp = new PolicyDecisionPoint(
                PolicyLoader.load(getClass().getResourceAsStream("/zero-trust-policy.yaml")));
        return new CutoverApprovals(pdp, new HashChainedAuditLog(auditPath, Clock.systemUTC()));
    }

    @Test
    void submitterCannotApproveOwnSubmission() throws Exception {
        Path log = dir.resolve("audit.jsonl");
        try (HashChainedAuditLog audit = new HashChainedAuditLog(log, Clock.systemUTC())) {
            PolicyDecisionPoint pdp = new PolicyDecisionPoint(
                    PolicyLoader.load(getClass().getResourceAsStream("/zero-trust-policy.yaml")));
            CutoverApprovals ca = new CutoverApprovals(pdp, audit);

            assertThat(ca.submit(ADMIN001, "run-1", "transactions=300 rejects=4").allowed()).isTrue();
            Decision own = ca.approve(ADMIN001, "run-1");
            assertThat(own.allowed()).isFalse();
            assertThat(own.reason()).isEqualTo("SEPARATION_OF_DUTIES");

            assertThat(ca.approve(ADMIN002, "run-1").allowed()).isTrue();
            // double approval is denied
            assertThat(ca.approve(ADMIN002, "run-1").allowed()).isFalse();
        }
        VerificationResult result = AuditChainVerifier.verify(log);
        assertThat(result.valid()).as(result.problem()).isTrue();
    }

    @Test
    void userRoleCannotSubmit() throws Exception {
        Path log = dir.resolve("audit2.jsonl");
        try (HashChainedAuditLog audit = new HashChainedAuditLog(log, Clock.systemUTC())) {
            PolicyDecisionPoint pdp = new PolicyDecisionPoint(
                    PolicyLoader.load(getClass().getResourceAsStream("/zero-trust-policy.yaml")));
            CutoverApprovals ca = new CutoverApprovals(pdp, audit);
            assertThat(ca.submit(USER0001, "run-2", "transactions=1").allowed()).isFalse();
        }
    }

    @Test
    void approvingWithoutSubmissionIsDenied() throws Exception {
        Path log = dir.resolve("audit3.jsonl");
        try (HashChainedAuditLog audit = new HashChainedAuditLog(log, Clock.systemUTC())) {
            PolicyDecisionPoint pdp = new PolicyDecisionPoint(
                    PolicyLoader.load(getClass().getResourceAsStream("/zero-trust-policy.yaml")));
            CutoverApprovals ca = new CutoverApprovals(pdp, audit);
            Decision d = ca.approve(ADMIN002, "no-such-run");
            assertThat(d.allowed()).isFalse();
        }
    }

    @Test
    void resubmissionAfterApprovalIsDeniedAndApprovalPreserved() throws Exception {
        Path log = dir.resolve("audit4.jsonl");
        try (HashChainedAuditLog audit = new HashChainedAuditLog(log, Clock.systemUTC())) {
            PolicyDecisionPoint pdp = new PolicyDecisionPoint(
                    PolicyLoader.load(getClass().getResourceAsStream("/zero-trust-policy.yaml")));
            CutoverApprovals ca = new CutoverApprovals(pdp, audit);
            ca.submit(ADMIN001, "run-9", "t=1");
            ca.approve(ADMIN002, "run-9");
            Decision resubmit = ca.submit(ADMIN002, "run-9", "t=1");
            assertThat(resubmit.allowed()).isFalse();
            assertThat(resubmit.reason()).isEqualTo("ALREADY_SUBMITTED");
            // the approval is preserved: the run stays approved
            assertThat(ca.approve(ADMIN002, "run-9").allowed()).isFalse();
        }
    }

    @Test
    void resubmissionOfPendingRunKeepsOriginalSubmitter() throws Exception {
        Path log = dir.resolve("audit5.jsonl");
        try (HashChainedAuditLog audit = new HashChainedAuditLog(log, Clock.systemUTC())) {
            PolicyDecisionPoint pdp = new PolicyDecisionPoint(
                    PolicyLoader.load(getClass().getResourceAsStream("/zero-trust-policy.yaml")));
            CutoverApprovals ca = new CutoverApprovals(pdp, audit);
            ca.submit(ADMIN001, "run-10", "t=1");
            Decision resubmit = ca.submit(ADMIN002, "run-10", "t=1");
            assertThat(resubmit.allowed()).isFalse();
            assertThat(resubmit.reason()).isEqualTo("ALREADY_SUBMITTED");
            // original submitter preserved: ADMIN001 still cannot approve (SoD)
            assertThat(ca.approve(ADMIN001, "run-10").reason()).isEqualTo("SEPARATION_OF_DUTIES");
            assertThat(ca.approve(ADMIN002, "run-10").allowed()).isTrue();
        }
    }

    @Test
    void failedSubmitAuditLeavesNoSubmission() throws Exception {
        Path log = dir.resolve("audit6.jsonl");
        HashChainedAuditLog audit = new HashChainedAuditLog(log, Clock.systemUTC());
        PolicyDecisionPoint pdp = new PolicyDecisionPoint(
                PolicyLoader.load(getClass().getResourceAsStream("/zero-trust-policy.yaml")));
        CutoverApprovals ca = new CutoverApprovals(pdp, audit);
        // Break the log: the append throws, and state must not change.
        audit.close();
        assertThatThrownBy(() -> ca.submit(ADMIN001, "run-11", "t=1"))
                .isInstanceOf(RuntimeException.class);
        // With a working log, the run still has no submission: approve is denied.
        try (HashChainedAuditLog audit2 = new HashChainedAuditLog(log, Clock.systemUTC())) {
            CutoverApprovals ca2 = new CutoverApprovals(pdp, audit2);
            Decision d = ca2.approve(ADMIN002, "run-11");
            assertThat(d.allowed()).isFalse();
            assertThat(d.reason()).isEqualTo("NO_MATCHING_ROLE");
        }
    }

    @Test
    void failedApproveAuditLeavesSubmissionUnapproved() throws Exception {
        Path log = dir.resolve("audit7.jsonl");
        HashChainedAuditLog audit = new HashChainedAuditLog(log, Clock.systemUTC());
        PolicyDecisionPoint pdp = new PolicyDecisionPoint(
                PolicyLoader.load(getClass().getResourceAsStream("/zero-trust-policy.yaml")));
        CutoverApprovals ca = new CutoverApprovals(pdp, audit);
        ca.submit(ADMIN001, "run-12", "t=1");
        audit.close();
        assertThatThrownBy(() -> ca.approve(ADMIN002, "run-12"))
                .isInstanceOf(RuntimeException.class);
        // Still unapproved: the same approver can retry and succeed on a working log.
        assertThat(ca.approvedBy("run-12")).isNull();
        try (HashChainedAuditLog audit2 = new HashChainedAuditLog(log, Clock.systemUTC())) {
            ca.swapAuditLog(audit2);
            assertThat(ca.approve(ADMIN002, "run-12").allowed()).isTrue();
            assertThat(ca.approvedBy("run-12")).isEqualTo("ADMIN002");
        }
    }
}
