package com.carddemo.zerotrust.policy;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Allow/deny matrix over every subject kind and permission. The README's access matrix
 * is generated from this same table — keep the two in sync.
 */
class PolicyDecisionPointTest {

    private static final PolicyDecisionPoint PDP = new PolicyDecisionPoint(
            PolicyLoader.load(PolicyDecisionPointTest.class
                    .getResourceAsStream("/zero-trust-policy.yaml")));

    private static final Subject ADMIN001 = Subject.human("ADMIN001", "A");
    private static final Subject USER0001 = Subject.human("USER0001", "U");
    private static final Subject USER0005 = Subject.human("USER0005", "U");
    private static final Subject UNKNOWN_TYPE = Subject.human("USER0009", "X");
    private static final Subject POST = Subject.workload("svc-posttran-post");
    private static final Subject REPORT = Subject.workload("svc-posttran-report");
    private static final Subject UNKNOWN_SVC = Subject.workload("svc-unknown");

    static Object[][] matrix() {
        return new Object[][]{
                {ADMIN001, "run-posting", false},
                {ADMIN001, "view-rejects", true},
                {ADMIN001, "submit-cutover", true},
                {ADMIN001, "approve-cutover", true},
                {ADMIN001, "read-audit", true},
                {USER0001, "run-posting", false},
                {USER0001, "view-rejects", true},
                {USER0001, "submit-cutover", false},
                {USER0001, "approve-cutover", false},
                {USER0001, "read-audit", false},
                {USER0005, "run-posting", false},
                {USER0005, "view-rejects", true},
                {USER0005, "submit-cutover", false},
                {USER0005, "approve-cutover", false},
                {USER0005, "read-audit", true},
                {UNKNOWN_TYPE, "run-posting", false},
                {UNKNOWN_TYPE, "view-rejects", false},
                {UNKNOWN_TYPE, "submit-cutover", false},
                {UNKNOWN_TYPE, "approve-cutover", false},
                {UNKNOWN_TYPE, "read-audit", false},
                {POST, "run-posting", true},
                {POST, "view-rejects", false},
                {POST, "submit-cutover", false},
                {POST, "approve-cutover", false},
                {POST, "read-audit", false},
                {REPORT, "run-posting", false},
                {REPORT, "view-rejects", true},
                {REPORT, "submit-cutover", false},
                {REPORT, "approve-cutover", false},
                {REPORT, "read-audit", false},
                {UNKNOWN_SVC, "run-posting", false},
                {UNKNOWN_SVC, "view-rejects", false},
                {UNKNOWN_SVC, "submit-cutover", false},
                {UNKNOWN_SVC, "approve-cutover", false},
                {UNKNOWN_SVC, "read-audit", false},
        };
    }

    @ParameterizedTest(name = "{0} {1} -> allowed={2}")
    @org.junit.jupiter.params.provider.MethodSource("matrix")
    void decidesMatrix(Subject subject, String permission, boolean allowed) {
        Decision decision = PDP.decide(AccessRequest.of(subject, permission));
        assertThat(decision.allowed()).as("%s %s", subject, permission).isEqualTo(allowed);
    }

    @Test
    void adminRunPostingDeniedByNoMatchingRole() {
        Decision decision = PDP.decide(AccessRequest.of(ADMIN001, "run-posting"));
        assertThat(decision.allowed()).isFalse();
        assertThat(decision.reason()).isIn("NO_MATCHING_ROLE", "SUBJECT_KIND_NOT_PERMITTED");
    }

    @Test
    void unknownPermissionIsDenied() {
        Decision decision = PDP.decide(AccessRequest.of(ADMIN001, "drop-tables"));
        assertThat(decision.allowed()).isFalse();
        assertThat(decision.reason()).isEqualTo("UNKNOWN_PERMISSION");
    }

    @Test
    void unknownSubjectsAreDeniedAsUnknown() {
        assertThat(PDP.decide(AccessRequest.of(UNKNOWN_SVC, "run-posting")).reason())
                .isEqualTo("UNKNOWN_SUBJECT");
        assertThat(PDP.decide(AccessRequest.of(UNKNOWN_TYPE, "view-rejects")).reason())
                .isEqualTo("UNKNOWN_SUBJECT");
    }

    @Test
    void loaderRejectsRoleGrantingUndefinedPermission() {
        String yaml = """
                permissions: [run-posting]
                roles:
                  bad-role: [fly]
                subjects: {userTypes: {}, users: {}, workloads: {}}
                constraints: {}
                """;
        assertThatThrownBy(() -> PolicyLoader.load(stream(yaml)))
                .isInstanceOf(PolicyLoader.PolicyException.class)
                .hasMessageContaining("fly");
    }

    @Test
    void loaderRejectsAssignmentNamingUndefinedRole() {
        String yaml = """
                permissions: [run-posting]
                roles:
                  posting-job: [run-posting]
                subjects:
                  userTypes: {}
                  users: {}
                  workloads:
                    svc-x: [no-such-role]
                constraints: {}
                """;
        assertThatThrownBy(() -> PolicyLoader.load(stream(yaml)))
                .isInstanceOf(PolicyLoader.PolicyException.class)
                .hasMessageContaining("no-such-role");
    }

    @Test
    void loaderRejectsConstraintOnUndefinedPermission() {
        String yaml = """
                permissions: [run-posting]
                roles:
                  posting-job: [run-posting]
                subjects: {userTypes: {}, users: {}, workloads: {}}
                constraints:
                  teleport: {subjectKind: HUMAN}
                """;
        assertThatThrownBy(() -> PolicyLoader.load(stream(yaml)))
                .isInstanceOf(PolicyLoader.PolicyException.class)
                .hasMessageContaining("teleport");
    }

    @Test
    void loaderRejectsConstraintWithUnknownSubjectKind() {
        String yaml = """
                permissions: [run-posting]
                roles:
                  posting-job: [run-posting]
                subjects: {userTypes: {}, users: {}, workloads: {}}
                constraints:
                  run-posting: {subjectKind: ROBOT}
                """;
        assertThatThrownBy(() -> PolicyLoader.load(stream(yaml)))
                .isInstanceOf(RuntimeException.class);
    }

    private static ByteArrayInputStream stream(String s) {
        return new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8));
    }

    @Test
    void cardDemoUserBecomesHumanSubject() {
        Subject s = new CardDemoUser("ADMIN001", "A").toSubject();
        assertThat(s.kind()).isEqualTo(SubjectKind.HUMAN);
        assertThat(s.userType()).isEqualTo(Optional.of("A"));
    }
}
