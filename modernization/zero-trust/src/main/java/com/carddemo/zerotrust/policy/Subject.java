package com.carddemo.zerotrust.policy;

import java.util.Optional;

/**
 * A caller presenting itself for an authorization decision. {@code userType} is the
 * CSUSR01Y SEC-USR-TYPE byte for human subjects; empty for workloads.
 */
public record Subject(String id, SubjectKind kind, Optional<String> userType) {

    public static Subject human(String id, String userType) {
        return new Subject(id, SubjectKind.HUMAN, Optional.of(userType));
    }

    public static Subject workload(String id) {
        return new Subject(id, SubjectKind.WORKLOAD, Optional.empty());
    }
}
