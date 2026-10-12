package com.carddemo.zerotrust.policy;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * The parsed authorization policy. Role names map to the permissions they grant;
 * assignment maps resolve subjects to roles; constraints restrict how a permission
 * may be exercised.
 */
public record Policy(
        Set<String> permissions,
        Map<String, Set<String>> roles,
        Map<String, Set<String>> userTypes,
        Map<String, Set<String>> users,
        Map<String, Set<String>> workloads,
        Map<String, Constraint> constraints) {

    /** Attribute-level rule attached to a permission. */
    public record Constraint(Optional<SubjectKind> subjectKind, Optional<String> separationOfDutiesAttribute) {

        public static final Constraint NONE = new Constraint(Optional.empty(), Optional.empty());
    }
}
