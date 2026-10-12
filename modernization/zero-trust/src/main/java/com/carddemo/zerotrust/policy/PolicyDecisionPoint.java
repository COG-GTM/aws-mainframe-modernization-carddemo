package com.carddemo.zerotrust.policy;

import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

/**
 * Pure, deterministic policy evaluation. Deny by default: a request is allowed only when
 * an assigned role grants the permission and every attribute constraint on it is met.
 */
public final class PolicyDecisionPoint {

    private final Policy policy;

    public PolicyDecisionPoint(Policy policy) {
        this.policy = policy;
    }

    public Decision decide(AccessRequest request) {
        if (!policy.permissions().contains(request.permission())) {
            return Decision.deny("UNKNOWN_PERMISSION");
        }
        Set<String> roles = assignedRoles(request.subject());
        if (roles.isEmpty()) {
            return Decision.deny("UNKNOWN_SUBJECT");
        }
        boolean kindFailed = false;
        boolean sodFailed = false;
        for (String role : roles) {
            if (!policy.roles().getOrDefault(role, Set.of()).contains(request.permission())) {
                continue;
            }
            Policy.Constraint constraint = policy.constraints()
                    .getOrDefault(request.permission(), Policy.Constraint.NONE);
            if (constraint.subjectKind().isPresent()
                    && constraint.subjectKind().get() != request.subject().kind()) {
                kindFailed = true;
                continue;
            }
            if (constraint.separationOfDutiesAttribute().isPresent()) {
                String attr = constraint.separationOfDutiesAttribute().get();
                if (request.subject().id().equals(request.resourceAttributes().get(attr))) {
                    sodFailed = true;
                    continue;
                }
            }
            return Decision.allow(role);
        }
        if (sodFailed && !kindFailed) {
            return Decision.deny("SEPARATION_OF_DUTIES");
        }
        if (kindFailed) {
            return Decision.deny("SUBJECT_KIND_NOT_PERMITTED");
        }
        return Decision.deny("NO_MATCHING_ROLE");
    }

    private Set<String> assignedRoles(Subject subject) {
        Set<String> roles = new LinkedHashSet<>();
        switch (subject.kind()) {
            case HUMAN -> {
                subject.userType()
                        .map(type -> policy.userTypes().getOrDefault(type, Set.of()))
                        .ifPresent(roles::addAll);
                roles.addAll(policy.users().getOrDefault(subject.id(), Set.of()));
            }
            case WORKLOAD -> roles.addAll(policy.workloads().getOrDefault(subject.id(), Set.of()));
        }
        return roles;
    }

    public Policy policy() {
        return policy;
    }

    /** Convenience for callers that want the permission id as a first-class value. */
    public record Permission(String id) {
    }
}
