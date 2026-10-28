package com.carddemo.zerotrust.policy;

import java.util.Optional;

/**
 * The policy decision point's answer. {@code reason} is a machine-readable code:
 * ALLOW, UNKNOWN_SUBJECT, NO_MATCHING_ROLE, UNKNOWN_PERMISSION,
 * SUBJECT_KIND_NOT_PERMITTED, or SEPARATION_OF_DUTIES.
 */
public record Decision(boolean allowed, String reason, Optional<String> grantingRole) {

    public static Decision allow(String grantingRole) {
        return new Decision(true, "ALLOW", Optional.of(grantingRole));
    }

    public static Decision deny(String reason) {
        return new Decision(false, reason, Optional.empty());
    }
}
