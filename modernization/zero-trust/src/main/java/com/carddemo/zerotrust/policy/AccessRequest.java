package com.carddemo.zerotrust.policy;

import java.util.Map;

/**
 * One authorization question: subject, permission, and attributes of the resource being
 * accessed (e.g. {@code submittedBy} on a cutover approval, used by separation-of-duties).
 */
public record AccessRequest(Subject subject, String permission, Map<String, String> resourceAttributes) {

    public AccessRequest {
        resourceAttributes = Map.copyOf(resourceAttributes);
    }

    public static AccessRequest of(Subject subject, String permission) {
        return new AccessRequest(subject, permission, Map.of());
    }
}
