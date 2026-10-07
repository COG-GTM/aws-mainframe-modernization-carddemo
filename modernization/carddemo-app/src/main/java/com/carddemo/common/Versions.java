package com.carddemo.common;

import org.springframework.orm.ObjectOptimisticLockingFailureException;

/** Explicit check of the client-supplied version against the freshly loaded entity (ADR-0010). */
public final class Versions {

    private Versions() {
    }

    /**
     * Fails with the same exception JPA raises at flush (409 {@code CHANGED}) when the version the client read
     * differs from the current one. Call it inside the update transaction, after the load and before any change.
     */
    public static void requireCurrent(Class<?> entity, Object id, long suppliedVersion, long currentVersion) {
        if (suppliedVersion != currentVersion) {
            throw new ObjectOptimisticLockingFailureException(entity, id);
        }
    }
}
