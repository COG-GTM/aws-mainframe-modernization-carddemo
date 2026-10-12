package com.carddemo.zerotrust.posting;

/** Authorization or token verification failed; the protected operation did not run. */
public class AccessDeniedException extends RuntimeException {

    public AccessDeniedException(String reason) {
        super(reason);
    }
}
