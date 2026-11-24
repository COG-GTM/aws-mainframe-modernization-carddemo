package com.carddemo.authfrds.domain.ims;

/** A non-blank DIBSTAT (IMS-RETURN-CODE) from a DL/I call. */
public class ImsStatusException extends RuntimeException {

    private final String status;

    public ImsStatusException(String status) {
        super("IMS status " + status);
        this.status = status;
    }

    public String status() {
        return status;
    }
}
