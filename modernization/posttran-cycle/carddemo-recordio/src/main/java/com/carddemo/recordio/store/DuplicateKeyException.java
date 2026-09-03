package com.carddemo.recordio.store;

/** VSAM file status 22: WRITE of a key that already exists in a KSDS. */
public class DuplicateKeyException extends RuntimeException {

    public DuplicateKeyException(String dataset, String key) {
        super("duplicate key '" + key + "' in " + dataset + " (file status 22)");
    }
}
