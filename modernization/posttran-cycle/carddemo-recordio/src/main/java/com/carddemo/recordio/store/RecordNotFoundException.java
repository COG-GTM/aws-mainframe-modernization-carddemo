package com.carddemo.recordio.store;

/** VSAM file status 23: READ/REWRITE of a key that is not in the KSDS. */
public class RecordNotFoundException extends RuntimeException {

    public RecordNotFoundException(String dataset, String key) {
        super("key '" + key + "' not found in " + dataset + " (file status 23)");
    }

    /** With the DISPLAY text the COBOL program emits before abending. */
    public RecordNotFoundException(String dataset, String key, String cobolMessage) {
        super(cobolMessage + ": key '" + key + "' not found in " + dataset + " (file status 23)");
    }
}
