package com.carddemo.common.error;

/**
 * Invalid bytes in a numeric field (a bad digit, zone or sign nibble): what z/OS reports as a data
 * exception (S0C7). It abends; the field is never read as zero.
 */
public class NumericDataException extends AbendException {

    public NumericDataException(String message) {
        super(message);
    }
}
