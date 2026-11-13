package com.carddemo.common.testing;

/** One field-level difference between an expected and an actual record (record numbers are one-based). */
public record RecordDifference(int recordNumber, String field, int offset, String expected, String actual) {

    @Override
    public String toString() {
        return String.format("record %d %s @%d: expected [%s] but was [%s]", recordNumber, field, offset, expected, actual);
    }
}
