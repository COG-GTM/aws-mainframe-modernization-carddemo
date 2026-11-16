package com.carddemo.legacyio;

/** Picks the layout of a record from its raw bytes, for files that mix record types. */
@FunctionalInterface
public interface LayoutSelector {

    RecordLayout select(byte[] record, LegacyEncoding encoding);

    static LayoutSelector of(RecordLayout layout) {
        return (record, encoding) -> layout;
    }
}
