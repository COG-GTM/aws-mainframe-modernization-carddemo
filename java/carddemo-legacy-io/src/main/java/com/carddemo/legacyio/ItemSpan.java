package com.carddemo.legacyio;

/** Byte range of any item (group or elementary) in a record, e.g. a VSAM key group such as {@code TRAN-CAT-KEY}. */
public record ItemSpan(String name, int offset, int length) {
}
