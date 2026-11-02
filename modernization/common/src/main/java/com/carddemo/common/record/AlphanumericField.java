package com.carddemo.common.record;

/** A {@code PIC X(n)} item or a group item read as alphanumeric. */
public final class AlphanumericField extends Field {

    AlphanumericField(String copybook, String name, String picture, int offset, int length, String view) {
        super(copybook, name, picture, offset, length, view);
    }
}
