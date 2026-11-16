package com.carddemo.legacyio;

/**
 * An elementary item placed in a record.
 *
 * @param name   data name with OCCURS subscripts, e.g. {@code EXP-CUST-ADDR-LINE(2)}; {@code FILLER} for unnamed items
 * @param offset zero-based byte offset in the record
 * @param length bytes the item occupies
 */
public record FieldDefinition(String name, int offset, int length, Picture picture, Usage usage) {

    public boolean isNumeric() {
        return picture.isNumeric();
    }

    public boolean isFiller() {
        return name.equals("FILLER");
    }
}
