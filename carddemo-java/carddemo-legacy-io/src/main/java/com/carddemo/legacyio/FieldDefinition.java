package com.carddemo.legacyio;

import com.carddemo.legacyio.copybook.Picture;
import com.carddemo.legacyio.copybook.Usage;

/**
 * One elementary field of a {@link RecordLayout}, at a fixed position in the record.
 *
 * @param name data name; occurrences of an {@code OCCURS} item carry their 1-based subscripts, e.g.
 *     {@code EXP-CUST-ADDR-LINE(2)}; each unnamed {@code FILLER} is named {@code FILLER@<offset>}
 * @param offset byte offset in the record
 * @param length bytes the field occupies
 * @param picture the field's picture
 * @param usage the field's storage format
 */
public record FieldDefinition(String name, int offset, int length, Picture picture, Usage usage) {

    public boolean isNumeric() {
        return picture.category() == Picture.Category.NUMERIC;
    }

    public boolean isFiller() {
        return name.startsWith("FILLER@");
    }

    public int end() {
        return offset + length;
    }
}
