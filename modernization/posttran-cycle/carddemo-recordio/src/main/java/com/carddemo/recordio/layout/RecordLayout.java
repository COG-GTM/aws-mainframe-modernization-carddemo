package com.carddemo.recordio.layout;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;

/**
 * Two-way mapping between a copybook record image and an immutable Java value.
 *
 * @param <T> the Java value type
 */
public interface RecordLayout<T> {

    /** Record length in bytes (the copybook RECLN). */
    int length();

    T decode(FixedWidthRecord record);

    /**
     * Writes every named (non-FILLER) field of {@code value} into {@code target}. FILLER bytes are
     * left as they are, which is how a COBOL MOVE/REWRITE of a record area behaves.
     */
    void encodeInto(FixedWidthRecord target, T value);

    default FixedWidthRecord encode(T value, RecordEncoding encoding) {
        FixedWidthRecord record = FixedWidthRecord.blank(length(), encoding);
        encodeInto(record, value);
        return record;
    }

    default T decode(byte[] image, RecordEncoding encoding) {
        return decode(FixedWidthRecord.of(image, length(), encoding));
    }
}
