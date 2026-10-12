package com.carddemo.recordio.codec;

import java.math.BigDecimal;
import java.util.Arrays;

/**
 * A mutable fixed-length record image (the Java analogue of a COBOL FD record area).
 *
 * <p>Field accessors take byte offsets/lengths taken directly from the copybook. Setters overwrite
 * only the addressed bytes, so bytes the program never touches (FILLER, unread fields) round-trip
 * verbatim, exactly as a COBOL REWRITE of the record area would emit them.
 */
public final class FixedWidthRecord {

    private final byte[] image;
    private final RecordEncoding encoding;

    public FixedWidthRecord(byte[] image, RecordEncoding encoding) {
        this.image = image;
        this.encoding = encoding;
    }

    public static FixedWidthRecord blank(int length, RecordEncoding encoding) {
        byte[] bytes = new byte[length];
        Arrays.fill(bytes, encoding.space());
        return new FixedWidthRecord(bytes, encoding);
    }

    public static FixedWidthRecord of(byte[] image, int expectedLength, RecordEncoding encoding) {
        if (image.length != expectedLength) {
            throw new RecordFormatException(
                    "record length " + image.length + " does not match copybook length " + expectedLength);
        }
        return new FixedWidthRecord(image, encoding);
    }

    public int length() {
        return image.length;
    }

    public RecordEncoding encoding() {
        return encoding;
    }

    public byte[] bytes() {
        return image.clone();
    }

    public FixedWidthRecord copy() {
        return new FixedWidthRecord(image.clone(), encoding);
    }

    /** MOVE of a group item: copies {@code source}'s bytes verbatim into this record at {@code offset}. */
    public void setBytes(int offset, FixedWidthRecord source) {
        System.arraycopy(source.image, 0, image, offset, source.image.length);
    }

    /** PIC X(n): raw text, trailing spaces preserved (COBOL semantics), no trimming. */
    public String text(int offset, int length) {
        return new String(image, offset, length, encoding.charset());
    }

    /** PIC X(n) with trailing spaces removed, for display-oriented fields. */
    public String trimmedText(int offset, int length) {
        return text(offset, length).stripTrailing();
    }

    /** MOVE alphanumeric: left-justified, space-padded, right-truncated. */
    public void setText(int offset, int length, String value) {
        byte[] bytes = value.getBytes(encoding.charset());
        Arrays.fill(image, offset, offset + length, encoding.space());
        System.arraycopy(bytes, 0, image, offset, Math.min(bytes.length, length));
    }

    /** PIC [S]9(n)[V9(m)] USAGE DISPLAY. */
    public BigDecimal zoned(int offset, int digits, int scale, boolean signed) {
        return CobolNumeric.decodeZoned(text(offset, digits), scale, signed);
    }

    public void setZoned(int offset, int digits, int scale, boolean signed, BigDecimal value) {
        String encoded = CobolNumeric.encodeZoned(value, digits, scale, signed);
        System.arraycopy(encoded.getBytes(encoding.charset()), 0, image, offset, digits);
    }

    /** PIC 9(n) unsigned integer. */
    public long unsignedInt(int offset, int digits) {
        return zoned(offset, digits, 0, false).longValueExact();
    }

    public void setUnsignedInt(int offset, int digits, long value) {
        setZoned(offset, digits, 0, false, BigDecimal.valueOf(value));
    }
}
