package com.carddemo.runtime.cobol;

import java.util.Arrays;

/**
 * A fixed-length storage area (a 01-level record / group item). Typed access goes through
 * {@link CobolField} descriptors defined by a {@link RecordLayout}; REDEFINES are simply two descriptors
 * over the same bytes.
 *
 * <p>Copybook classes extend this and expose named accessors.
 */
public class CobolRecord {

    private final byte[] data;

    /** Creates a record of the given length initialised to LOW-VALUES (uninitialised WORKING-STORAGE). */
    public CobolRecord(int length) {
        this.data = new byte[length];
    }

    protected CobolRecord(byte[] data, boolean copy) {
        this.data = copy ? data.clone() : data;
    }

    public static CobolRecord ofBytes(byte[] bytes) {
        return new CobolRecord(bytes, true);
    }

    public final int length() {
        return data.length;
    }

    /** Returns a copy of the record bytes. */
    public final byte[] toBytes() {
        return data.clone();
    }

    /** The whole record as an alphanumeric string (ISO-8859-1, one char per byte). */
    public final String asString() {
        return PicX.fromBytes(data);
    }

    /** Group MOVE of alphanumeric data into the whole record: pad with spaces / truncate. */
    public final void move(String value) {
        setString(0, data.length, value);
    }

    /** Group MOVE of another record's bytes (pad with spaces / truncate). */
    public final void move(CobolRecord source) {
        move(source.asString());
    }

    /** Copies raw bytes into the record (shorter input is padded with spaces like a group move). */
    public final void setBytes(byte[] bytes) {
        int n = Math.min(bytes.length, data.length);
        System.arraycopy(bytes, 0, data, 0, n);
        Arrays.fill(data, n, data.length, (byte) ' ');
    }

    /** MOVE LOW-VALUES / SPACES / HIGH-VALUES / ZEROS (as character '0') TO record. */
    public final void fill(char c) {
        Arrays.fill(data, (byte) c);
    }

    public final void initializeLowValues() {
        fill(PicX.LOW_VALUE);
    }

    public final void initializeSpaces() {
        fill(PicX.SPACE);
    }

    final String getString(int offset, int length) {
        return new String(data, offset, length, PicX.CHARSET);
    }

    final void setString(int offset, int length, String value) {
        byte[] src = PicX.toBytes(PicX.of(value, length));
        System.arraycopy(src, 0, data, offset, length);
    }

    final byte[] getBytes(int offset, int length) {
        return Arrays.copyOfRange(data, offset, offset + length);
    }

    final void putBytes(int offset, byte[] bytes) {
        System.arraycopy(bytes, 0, data, offset, bytes.length);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "[" + asString().replace('\u0000', '.') + "]";
    }
}
