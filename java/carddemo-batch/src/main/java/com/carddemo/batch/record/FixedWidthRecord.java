package com.carddemo.batch.record;

import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

import java.util.Map;

/**
 * Base of the copybook-backed record classes. The record is held as the byte buffer COBOL would keep
 * in WORKING-STORAGE / the FD record area; typed getters and setters decode and encode in place, so
 * writing the record back out is byte-identical to what the COBOL program writes.
 */
public abstract class FixedWidthRecord {

    protected final byte[] data;

    protected FixedWidthRecord(Layout layout) {
        this.data = FixedWidth.blank(layout);
    }

    protected FixedWidthRecord(Layout layout, byte[] raw) {
        this.data = FixedWidth.decode(layout, raw);
    }

    public abstract Layout layout();

    /** The record bytes (a copy), exactly as they would be WRITEten. */
    public byte[] encode() {
        return data.clone();
    }

    public int length() {
        return data.length;
    }

    /** COBOL {@code INITIALIZE record}. */
    public void initialize() {
        FixedWidth.initialize(data, layout());
    }

    /**
     * Fills the record with LOW-VALUES (x'00'), the content of a never-assigned FD record area under
     * GnuCOBOL (undefined on z/OS). Used to reproduce what the COBOL writes for fields it never MOVEs to.
     */
    public void lowValues() {
        java.util.Arrays.fill(data, (byte) 0);
    }

    /** The record as ISO-8859-1 text, i.e. what {@code DISPLAY record} prints. */
    public String display() {
        return new String(data, FixedWidth.CHARSET);
    }

    /** Field name to typed value (String / BigDecimal / List of maps for OCCURS), FILLER omitted. */
    public Map<String, Object> toMap() {
        return FixedWidth.toMap(data, layout());
    }

    @Override
    public String toString() {
        return layout().name() + toMap();
    }
}
