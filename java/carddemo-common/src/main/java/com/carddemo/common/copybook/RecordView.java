package com.carddemo.common.copybook;

import com.carddemo.common.cobol.CobolBinary;
import com.carddemo.common.cobol.CobolDataException;
import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.cobol.NumericEdited;
import com.carddemo.common.cobol.PackedDecimal;
import com.carddemo.common.cobol.ZonedDecimal;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Typed read/write access to the bytes of one record through its {@link RecordLayout} (a flyweight: no copy of
 * the data is made). OCCURS indices are zero-based and given outermost first. REDEFINES alternates are read and
 * written like any other item, over the storage they share.
 */
public final class RecordView {

    private final RecordLayout layout;
    private final DataEncoding encoding;
    private final byte[] data;

    private RecordView(RecordLayout layout, DataEncoding encoding, byte[] data) {
        this.layout = layout;
        this.encoding = encoding;
        this.data = data;
    }

    public static RecordView wrap(RecordLayout layout, DataEncoding encoding, byte[] data) {
        if (data.length < layout.length()) {
            throw new CobolDataException("Record of " + data.length + " bytes is shorter than " + layout);
        }
        return new RecordView(layout, encoding, data);
    }

    /** A new record with COBOL INITIALIZE semantics: spaces for alphanumeric/FILLER, zero for numeric items. */
    public static RecordView initialized(RecordLayout layout, DataEncoding encoding) {
        byte[] data = new byte[layout.length()];
        Arrays.fill(data, encoding.space());
        RecordView view = new RecordView(layout, encoding, data);
        for (CopybookField f : layout.primaryElementaryFields()) {
            if (f.pic().isNumeric()) {
                for (int offset : offsets(f)) {
                    view.writeDecimal(f, offset, BigDecimal.ZERO);
                }
            }
        }
        return view;
    }

    public RecordLayout layout() {
        return layout;
    }

    public DataEncoding encoding() {
        return encoding;
    }

    /** The backing bytes (not a copy). */
    public byte[] bytes() {
        return data;
    }

    public String getString(String name, int... indices) {
        return getString(layout.field(name), indices);
    }

    /** Raw text of an alphanumeric, edited or group item (trailing spaces kept). */
    public String getString(CopybookField field, int... indices) {
        return encoding.decode(data, offset(field, indices), field.length());
    }

    public BigDecimal getDecimal(String name, int... indices) {
        return getDecimal(layout.field(name), indices);
    }

    public BigDecimal getDecimal(CopybookField field, int... indices) {
        requireElementary(field);
        int offset = offset(field, indices);
        if (field.pic().isEdited()) {
            return NumericEdited.parse(encoding.decode(data, offset, field.length()), field.pic());
        }
        requireNumeric(field);
        return switch (field.usage()) {
            case DISPLAY -> ZonedDecimal.decode(data, offset, field.length(), field.pic().scale(), field.pic().signed(),
                    field.signMode(), encoding);
            case PACKED_DECIMAL -> PackedDecimal.decode(data, offset, field.length(), field.pic().scale());
            case BINARY, NATIVE_BINARY -> CobolBinary.decode(data, offset, field.length(), field.pic().scale(),
                    field.pic().signed());
        };
    }

    public long getLong(String name, int... indices) {
        return getDecimal(name, indices).longValueExact();
    }

    public int getInt(String name, int... indices) {
        return getDecimal(name, indices).intValueExact();
    }

    /** COBOL class test {@code IF field IS NUMERIC}. */
    public boolean isNumeric(CopybookField field, int... indices) {
        if (!field.pic().isNumeric() && !field.pic().isEdited()) {
            String text = getString(field, indices);
            return !text.isEmpty() && text.chars().allMatch(ch -> ch >= '0' && ch <= '9');
        }
        try {
            getDecimal(field, indices);
            return true;
        } catch (CobolDataException e) {
            return false;
        }
    }

    public void setString(String name, String value, int... indices) {
        setString(layout.field(name), value, indices);
    }

    /** Alphanumeric MOVE: left-justified, space padded, truncated on the right. */
    public void setString(CopybookField field, String value, int... indices) {
        if (field.pic() != null && field.pic().isNumeric()) {
            throw new CobolDataException("Use setDecimal for numeric item " + field.name());
        }
        int offset = offset(field, indices);
        Arrays.fill(data, offset, offset + field.length(), encoding.space());
        byte[] bytes = encoding.encode(value == null ? "" : value);
        System.arraycopy(bytes, 0, data, offset, Math.min(bytes.length, field.length()));
    }

    public void setDecimal(String name, BigDecimal value, int... indices) {
        setDecimal(layout.field(name), value, indices);
    }

    public void setDecimal(CopybookField field, BigDecimal value, int... indices) {
        requireElementary(field);
        writeDecimal(field, offset(field, indices), value);
    }

    public void setLong(String name, long value, int... indices) {
        setDecimal(name, BigDecimal.valueOf(value), indices);
    }

    private void writeDecimal(CopybookField field, int offset, BigDecimal value) {
        if (field.pic().isEdited()) {
            byte[] text = encoding.encode(NumericEdited.format(value, field.pic()));
            System.arraycopy(text, 0, data, offset, field.length());
            return;
        }
        requireNumeric(field);
        int digits = field.pic().digits();
        int scale = field.pic().scale();
        boolean signed = field.pic().signed();
        switch (field.usage()) {
            case DISPLAY -> ZonedDecimal.encode(value, digits, scale, signed, field.signMode(), encoding, data, offset);
            case PACKED_DECIMAL -> PackedDecimal.encode(value, digits, scale, signed, data, offset);
            case BINARY -> CobolBinary.encode(value, digits, scale, signed, false, data, offset);
            case NATIVE_BINARY -> CobolBinary.encode(value, digits, scale, signed, true, data, offset);
        }
    }

    /** Absolute byte offset of one occurrence of {@code field}. */
    public static int offset(CopybookField field, int... indices) {
        List<CopybookField> occurs = field.occursAncestors();
        if (indices.length != occurs.size()) {
            throw new IllegalArgumentException(field.name() + " needs " + occurs.size() + " OCCURS index(es), got "
                    + indices.length);
        }
        int offset = field.offset();
        for (int k = 0; k < indices.length; k++) {
            CopybookField o = occurs.get(k);
            if (indices[k] < 0 || indices[k] >= o.occurs()) {
                throw new IndexOutOfBoundsException(o.name() + " index " + indices[k] + " outside 0.." + (o.occurs() - 1));
            }
            offset += indices[k] * o.length();
        }
        return offset;
    }

    /** Offsets of every occurrence of {@code field} (a single offset when it is not under any OCCURS). */
    public static List<Integer> offsets(CopybookField field) {
        List<Integer> result = new ArrayList<>();
        result.add(field.offset());
        for (CopybookField o : field.occursAncestors()) {
            List<Integer> next = new ArrayList<>();
            for (int base : result) {
                for (int i = 0; i < o.occurs(); i++) {
                    next.add(base + i * o.length());
                }
            }
            result = next;
        }
        return result;
    }

    private static void requireElementary(CopybookField field) {
        if (field.isGroup()) {
            throw new CobolDataException("Group item " + field.name() + " has no numeric value");
        }
    }

    private static void requireNumeric(CopybookField field) {
        if (!field.pic().isNumeric()) {
            throw new CobolDataException("Item " + field.name() + " PIC " + field.pic().source() + " is not numeric");
        }
    }
}
