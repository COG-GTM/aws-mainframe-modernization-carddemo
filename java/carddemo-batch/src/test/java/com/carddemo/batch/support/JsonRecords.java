package com.carddemo.batch.support;

import com.carddemo.batch.codec.CodecException;
import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Decodes Java-written records into the same JSON shape {@code test-harness/records.py} produces for the
 * goldens: every scalar is a string (plain decimal for numerics, exact text for PIC X), FILLER is omitted,
 * OCCURS groups are lists, and a COMP-3 field whose bytes are not a valid packed decimal becomes the
 * marker {@code INVALID-COMP-3:<hex>} (lenient mode) instead of failing.
 */
public final class JsonRecords {

    public static final String INVALID_PREFIX = "INVALID-COMP-3:";

    private JsonRecords() {
    }

    public static Map<String, Object> toJson(byte[] buf, Layout layout) {
        return toJson(buf, layout.fields());
    }

    private static Map<String, Object> toJson(byte[] buf, List<Field> fields) {
        Map<String, Object> out = new LinkedHashMap<>();
        for (Field f : fields) {
            switch (f.usage()) {
                case FILLER:
                    break;
                case TEXT:
                    out.put(f.name(), FixedWidth.text(buf, f));
                    break;
                case ZONED:
                    out.put(f.name(), plain(FixedWidth.decimal(buf, f)));
                    break;
                case PACKED:
                    try {
                        out.put(f.name(), plain(FixedWidth.decimal(buf, f)));
                    } catch (CodecException e) {
                        out.put(f.name(), INVALID_PREFIX + hex(FixedWidth.bytes(buf, f)));
                    }
                    break;
                case GROUP:
                    List<Map<String, Object>> occ = new ArrayList<>();
                    for (int i = 0; i < f.occurs(); i++) {
                        occ.add(toJson(buf, f.occurrence(i).children()));
                    }
                    out.put(f.name(), f.occurs() > 1 ? occ : occ.get(0));
                    break;
                default:
                    break;
            }
        }
        return out;
    }

    /** records.py formatting: no leading zeros, scale digits kept, "-" only for a non-zero negative. */
    public static String plain(BigDecimal v) {
        return v.signum() == 0 ? v.abs().toPlainString() : v.toPlainString();
    }

    public static String hex(byte[] b) {
        StringBuilder sb = new StringBuilder(b.length * 2);
        for (byte x : b) {
            sb.append(String.format("%02x", x & 0xFF));
        }
        return sb.toString();
    }
}
