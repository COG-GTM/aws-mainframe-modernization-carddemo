package com.carddemo.legacyio;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/** Decodes and encodes the bytes of records of one layout in one encoding. */
public final class RecordCodec {

    private final RecordLayout layout;
    private final Encoding encoding;

    public RecordCodec(RecordLayout layout, Encoding encoding) {
        this.layout = layout;
        this.encoding = encoding;
    }

    public RecordLayout layout() {
        return layout;
    }

    public Encoding encoding() {
        return encoding;
    }

    public Record decode(byte[] bytes) {
        if (bytes.length != layout.length()) {
            throw new IllegalArgumentException(layout.name() + " records are " + layout.length() + " bytes; got "
                    + bytes.length);
        }
        Map<String, Object> values = new LinkedHashMap<>();
        Map<String, byte[]> sourceBytes = new HashMap<>();
        for (FieldDefinition field : layout.fields()) {
            Object value = FieldCodec.decode(field, encoding, bytes, 0);
            values.put(field.name(), value);
            if (value instanceof BigDecimal) {
                sourceBytes.put(field.name(), Arrays.copyOfRange(bytes, field.offset(), field.end()));
            }
        }
        return Record.decoded(layout, values, encoding, sourceBytes);
    }

    public byte[] encode(Record record) {
        if (record.layout() != layout) {
            throw new IllegalArgumentException("Record has layout " + record.layout().name() + ", codec has "
                    + layout.name());
        }
        byte[] bytes = new byte[layout.length()];
        for (FieldDefinition field : layout.fields()) {
            byte[] source = record.sourceBytes(field.name(), encoding);
            if (source != null) {
                System.arraycopy(source, 0, bytes, field.offset(), source.length);
            } else {
                FieldCodec.encode(field, encoding, record.get(field.name()), bytes, 0);
            }
        }
        return bytes;
    }
}
