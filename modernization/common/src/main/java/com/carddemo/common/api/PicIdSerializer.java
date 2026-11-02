package com.carddemo.common.api;

import com.carddemo.common.cobol.CobolValues;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.BeanProperty;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.ContextualSerializer;
import java.io.IOException;

/** Writes a {@link PicId} number as a zero-padded string of its picture width. */
public class PicIdSerializer extends JsonSerializer<Number> implements ContextualSerializer {

    private final int digits;

    public PicIdSerializer() {
        this(0);
    }

    private PicIdSerializer(int digits) {
        this.digits = digits;
    }

    @Override
    public JsonSerializer<?> createContextual(SerializerProvider provider, BeanProperty property) {
        PicId picId = property == null ? null : property.getAnnotation(PicId.class);
        return picId == null ? this : new PicIdSerializer(picId.digits());
    }

    @Override
    public void serialize(Number value, JsonGenerator generator, SerializerProvider provider) throws IOException {
        if (digits == 0) {
            generator.writeNumber(value.longValue());
        } else {
            generator.writeString(CobolValues.zeroPad(value.longValue(), digits));
        }
    }
}
