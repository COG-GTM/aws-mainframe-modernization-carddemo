package com.carddemo.batch.io;

import java.util.function.Function;

/** Encodes and decodes one copybook record as a fixed-width line, byte for byte. */
public interface FixedWidthCodec<T> {

    /** Record length in bytes, as declared by the copybook. */
    int length();

    T decode(String line);

    String encode(T record);

    static <T> FixedWidthCodec<T> of(int length, Function<FieldReader, T> decoder, Function<T, String> encoder) {
        return new FixedWidthCodec<>() {
            @Override
            public int length() {
                return length;
            }

            @Override
            public T decode(String line) {
                FieldReader r = new FieldReader(line, length);
                T value = decoder.apply(r);
                r.end();
                return value;
            }

            @Override
            public String encode(T record) {
                return encoder.apply(record);
            }
        };
    }
}
