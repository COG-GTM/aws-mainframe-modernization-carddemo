package com.carddemo.modernization.records.codec;

import java.nio.charset.Charset;

public interface RecordCodec<T> {
    int length();
    T decode(byte[] bytes, Charset charset);
    byte[] encode(T record, Charset charset);
    default T decode(byte[] bytes) { return decode(bytes, TextCodec.ASCII); }
    default byte[] encode(T record) { return encode(record, TextCodec.ASCII); }
}
