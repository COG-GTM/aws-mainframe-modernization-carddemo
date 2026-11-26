package com.carddemo.programs.cbact04c.copybook;

/** Fixed-width encoder/decoder for one copybook 01-level record. */
public interface RecordCodec<T> {

    int length();

    T decode(String image);

    String encode(T record);
}
