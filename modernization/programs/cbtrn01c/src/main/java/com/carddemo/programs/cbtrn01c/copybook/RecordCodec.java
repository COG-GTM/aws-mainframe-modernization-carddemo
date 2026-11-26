package com.carddemo.programs.cbtrn01c.copybook;

/** Converts a copybook record to and from its fixed-width record image. */
public interface RecordCodec<R> {

    int length();

    R parse(String image);

    String format(R record);
}
