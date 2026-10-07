package com.carddemo.batch.harness;

import com.carddemo.common.codec.FixedWidthRecord;

/**
 * A dataset opened {@code OUTPUT}: a fixed-width file ({@link FixedFileSink}, RECFM=FB, byte-for-byte the codec
 * image), a variable-length file ({@link VariableFileSink}, RECFM=V) or a PostgreSQL table ({@link TableSink}).
 * Failures surface as {@code FileStatusException}.
 */
public interface RecordSink extends AutoCloseable {

    String ddname();

    /** {@code OPEN OUTPUT}. */
    void open();

    /** {@code WRITE record}. */
    void write(FixedWidthRecord record);

    /** Records written since {@link #open()}. */
    long count();

    boolean isOpen();

    /** {@code CLOSE}; a no-op when not open, as the implicit close at {@code GOBACK}. */
    @Override
    void close();
}
