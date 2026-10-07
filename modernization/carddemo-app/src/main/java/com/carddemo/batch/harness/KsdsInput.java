package com.carddemo.batch.harness;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import org.springframework.data.domain.Limit;

/**
 * A VSAM KSDS opened {@code INPUT} and read sequentially in key order ({@code READ ... NEXT}), backed by either its
 * PostgreSQL table (keyset pages, {@code COLLATE "C"} key order = EBCDIC-agnostic byte order of the sample keys) or a
 * fixed-width unload file. I/O errors surface as {@code FileStatusException} so programs can reproduce their
 * {@code 9910-DISPLAY-IO-STATUS} paths.
 */
public interface KsdsInput {

    String ddname();

    /** {@code OPEN INPUT}. */
    void open();

    /** {@code READ NEXT}; empty at end of file (status 10). */
    Optional<FixedWidthRecord> readNext();

    /** {@code CLOSE}. */
    void close();

    /** A KSDS unload: fixed-length records when EBCDIC, line sequential when ASCII (as under GnuCOBOL). */
    static KsdsInput file(String ddname, Path path, RecordLayout layout, RecordEncoding encoding) {
        return new FileKsdsInput(ddname, path, layout, encoding);
    }

    /**
     * The table holding the KSDS: {@code after(lastKey, limit)} returns the next rows in key order with keys greater
     * than {@code lastKey}, starting from {@code lowValues}.
     */
    static <E, K> KsdsInput table(String ddname, K lowValues, BiFunction<K, Limit, List<E>> after,
                                  Function<E, K> key, Function<E, FixedWidthRecord> toRecord, int pageSize) {
        return new TableKsdsInput<>(ddname, lowValues, after, key, toRecord, pageSize);
    }
}
