package com.carddemo.transaction.batch;

import com.carddemo.common.batch.GenerationDirectory;
import com.carddemo.common.batch.ReturnCode;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordWriter;
import org.springframework.stereotype.Component;

import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/**
 * Writes DD DALYREJS (430-byte FB records, in the input encoding, ordered by input record number) from the staged
 * rejects of one JobInstance. D-012 mirrors {@code DISP=(NEW,CATLG,DELETE)}: the file is assembled under a temp name
 * and renamed to {@code DALYREJS} only for RC 0/4; any other RC leaves no DALYREJS. When the job calls this (and how a
 * failed publish recovers) is the job's decision.
 */
@Component
public class RejectFileWriter {

    public static final String DD = "DALYREJS";

    private final RejectStore store;

    public RejectFileWriter(RejectStore store) {
        this.store = store;
    }

    /** Writes the temp file and returns the number of records written. */
    public long writeTemp(GenerationDirectory generation, long jobInstanceId) {
        List<byte[]> images = store.images(jobInstanceId);
        try (RecordWriter writer = RecordWriter.open(generation.temp(DD), DD, RejectRecordLayout.LAYOUT, RecordFormat.FIXED_BLOCK)) {
            images.forEach(writer::write);
            return writer.recordsWritten();
        }
    }

    /** Publishes DALYREJS for RC 0/4 or discards it for any other RC (D-012). */
    public Optional<Path> finish(GenerationDirectory generation, long jobInstanceId, int returnCode) {
        if (!ReturnCode.isSuccess(returnCode)) {
            generation.discard(DD);
            return Optional.empty();
        }
        writeTemp(generation, jobInstanceId);
        return Optional.of(generation.publish(DD));
    }
}
