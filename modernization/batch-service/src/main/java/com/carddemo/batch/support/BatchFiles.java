package com.carddemo.batch.support;

import com.carddemo.common.batch.BatchJobParameters;
import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.batch.JobRunDirectory;
import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordLayout;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.springframework.batch.core.JobParameters;

/** Output DD helpers of the batch-service jobs: print lines, variable-length records and SYSOUT. */
public final class BatchFiles {

    private static RecordLayout printLayout(int lrecl) {
        RecordLayout.Builder builder = RecordLayout.builder("PRINT" + lrecl, "PRINT-LINE");
        builder.alpha("PRINT-LINE", "X(" + lrecl + ")");
        return builder.build(lrecl);
    }

    /** The DISPLAY output of a program, written next to its output DDs. */
    public static final String SYSOUT = "SYSOUT";

    private static final Map<Integer, RecordLayout> TEXT_LAYOUTS = new ConcurrentHashMap<>();

    private BatchFiles() {
    }

    /**
     * Writes print lines as records of {@code lrecl} bytes (a {@code PIC X(lrecl)} FD record): blank padded,
     * in the run's code page, fixed-length or line sequential as the run asks. Returns the record count.
     */
    public static int writeLines(JobParameters params, String dd, List<String> lines, int lrecl) {
        CodePage codePage = BatchJobParameters.codePage(params);
        RecordLayout layout = TEXT_LAYOUTS.computeIfAbsent(lrecl, BatchFiles::printLayout);
        AlphanumericField line = (AlphanumericField) layout.fields().get(0);
        List<CobolRecord> records = lines.stream()
                .map(text -> CobolRecord.initialized(layout, codePage).set(line, text))
                .toList();
        return DdFiles.write(BatchJobParameters.runDirectory(params), dd, records, BatchJobParameters.format(params));
    }

    /**
     * Writes {@code RECFM=V} records: each with its 4-byte RDW (big-endian length including the RDW, then two
     * zero bytes) as z/OS holds a VB data set; line sequential runs write one record per line instead.
     */
    public static int writeVariable(JobParameters params, String dd, List<byte[]> records) {
        RecordFormat format = BatchJobParameters.format(params);
        JobRunDirectory generation = BatchJobParameters.runDirectory(params);
        boolean line = format == RecordFormat.LINE_SEQUENTIAL;
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        for (byte[] record : records) {
            if (line) {
                out.writeBytes(record);
                out.write('\n');
            } else {
                int length = record.length + 4;
                out.write(length >> 8);
                out.write(length & 0xFF);
                out.write(0);
                out.write(0);
                out.writeBytes(record);
            }
        }
        write(generation.dd(line ? dd + ".txt" : dd), out.toByteArray());
        return records.size();
    }

    /** The program's DISPLAY lines (one line each, as a JES SYSOUT shows them). */
    public static void writeSysout(JobParameters params, List<String> lines) {
        StringBuilder text = new StringBuilder();
        lines.forEach(l -> text.append(l).append('\n'));
        write(BatchJobParameters.runDirectory(params).dd(SYSOUT), text.toString().getBytes(StandardCharsets.ISO_8859_1));
    }

    /** The file an earlier step of this run wrote for {@code dd}. */
    public static Path stepOutput(JobParameters params, String dd) {
        JobRunDirectory generation = BatchJobParameters.runDirectory(params);
        Path fixed = generation.dd(dd);
        return Files.isRegularFile(fixed) ? fixed : generation.dd(dd + ".txt");
    }

    private static void write(Path file, byte[] bytes) {
        try {
            Files.write(file, bytes);
        } catch (IOException ex) {
            throw new UncheckedIOException("Cannot write " + file, ex);
        }
    }
}
