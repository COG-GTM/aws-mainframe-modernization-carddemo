package com.carddemo.batch.source;

import com.carddemo.common.batch.BatchJobParameters;
import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.RecordLayout;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.function.Supplier;
import org.springframework.batch.core.JobParameters;

/**
 * An input DD that names a VSAM KSDS read sequentially ({@code ACCESS MODE IS SEQUENTIAL}): the
 * {@code --dd=<DD>=<file>} file when given, else the service table the data-loader filled from the
 * same sample file. Records come back in key order (unsigned bytes of the key in the records' code
 * page, as the cluster returns them); a duplicate key is an error, as the REPRO that loads a KSDS
 * rejects it.
 */
public final class KsdsInput {

    /** The records of one DD and where they came from (file path or table), for the step summary. */
    public record Dataset(String source, List<CobolRecord> records) {
    }

    private KsdsInput() {
    }

    public static Dataset read(JobParameters params, String dd, RecordLayout layout, int keyOffset, int keyLength,
                               String table, Supplier<List<CobolRecord>> tableRows) {
        return BatchJobParameters.dd(params, dd)
                .map(file -> new Dataset(file.toString(),
                        keyOrder(dd, DdFiles.read(file, layout, BatchJobParameters.codePage(params)), keyOffset, keyLength)))
                .orElseGet(() -> new Dataset(table, keyOrder(dd, tableRows.get(), keyOffset, keyLength)));
    }

    /** Sorts by key bytes; fails on a duplicate key. */
    public static List<CobolRecord> keyOrder(String dd, List<CobolRecord> records, int keyOffset, int keyLength) {
        List<CobolRecord> sorted = new ArrayList<>(records);
        Comparator<CobolRecord> byKey = (a, b) -> Arrays.compareUnsigned(a.toBytes(), keyOffset, keyOffset + keyLength,
                b.toBytes(), keyOffset, keyOffset + keyLength);
        sorted.sort(byKey);
        for (int i = 1; i < sorted.size(); i++) {
            if (byKey.compare(sorted.get(i - 1), sorted.get(i)) == 0) {
                byte[] key = Arrays.copyOfRange(sorted.get(i).toBytes(), keyOffset, keyOffset + keyLength);
                throw new IllegalStateException(dd + ": duplicate key " + sorted.get(i).codePage().decodeText(key, 0, key.length));
            }
        }
        return sorted;
    }
}
