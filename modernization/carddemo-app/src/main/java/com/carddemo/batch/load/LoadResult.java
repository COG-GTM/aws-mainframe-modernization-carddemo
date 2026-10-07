package com.carddemo.batch.load;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import java.util.List;

/**
 * Outcome of loading one dataset image.
 *
 * @param dataset the dataset
 * @param read    records in the file
 * @param loaded  rows inserted or replaced
 * @param empty   records skipped because every data item is LOW-VALUES (VSAM priming records, see
 *                {@link com.carddemo.common.data.CopybookRecordMapper#isLowValues})
 * @param rejects records that could not be decoded or mapped; they are not loaded
 */
public record LoadResult(Dataset dataset, int read, int loaded, int empty, List<Reject> rejects) {

    public LoadResult {
        rejects = List.copyOf(rejects);
    }

    /** A record the copybook mapper refused, by 1-based record number in the file. */
    public record Reject(int recordNumber, String reason) {
    }
}
