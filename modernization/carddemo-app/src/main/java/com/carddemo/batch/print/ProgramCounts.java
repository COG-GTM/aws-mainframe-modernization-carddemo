package com.carddemo.batch.print;

/** What a print program read and wrote, for the step's read/write counts in {@code batch_run}. */
public record ProgramCounts(long read, long written) {
}
