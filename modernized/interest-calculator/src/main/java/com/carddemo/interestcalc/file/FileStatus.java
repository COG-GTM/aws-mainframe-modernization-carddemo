package com.carddemo.interestcalc.file;

/**
 * The VSAM/COBOL file status codes that CBACT04C actually branches on. Keeping them as first
 * class values (rather than collapsing them into exceptions or {@code Optional}) is what makes
 * the {@code '23'} retry in {@code 1200-GET-INTEREST-RATE} a faithful translation.
 */
public final class FileStatus {

    /** Successful completion. */
    public static final String OK = "00";
    /** End of file on a sequential read. */
    public static final String END_OF_FILE = "10";
    /** Record not found on a keyed read (INVALID KEY). */
    public static final String NOT_FOUND = "23";

    private FileStatus() {
    }
}
