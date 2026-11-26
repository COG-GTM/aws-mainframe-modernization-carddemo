package com.carddemo.programs.cbimport;

import com.carddemo.programs.cbimport.copybook.Pic;

/** WS-IMPORT-STATISTICS: PIC 9(09) counters; ADD 1 without ON SIZE ERROR wraps at 10^9. */
public final class ImportStatistics {

    private static final long MODULUS = 1_000_000_000L;

    long totalRecordsRead;
    long customerRecordsImported;
    long accountRecordsImported;
    long xrefRecordsImported;
    long tranRecordsImported;
    long cardRecordsImported;
    long errorRecordsWritten;
    long unknownRecordTypeCount;

    static long add1(long counter) {
        return (counter + 1) % MODULUS;
    }

    static String display(long counter) {
        return Pic.displayUnsigned(counter, 9);
    }

    public long totalRecordsRead() {
        return totalRecordsRead;
    }

    public long customerRecordsImported() {
        return customerRecordsImported;
    }

    public long accountRecordsImported() {
        return accountRecordsImported;
    }

    public long xrefRecordsImported() {
        return xrefRecordsImported;
    }

    public long tranRecordsImported() {
        return tranRecordsImported;
    }

    public long cardRecordsImported() {
        return cardRecordsImported;
    }

    public long errorRecordsWritten() {
        return errorRecordsWritten;
    }

    public long unknownRecordTypeCount() {
        return unknownRecordTypeCount;
    }
}
