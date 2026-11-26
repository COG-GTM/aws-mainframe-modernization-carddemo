package com.carddemo.programs.cbcus01c;

/**
 * Pluggable replacement for {@code SELECT CUSTFILE-FILE ASSIGN TO CUSTFILE ORGANIZATION IS INDEXED
 * ACCESS MODE IS SEQUENTIAL RECORD KEY IS FD-CUST-ID FILE STATUS IS CUSTFILE-STATUS}.
 *
 * <p>Every operation reports the two-byte FILE STATUS rather than throwing, so the program can
 * apply the original status-code branching unchanged.
 */
public interface CustomerFile {

    /** OPEN INPUT. Returns CUSTFILE-STATUS. */
    String open();

    /** READ next record in key sequence. */
    ReadResult readNext();

    /** CLOSE. Returns CUSTFILE-STATUS. */
    String close();

    /** Result of a sequential READ: status plus the FD record when status is '00'. */
    record ReadResult(String status, FdCustfileRec record) {
        public static ReadResult ok(FdCustfileRec record) {
            return new ReadResult(FileStatus.SUCCESS, record);
        }

        public static ReadResult status(String status) {
            return new ReadResult(status, null);
        }
    }
}
