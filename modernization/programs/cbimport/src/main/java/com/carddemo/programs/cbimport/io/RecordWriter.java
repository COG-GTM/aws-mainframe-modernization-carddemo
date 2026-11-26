package com.carddemo.programs.cbimport.io;

import com.carddemo.programs.cbimport.FileStatus;

/** A sequential fixed-length output file; every operation returns the two-character file status. */
public interface RecordWriter {

    String open();

    String write(String record);

    String close();

    /** A DD name that is absent from the JCL: OPEN OUTPUT fails with status '35'. */
    static RecordWriter unallocated() {
        return new RecordWriter() {
            @Override
            public String open() {
                return FileStatus.FILE_NOT_FOUND;
            }

            @Override
            public String write(String record) {
                return "48";
            }

            @Override
            public String close() {
                return "42";
            }
        };
    }
}
