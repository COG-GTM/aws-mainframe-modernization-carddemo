package com.carddemo.programs.copaua0c.gateway;

import com.carddemo.programs.copaua0c.copybook.ErrorLogRecord;

/** EXEC CICS WRITEQ TD QUEUE(...) FROM(ERROR-LOG-RECORD) NOHANDLE. */
public interface TransientDataQueue {

    void writeq(String queue, ErrorLogRecord record);
}
