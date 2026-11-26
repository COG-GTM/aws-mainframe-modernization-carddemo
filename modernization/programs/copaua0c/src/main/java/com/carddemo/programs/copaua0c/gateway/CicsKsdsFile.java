package com.carddemo.programs.copaua0c.gateway;

/** EXEC CICS READ DATASET(...) RIDFLD(...) RESP(...) RESP2(...) on a VSAM KSDS. */
public interface CicsKsdsFile<T> {

    /** EIBRESP values used by COPAUA0C (DFHRESP). */
    int RESP_NORMAL = 0;
    int RESP_NOTFND = 13;
    int RESP_DISABLED = 84;
    int RESP_NOTOPEN = 19;

    /** {@code record} is null unless {@code resp == RESP_NORMAL}. */
    record ReadResult<T>(int resp, int resp2, T record) {
    }

    ReadResult<T> read(String key);
}
