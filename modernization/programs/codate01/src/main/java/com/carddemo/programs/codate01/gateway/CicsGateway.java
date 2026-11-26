package com.carddemo.programs.codate01.gateway;

/** The EXEC CICS commands CODATE01 issues. */
public interface CicsGateway {

    /** EXEC CICS RETRIEVE INTO(MQTM) RESP RESP2. */
    RetrieveResult retrieve();

    /** EXEC CICS ASKTIME ABSTIME: milliseconds since 00:00 on 1 January 1900 (local time). */
    long askTime();

    /** EXEC CICS SYNCPOINT. */
    void syncpoint();

    /** EXEC CICS RETURN (task end; CICS commits the unit of work). */
    void returnToCics();
}
