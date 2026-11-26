package com.carddemo.programs.copaus1c.gateway;

/** EXEC CICS SYNCPOINT / SYNCPOINT ROLLBACK (both also release the scheduled PSB). */
public interface SyncpointGateway {

    void syncpoint();

    void rollback();
}
