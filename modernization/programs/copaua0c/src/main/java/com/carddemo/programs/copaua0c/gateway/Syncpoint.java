package com.carddemo.programs.copaua0c.gateway;

/** EXEC CICS SYNCPOINT (commits IMS updates and releases the PSB). */
public interface Syncpoint {

    void syncpoint();
}
