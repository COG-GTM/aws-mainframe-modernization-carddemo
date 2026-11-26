package com.carddemo.programs.cotrtlic.gateway;

/** EXEC CICS SYNCPOINT: commit the current unit of work. */
public interface CicsSyncpoint {
    void syncpoint();
}
