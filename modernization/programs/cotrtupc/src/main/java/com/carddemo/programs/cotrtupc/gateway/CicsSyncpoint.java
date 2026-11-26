package com.carddemo.programs.cotrtupc.gateway;

/** EXEC CICS SYNCPOINT END-EXEC - commit the current unit of work. */
@FunctionalInterface
public interface CicsSyncpoint {
    void syncpoint();
}
