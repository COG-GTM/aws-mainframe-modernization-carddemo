package com.carddemo.programs.cotrtupc.gateway;

/** CICS unit-of-work control. */
@FunctionalInterface
public interface CicsSyncpoint {

    /** EXEC CICS SYNCPOINT END-EXEC - commit the current unit of work. */
    void syncpoint();

    /** Dynamic transaction backout after an abend: undo uncommitted changes of the current unit of work. */
    default void syncpointRollback() {
    }
}
