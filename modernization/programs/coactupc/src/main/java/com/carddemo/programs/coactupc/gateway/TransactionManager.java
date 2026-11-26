package com.carddemo.programs.coactupc.gateway;

/** CICS unit-of-work control. */
public interface TransactionManager {

    /** EXEC CICS SYNCPOINT (also the implicit syncpoint at task end on EXEC CICS RETURN). */
    void syncpoint();

    /** EXEC CICS SYNCPOINT ROLLBACK (also the implicit backout when the task abends). */
    void rollback();
}
