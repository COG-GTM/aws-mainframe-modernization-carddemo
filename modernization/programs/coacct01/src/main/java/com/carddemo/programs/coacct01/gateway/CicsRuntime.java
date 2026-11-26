package com.carddemo.programs.coacct01.gateway;

import com.carddemo.programs.coacct01.copybook.MqTriggerMessage;

/** The EXEC CICS services COACCT01 uses other than file control. */
public interface CicsRuntime {

    /** EXEC CICS RETRIEVE INTO(MQTM) RESP(..) RESP2(..). */
    RetrieveResult retrieve();

    /** EXEC CICS SYNCPOINT: commit the unit of work (MQ syncpoint GETs/PUTs). */
    void syncpoint();

    /** EXEC CICS RETURN: normal task end, which commits the unit of work. */
    void returnToCics();

    /** Abnormal task end (e.g. ASRA): the unit of work is backed out. */
    void abend(String abendCode);

    record RetrieveResult(int resp, int resp2, MqTriggerMessage triggerMessage) {
    }
}
