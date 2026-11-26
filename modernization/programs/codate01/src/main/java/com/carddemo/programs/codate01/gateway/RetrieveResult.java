package com.carddemo.programs.codate01.gateway;

import com.carddemo.programs.codate01.copybook.MqTriggerMessage;

/** EXEC CICS RETRIEVE INTO(MQTM) RESP(..) RESP2(..) outcome; {@code data} is null unless resp is NORMAL. */
public record RetrieveResult(int resp, int resp2, MqTriggerMessage data) {

    public static RetrieveResult normal(MqTriggerMessage data) {
        return new RetrieveResult(CicsResp.NORMAL, 0, data);
    }

    public static RetrieveResult failed(int resp, int resp2) {
        return new RetrieveResult(resp, resp2, null);
    }
}
