package com.carddemo.programs.coacct01.gateway.inmemory;

import com.carddemo.programs.coacct01.copybook.MqTriggerMessage;
import com.carddemo.programs.coacct01.gateway.CicsResp;
import com.carddemo.programs.coacct01.gateway.CicsRuntime;

/** CICS task stub: supplies the trigger message and drives the in-memory MQ unit of work. */
public final class InMemoryCicsRuntime implements CicsRuntime {

    private final InMemoryMqGateway mq;
    private final RetrieveResult retrieveResult;
    private int syncpointCount;
    private boolean returned;
    private String abendCode;

    private InMemoryCicsRuntime(InMemoryMqGateway mq, RetrieveResult retrieveResult) {
        this.mq = mq;
        this.retrieveResult = retrieveResult;
    }

    /** Task started by the trigger monitor for the given input queue. */
    public static InMemoryCicsRuntime triggeredBy(InMemoryMqGateway mq, String inputQueue) {
        return new InMemoryCicsRuntime(mq,
                new RetrieveResult(CicsResp.NORMAL, 0, MqTriggerMessage.forQueue(inputQueue)));
    }

    /** Task whose EXEC CICS RETRIEVE fails with the given RESP/RESP2. */
    public static InMemoryCicsRuntime retrieveFails(InMemoryMqGateway mq, int resp, int resp2) {
        return new InMemoryCicsRuntime(mq, new RetrieveResult(resp, resp2, null));
    }

    @Override
    public RetrieveResult retrieve() {
        return retrieveResult;
    }

    @Override
    public void syncpoint() {
        syncpointCount++;
        mq.commit();
    }

    @Override
    public void returnToCics() {
        returned = true;
        mq.commit();
    }

    @Override
    public void abend(String code) {
        abendCode = code;
        mq.backout();
    }

    public int syncpointCount() {
        return syncpointCount;
    }

    public boolean returned() {
        return returned;
    }

    public String abendCode() {
        return abendCode;
    }
}
