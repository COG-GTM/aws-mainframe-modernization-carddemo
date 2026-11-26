package com.carddemo.programs.copaua0c.gateway.inmemory;

import com.carddemo.programs.copaua0c.copybook.MqConstants;
import com.carddemo.programs.copaua0c.copybook.MqGetMessageOptions;
import com.carddemo.programs.copaua0c.copybook.MqMessageDescriptor;
import com.carddemo.programs.copaua0c.copybook.MqObjectDescriptor;
import com.carddemo.programs.copaua0c.copybook.MqPutMessageOptions;
import com.carddemo.programs.copaua0c.gateway.MqGateway;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

/** Single request queue plus captured MQPUT1 replies, with injectable completion/reason codes. */
public final class InMemoryMqGateway implements MqGateway {

    public record RequestMessage(String data, String correlId, String replyToQ) {
    }

    public record Reply(MqObjectDescriptor od, MqMessageDescriptor md, MqPutMessageOptions pmo,
            int bufferLength, String payload) {
    }

    private final Deque<RequestMessage> requests = new ArrayDeque<>();
    private final List<Reply> replies = new ArrayList<>();
    private final List<MqGetMessageOptions> getOptions = new ArrayList<>();
    private String openedQueue;
    private int openOptions;
    private boolean closed;
    private CallResult openFailure;
    private CallResult getFailure;
    private CallResult putFailure;
    private CallResult closeFailure;

    public InMemoryMqGateway enqueue(String data, String correlId, String replyToQ) {
        requests.add(new RequestMessage(data, correlId, replyToQ));
        return this;
    }

    public List<Reply> replies() { return replies; }
    public List<MqGetMessageOptions> getOptions() { return getOptions; }
    public String openedQueue() { return openedQueue; }
    public int openOptions() { return openOptions; }
    public boolean closed() { return closed; }
    public int pending() { return requests.size(); }

    public void failOpen(int compCode, int reason) { openFailure = new CallResult(compCode, reason); }
    public void failGet(int compCode, int reason) { getFailure = new CallResult(compCode, reason); }
    public void failPut(int compCode, int reason) { putFailure = new CallResult(compCode, reason); }
    public void failClose(int compCode, int reason) { closeFailure = new CallResult(compCode, reason); }

    @Override
    public OpenResult open(int hConn, MqObjectDescriptor od, int options) {
        openedQueue = od.objectName();
        openOptions = options;
        if (openFailure != null) {
            return new OpenResult(openFailure.compCode(), openFailure.reason(), -1);
        }
        return new OpenResult(MqConstants.MQCC_OK, MqConstants.MQRC_NONE, 1);
    }

    @Override
    public GetResult get(int hConn, int hObj, MqMessageDescriptor md, MqGetMessageOptions gmo, int bufferLength) {
        getOptions.add(gmo);
        if (getFailure != null) {
            return new GetResult(getFailure.compCode(), getFailure.reason(), "", 0);
        }
        RequestMessage m = requests.poll();
        if (m == null) {
            return new GetResult(MqConstants.MQCC_FAILED, MqConstants.MQRC_NO_MSG_AVAILABLE, "", 0);
        }
        md.correlId(m.correlId());
        md.replyToQ(m.replyToQ());
        String data = m.data().length() > bufferLength ? m.data().substring(0, bufferLength) : m.data();
        return new GetResult(MqConstants.MQCC_OK, MqConstants.MQRC_NONE, data, data.length());
    }

    @Override
    public CallResult put1(int hConn, MqObjectDescriptor od, MqMessageDescriptor md, MqPutMessageOptions pmo,
            int bufferLength, String buffer) {
        if (putFailure != null) {
            return putFailure;
        }
        replies.add(new Reply(od, md.copy(), pmo, bufferLength, buffer));
        return new CallResult(MqConstants.MQCC_OK, MqConstants.MQRC_NONE);
    }

    @Override
    public CallResult close(int hConn, int hObj, int options) {
        if (closeFailure != null) {
            return closeFailure;
        }
        closed = true;
        return new CallResult(MqConstants.MQCC_OK, MqConstants.MQRC_NONE);
    }
}
