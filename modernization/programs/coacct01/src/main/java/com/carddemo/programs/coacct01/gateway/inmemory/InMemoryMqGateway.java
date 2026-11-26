package com.carddemo.programs.coacct01.gateway.inmemory;

import com.carddemo.programs.coacct01.copybook.MqGetMessageOptions;
import com.carddemo.programs.coacct01.copybook.MqMessageDescriptor;
import com.carddemo.programs.coacct01.copybook.MqObjectDescriptor;
import com.carddemo.programs.coacct01.copybook.MqPutMessageOptions;
import com.carddemo.programs.coacct01.gateway.MqGateway;
import com.carddemo.programs.coacct01.support.CobolText;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.carddemo.programs.coacct01.copybook.MqConstants.*;

/**
 * In-memory queue manager with syncpoint (unit-of-work) semantics and per-queue failure injection.
 * Queue names are compared without trailing spaces.
 */
public final class InMemoryMqGateway implements MqGateway {

    public record Message(String msgId, String correlId, String replyToQ, String format,
                          int codedCharSetId, String data) {
    }

    private record Pending(String queue, Message message) {
    }

    private record Failure(int conditionCode, int reasonCode) {
    }

    private final Map<String, Deque<Message>> queues = new LinkedHashMap<>();
    private final Map<Integer, String> handles = new HashMap<>();
    private final Map<String, Failure> openFailures = new HashMap<>();
    private final Map<String, Failure> getFailures = new HashMap<>();
    private final Map<String, Failure> putFailures = new HashMap<>();
    private final Map<String, Failure> closeFailures = new HashMap<>();
    private final List<Pending> pendingPuts = new ArrayList<>();
    private final List<Pending> pendingGets = new ArrayList<>();
    private final List<String> calls = new ArrayList<>();
    private int nextHandle = 101;
    private long nextMsgId = 1;

    public InMemoryMqGateway defineQueue(String name) {
        queues.putIfAbsent(name.strip(), new ArrayDeque<>());
        return this;
    }

    /** Puts a committed request message with a generated MsgId and the given CorrelId/ReplyToQ. */
    public Message enqueue(String queue, String data, String correlId, String replyToQ) {
        Message m = new Message(newMsgId(), CobolText.pic(correlId, 24), CobolText.pic(replyToQ, 48),
                MQFMT_STRING, MQCCSI_Q_MGR, data);
        queue(queue).addLast(m);
        return m;
    }

    public Message enqueue(String queue, String data) {
        return enqueue(queue, data, MQCI_NONE, "");
    }

    /** Committed messages currently on the queue. */
    public List<Message> messages(String queue) {
        return List.copyOf(queue(queue));
    }

    public boolean isOpen(String queue) {
        return handles.containsValue(queue.strip());
    }

    public List<String> calls() {
        return List.copyOf(calls);
    }

    public void failOpen(String queue, int cc, int rc) {
        openFailures.put(queue.strip(), new Failure(cc, rc));
    }

    public void failGet(String queue, int cc, int rc) {
        getFailures.put(queue.strip(), new Failure(cc, rc));
    }

    public void failPut(String queue, int cc, int rc) {
        putFailures.put(queue.strip(), new Failure(cc, rc));
    }

    public void failClose(String queue, int cc, int rc) {
        closeFailures.put(queue.strip(), new Failure(cc, rc));
    }

    public void commit() {
        calls.add("COMMIT");
        for (Pending p : pendingPuts) {
            queue(p.queue()).addLast(p.message());
        }
        pendingPuts.clear();
        pendingGets.clear();
    }

    public void backout() {
        calls.add("BACKOUT");
        for (int i = pendingGets.size() - 1; i >= 0; i--) {
            Pending p = pendingGets.get(i);
            queue(p.queue()).addFirst(p.message());
        }
        pendingGets.clear();
        pendingPuts.clear();
    }

    @Override
    public MqOpenResult open(int hconn, MqObjectDescriptor od, int options) {
        String name = od.objectName.strip();
        calls.add("MQOPEN " + name + " " + options);
        Failure f = openFailures.get(name);
        if (f != null) {
            return new MqOpenResult(-1, f.conditionCode(), f.reasonCode());
        }
        if (!queues.containsKey(name)) {
            return new MqOpenResult(-1, MQCC_FAILED, MQRC_UNKNOWN_OBJECT_NAME);
        }
        int hobj = nextHandle++;
        handles.put(hobj, name);
        return new MqOpenResult(hobj, MQCC_OK, MQRC_NONE);
    }

    @Override
    public MqGetResult get(int hconn, int hobj, MqMessageDescriptor md, MqGetMessageOptions gmo, int bufferLength) {
        String name = handles.get(hobj);
        calls.add("MQGET " + name + " " + gmo.options);
        if (name == null) {
            return new MqGetResult("", 0, MQCC_FAILED, MQRC_HOBJ_ERROR);
        }
        Failure f = getFailures.get(name);
        if (f != null) {
            return new MqGetResult("", 0, f.conditionCode(), f.reasonCode());
        }
        Iterator<Message> it = queue(name).iterator();
        while (it.hasNext()) {
            Message m = it.next();
            if (!matches(m, md, gmo)) {
                continue;
            }
            fill(md, m);
            int length = m.data().length();
            if (length > bufferLength) {
                return new MqGetResult(m.data().substring(0, bufferLength), length,
                        MQCC_WARNING, MQRC_TRUNCATED_MSG_FAILED);
            }
            it.remove();
            if ((gmo.options & MQGMO_SYNCPOINT) != 0) {
                pendingGets.add(new Pending(name, m));
            }
            return new MqGetResult(m.data(), length, MQCC_OK, MQRC_NONE);
        }
        return new MqGetResult("", 0, MQCC_FAILED, MQRC_NO_MSG_AVAILABLE);
    }

    @Override
    public MqCallResult put(int hconn, int hobj, MqMessageDescriptor md, MqPutMessageOptions pmo,
                            int bufferLength, String buffer) {
        String name = handles.get(hobj);
        calls.add("MQPUT " + name + " " + pmo.options);
        if (name == null) {
            return new MqCallResult(MQCC_FAILED, MQRC_HOBJ_ERROR);
        }
        Failure f = putFailures.get(name);
        if (f != null) {
            return new MqCallResult(f.conditionCode(), f.reasonCode());
        }
        if (MQMI_NONE.equals(md.msgId)) {
            md.msgId = newMsgId();
        }
        pmo.resolvedQName = CobolText.pic(name, 48);
        String data = buffer.substring(0, Math.min(bufferLength, buffer.length()));
        Message m = new Message(md.msgId, md.correlId, md.replyToQ, md.format, md.codedCharSetId, data);
        if ((pmo.options & MQPMO_SYNCPOINT) != 0) {
            pendingPuts.add(new Pending(name, m));
        } else {
            queue(name).addLast(m);
        }
        return new MqCallResult(MQCC_OK, MQRC_NONE);
    }

    @Override
    public MqCallResult close(int hconn, int hobj, int options) {
        String name = handles.get(hobj);
        calls.add("MQCLOSE " + name + " " + options);
        if (name == null) {
            return new MqCallResult(MQCC_FAILED, MQRC_HOBJ_ERROR);
        }
        Failure f = closeFailures.get(name);
        if (f != null) {
            return new MqCallResult(f.conditionCode(), f.reasonCode());
        }
        handles.remove(hobj);
        return new MqCallResult(MQCC_OK, MQRC_NONE);
    }

    private static boolean matches(Message m, MqMessageDescriptor md, MqGetMessageOptions gmo) {
        boolean msgIdOk = (gmo.matchOptions & MQMO_MATCH_MSG_ID) == 0
                || MQMI_NONE.equals(md.msgId) || md.msgId.equals(m.msgId());
        boolean correlOk = (gmo.matchOptions & MQMO_MATCH_CORREL_ID) == 0
                || MQCI_NONE.equals(md.correlId) || md.correlId.equals(m.correlId());
        return msgIdOk && correlOk;
    }

    private static void fill(MqMessageDescriptor md, Message m) {
        md.msgId = m.msgId();
        md.correlId = m.correlId();
        md.replyToQ = CobolText.pic(m.replyToQ(), 48);
        md.format = m.format();
        md.codedCharSetId = m.codedCharSetId();
    }

    private Deque<Message> queue(String name) {
        Deque<Message> q = queues.get(name.strip());
        if (q == null) {
            throw new IllegalArgumentException("Queue not defined: " + name.strip());
        }
        return q;
    }

    private String newMsgId() {
        String n = Long.toString(nextMsgId++);
        return CobolText.pic("AMQ CARDDEMO", 24 - n.length()) + n;
    }
}
