package com.carddemo.programs.codate01.gateway;

import static com.carddemo.programs.codate01.CobolFields.alnum;

import com.carddemo.programs.codate01.copybook.MqConstants;
import com.carddemo.programs.codate01.copybook.MqGetMessageOptions;
import com.carddemo.programs.codate01.copybook.MqMessageDescriptor;
import com.carddemo.programs.codate01.copybook.MqObjectDescriptor;
import com.carddemo.programs.codate01.copybook.MqPutMessageOptions;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/** In-memory queue manager with failure injection, for tests and local runs. */
public final class InMemoryMqGateway implements MqGateway {

    /** A message on a queue: descriptor snapshot plus payload. */
    public record StoredMessage(MqMessageDescriptor descriptor, String data) {
    }

    public record OpenCall(String queueName, int options, int hconn, MqCallResult result) {
    }

    public record GetCall(String queueName, int options, int waitInterval, String msgIdIn, String correlIdIn,
                          int bufferLength, MqCallResult result) {
    }

    public record PutCall(String queueName, MqMessageDescriptor descriptor, int options, int bufferLength,
                          String data, MqCallResult result) {
    }

    public record CloseCall(String queueName, int hobj, int options, MqCallResult result) {
    }

    private record Failure(int conditionCode, int reasonCode, int[] remaining) {
        boolean consume() {
            if (remaining[0] == 0) {
                return false;
            }
            if (remaining[0] > 0) {
                remaining[0]--;
            }
            return true;
        }
    }

    private final Map<String, Deque<StoredMessage>> queues = new LinkedHashMap<>();
    private final Map<Integer, String> handles = new HashMap<>();
    private final Map<String, Failure> openFailures = new HashMap<>();
    private final Map<String, Failure> getFailures = new HashMap<>();
    private final Map<String, Failure> putFailures = new HashMap<>();
    private final Map<String, Failure> closeFailures = new HashMap<>();
    private final List<OpenCall> opens = new ArrayList<>();
    private final List<GetCall> gets = new ArrayList<>();
    private final List<PutCall> puts = new ArrayList<>();
    private final List<CloseCall> closes = new ArrayList<>();
    private int nextHandle = 101;
    private int nextMsgSeq = 1;

    public InMemoryMqGateway defineQueue(String name) {
        queues.putIfAbsent(name.trim(), new ArrayDeque<>());
        return this;
    }

    /** Puts a request message on {@code queue} as another application would. */
    public InMemoryMqGateway enqueue(String queue, String data, String replyToQ) {
        MqMessageDescriptor md = new MqMessageDescriptor();
        md.msgId(String.format(Locale.ROOT, "REQMSG%018d", nextMsgSeq));
        md.correlId(String.format(Locale.ROOT, "REQCOR%018d", nextMsgSeq));
        nextMsgSeq++;
        md.replyToQ(replyToQ == null ? "" : replyToQ);
        md.format(MqConstants.MQFMT_STRING);
        return enqueue(queue, md, data);
    }

    public InMemoryMqGateway enqueue(String queue, MqMessageDescriptor md, String data) {
        defineQueue(queue);
        queues.get(queue.trim()).addLast(new StoredMessage(md.copy(), data));
        return this;
    }

    /** Every MQOPEN of {@code queue} fails. */
    public InMemoryMqGateway failOpen(String queue, int reasonCode) {
        openFailures.put(queue.trim(), new Failure(MqConstants.MQCC_FAILED, reasonCode, new int[] {-1}));
        return this;
    }

    /** The next {@code times} calls fail; a negative {@code times} means every call fails. */
    public InMemoryMqGateway failGet(String queue, int reasonCode, int times) {
        getFailures.put(queue.trim(), new Failure(MqConstants.MQCC_FAILED, reasonCode, new int[] {times}));
        return this;
    }

    public InMemoryMqGateway failPut(String queue, int reasonCode, int times) {
        putFailures.put(queue.trim(), new Failure(MqConstants.MQCC_FAILED, reasonCode, new int[] {times}));
        return this;
    }

    public InMemoryMqGateway failClose(String queue, int reasonCode, int times) {
        closeFailures.put(queue.trim(), new Failure(MqConstants.MQCC_FAILED, reasonCode, new int[] {times}));
        return this;
    }

    @Override
    public MqOpenResult mqOpen(int hconn, MqObjectDescriptor od, int options) {
        String name = od.objectName().trim();
        Failure f = openFailures.get(name);
        MqOpenResult result;
        if (f != null && f.consume()) {
            result = new MqOpenResult(MqConstants.MQHO_UNUSABLE_HOBJ, f.conditionCode(), f.reasonCode());
        } else if (!queues.containsKey(name)) {
            result = new MqOpenResult(MqConstants.MQHO_UNUSABLE_HOBJ, MqConstants.MQCC_FAILED,
                    MqConstants.MQRC_UNKNOWN_OBJECT_NAME);
        } else {
            int h = nextHandle++;
            handles.put(h, name);
            od.resolvedQName(name);
            result = new MqOpenResult(h, MqConstants.MQCC_OK, MqConstants.MQRC_NONE);
        }
        opens.add(new OpenCall(name, options, hconn, new MqCallResult(result.conditionCode(), result.reasonCode())));
        return result;
    }

    @Override
    public MqGetResult mqGet(int hconn, int hobj, MqMessageDescriptor md, MqGetMessageOptions gmo, int bufferLength) {
        String name = handles.get(hobj);
        String msgIdIn = md.msgId();
        String correlIdIn = md.correlId();
        MqGetResult result;
        if (name == null) {
            result = new MqGetResult(MqConstants.MQCC_FAILED, MqConstants.MQRC_HOBJ_ERROR, 0, null);
        } else {
            Failure f = getFailures.get(name);
            Deque<StoredMessage> q = queues.get(name);
            if (f != null && f.consume()) {
                result = new MqGetResult(f.conditionCode(), f.reasonCode(), 0, null);
            } else if (q.isEmpty()) {
                result = new MqGetResult(MqConstants.MQCC_FAILED, MqConstants.MQRC_NO_MSG_AVAILABLE, 0, null);
            } else {
                StoredMessage m = q.peekFirst();
                if (m.data().length() > bufferLength) {
                    // No MQGMO-ACCEPT-TRUNCATED-MSG: message stays on the queue
                    result = new MqGetResult(MqConstants.MQCC_FAILED, MqConstants.MQRC_TRUNCATED_MSG_FAILED,
                            m.data().length(), m.data().substring(0, bufferLength));
                } else {
                    q.removeFirst();
                    md.copyFrom(m.descriptor());
                    gmo.resolvedQName(name);
                    gmo.returnedLength = m.data().length();
                    result = new MqGetResult(MqConstants.MQCC_OK, MqConstants.MQRC_NONE, m.data().length(), m.data());
                }
            }
        }
        gets.add(new GetCall(name, gmo.options, gmo.waitInterval, msgIdIn, correlIdIn, bufferLength,
                new MqCallResult(result.conditionCode(), result.reasonCode())));
        return result;
    }

    @Override
    public MqCallResult mqPut(int hconn, int hobj, MqMessageDescriptor md, MqPutMessageOptions pmo,
                              int bufferLength, String buffer) {
        String name = handles.get(hobj);
        String data = alnum(buffer, bufferLength);
        MqCallResult result;
        if (name == null) {
            result = new MqCallResult(MqConstants.MQCC_FAILED, MqConstants.MQRC_HOBJ_ERROR);
        } else {
            Failure f = putFailures.get(name);
            if (f != null && f.consume()) {
                result = new MqCallResult(f.conditionCode(), f.reasonCode());
            } else {
                queues.get(name).addLast(new StoredMessage(md.copy(), data));
                result = new MqCallResult(MqConstants.MQCC_OK, MqConstants.MQRC_NONE);
            }
        }
        puts.add(new PutCall(name, md.copy(), pmo.options, bufferLength, data, result));
        return result;
    }

    @Override
    public MqCallResult mqClose(int hconn, int hobj, int options) {
        String name = handles.get(hobj);
        MqCallResult result;
        if (name == null) {
            result = new MqCallResult(MqConstants.MQCC_FAILED, MqConstants.MQRC_HOBJ_ERROR);
        } else {
            Failure f = closeFailures.get(name);
            if (f != null && f.consume()) {
                result = new MqCallResult(f.conditionCode(), f.reasonCode());
            } else {
                handles.remove(hobj);
                result = new MqCallResult(MqConstants.MQCC_OK, MqConstants.MQRC_NONE);
            }
        }
        closes.add(new CloseCall(name, hobj, options, result));
        return result;
    }

    /** Messages currently on {@code queue}, oldest first. */
    public List<StoredMessage> messages(String queue) {
        Deque<StoredMessage> q = queues.get(queue.trim());
        return q == null ? List.of() : List.copyOf(q);
    }

    public List<OpenCall> opens() { return List.copyOf(opens); }
    public List<GetCall> gets() { return List.copyOf(gets); }
    public List<PutCall> puts() { return List.copyOf(puts); }
    public List<CloseCall> closes() { return List.copyOf(closes); }
    public int openHandleCount() { return handles.size(); }
}
