package com.carddemo.programs.copaua0c.gateway;

import com.carddemo.programs.copaua0c.copybook.MqGetMessageOptions;
import com.carddemo.programs.copaua0c.copybook.MqMessageDescriptor;
import com.carddemo.programs.copaua0c.copybook.MqObjectDescriptor;
import com.carddemo.programs.copaua0c.copybook.MqPutMessageOptions;

/** CALL 'MQOPEN' / 'MQGET' / 'MQPUT1' / 'MQCLOSE'. */
public interface MqGateway {

    record OpenResult(int compCode, int reason, int hObj) {
    }

    /** {@code data} holds the first min(dataLength, bufferLength) characters of the message. */
    record GetResult(int compCode, int reason, String data, int dataLength) {
    }

    record CallResult(int compCode, int reason) {
    }

    OpenResult open(int hConn, MqObjectDescriptor od, int options);

    /** On success the gateway updates {@code md} with the message's CorrelId and ReplyToQ. */
    GetResult get(int hConn, int hObj, MqMessageDescriptor md, MqGetMessageOptions gmo, int bufferLength);

    CallResult put1(int hConn, MqObjectDescriptor od, MqMessageDescriptor md, MqPutMessageOptions pmo,
            int bufferLength, String buffer);

    CallResult close(int hConn, int hObj, int options);
}
