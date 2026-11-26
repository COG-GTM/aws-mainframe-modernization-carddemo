package com.carddemo.programs.coacct01.gateway;

import com.carddemo.programs.coacct01.copybook.MqGetMessageOptions;
import com.carddemo.programs.coacct01.copybook.MqMessageDescriptor;
import com.carddemo.programs.coacct01.copybook.MqObjectDescriptor;
import com.carddemo.programs.coacct01.copybook.MqPutMessageOptions;

/**
 * The MQI calls COACCT01 makes (CALL 'MQOPEN' / 'MQGET' / 'MQPUT' / 'MQCLOSE'). Descriptors and
 * options are passed by reference and may be updated in place, as with the MQI.
 * Buffers are single-byte character strings (one char per byte).
 */
public interface MqGateway {

    MqOpenResult open(int hconn, MqObjectDescriptor od, int options);

    MqGetResult get(int hconn, int hobj, MqMessageDescriptor md, MqGetMessageOptions gmo, int bufferLength);

    MqCallResult put(int hconn, int hobj, MqMessageDescriptor md, MqPutMessageOptions pmo,
                     int bufferLength, String buffer);

    MqCallResult close(int hconn, int hobj, int options);

    record MqOpenResult(int hobj, int conditionCode, int reasonCode) {
    }

    /** {@code data} holds the bytes MQ copied into the buffer (at most bufferLength), or "" if none. */
    record MqGetResult(String data, int dataLength, int conditionCode, int reasonCode) {
    }

    record MqCallResult(int conditionCode, int reasonCode) {
    }
}
