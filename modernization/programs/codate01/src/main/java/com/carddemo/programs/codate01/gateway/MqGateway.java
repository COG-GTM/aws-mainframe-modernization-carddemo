package com.carddemo.programs.codate01.gateway;

import com.carddemo.programs.codate01.copybook.MqGetMessageOptions;
import com.carddemo.programs.codate01.copybook.MqMessageDescriptor;
import com.carddemo.programs.codate01.copybook.MqObjectDescriptor;
import com.carddemo.programs.codate01.copybook.MqPutMessageOptions;

/** The IBM MQ API calls CODATE01 makes (CALL 'MQOPEN' / 'MQGET' / 'MQPUT' / 'MQCLOSE'). */
public interface MqGateway {

    MqOpenResult mqOpen(int hconn, MqObjectDescriptor objectDescriptor, int options);

    /** On success the gateway fills the output fields of {@code messageDescriptor} (MsgId, CorrelId, ReplyToQ, ...). */
    MqGetResult mqGet(int hconn, int hobj, MqMessageDescriptor messageDescriptor,
                      MqGetMessageOptions getMessageOptions, int bufferLength);

    MqCallResult mqPut(int hconn, int hobj, MqMessageDescriptor messageDescriptor,
                       MqPutMessageOptions putMessageOptions, int bufferLength, String buffer);

    MqCallResult mqClose(int hconn, int hobj, int options);
}
