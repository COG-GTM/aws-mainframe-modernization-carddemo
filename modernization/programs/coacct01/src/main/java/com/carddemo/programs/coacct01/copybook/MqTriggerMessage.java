package com.carddemo.programs.coacct01.copybook;

import com.carddemo.programs.coacct01.support.CobolText;

/**
 * CMQTML - MQTM trigger message (MQ-GET-QUEUE-MESSAGE / MQTM), as passed by the CICS trigger
 * monitor (CKTI) and obtained with EXEC CICS RETRIEVE.
 *
 * @param strucId     MQTM-STRUCID     PIC X(4)   'TM  '
 * @param version     MQTM-VERSION     PIC S9(9) BINARY
 * @param qName       MQTM-QNAME       PIC X(48)
 * @param processName MQTM-PROCESSNAME PIC X(48)
 * @param triggerData MQTM-TRIGGERDATA PIC X(64)
 * @param applType    MQTM-APPLTYPE    PIC S9(9) BINARY
 * @param applId      MQTM-APPLID      PIC X(256)
 * @param envData     MQTM-ENVDATA     PIC X(128)
 * @param userData    MQTM-USERDATA    PIC X(128)
 */
public record MqTriggerMessage(
        String strucId,
        int version,
        String qName,
        String processName,
        String triggerData,
        int applType,
        String applId,
        String envData,
        String userData) {

    public MqTriggerMessage {
        strucId = CobolText.pic(strucId, 4);
        qName = CobolText.pic(qName, 48);
        processName = CobolText.pic(processName, 48);
        triggerData = CobolText.pic(triggerData, 64);
        applId = CobolText.pic(applId, 256);
        envData = CobolText.pic(envData, 128);
        userData = CobolText.pic(userData, 128);
    }

    /** Trigger message for a queue, other fields at their defaults. */
    public static MqTriggerMessage forQueue(String queueName) {
        return new MqTriggerMessage("TM  ", 1, queueName, "", "", MqConstants.MQAT_CICS, "", "", "");
    }
}
