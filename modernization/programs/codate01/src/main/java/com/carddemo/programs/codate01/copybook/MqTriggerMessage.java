package com.carddemo.programs.codate01.copybook;

import static com.carddemo.programs.codate01.CobolFields.alnum;

/**
 * CMQTML - IBM MQ trigger message (MQTM), the data CKTI passes to the triggered transaction and that
 * CODATE01 obtains with EXEC CICS RETRIEVE INTO(MQTM). Fixed length 684 bytes.
 *
 * @param strucId     MQTM-STRUCID PIC X(4) ('TM  ')
 * @param version     MQTM-VERSION PIC S9(9) BINARY
 * @param qName       MQTM-QNAME PIC X(48) - name of the triggered (input) queue
 * @param processName MQTM-PROCESSNAME PIC X(48)
 * @param triggerData MQTM-TRIGGERDATA PIC X(64)
 * @param applType    MQTM-APPLTYPE PIC S9(9) BINARY
 * @param applId      MQTM-APPLID PIC X(256)
 * @param envData     MQTM-ENVDATA PIC X(128)
 * @param userData    MQTM-USERDATA PIC X(128)
 */
public record MqTriggerMessage(String strucId, int version, String qName, String processName,
                               String triggerData, int applType, String applId, String envData,
                               String userData) {

    public static final int LENGTH = 684;
    /** MQAT-CICS. */
    public static final int MQAT_CICS = 1;

    public MqTriggerMessage {
        strucId = alnum(strucId, 4);
        qName = alnum(qName, 48);
        processName = alnum(processName, 48);
        triggerData = alnum(triggerData, 64);
        applId = alnum(applId, 256);
        envData = alnum(envData, 128);
        userData = alnum(userData, 128);
    }

    /** Trigger message as CKTI builds it for a CICS application started for {@code queueName}. */
    public static MqTriggerMessage forQueue(String queueName, String transactionId) {
        return new MqTriggerMessage("TM  ", 1, queueName, "", "", MQAT_CICS, transactionId, "", "");
    }
}
