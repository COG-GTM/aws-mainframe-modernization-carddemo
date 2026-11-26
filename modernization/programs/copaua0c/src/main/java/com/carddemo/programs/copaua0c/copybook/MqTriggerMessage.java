package com.carddemo.programs.copaua0c.copybook;

import com.carddemo.programs.copaua0c.support.Cobol;

/** CMQTML - 01 MQTM trigger message retrieved by EXEC CICS RETRIEVE (character fields only). */
public record MqTriggerMessage(
        String qName,         // MQTM-QNAME        PIC X(48)
        String processName,   // MQTM-PROCESSNAME  PIC X(48)
        String triggerData,   // MQTM-TRIGGERDATA  PIC X(64)
        int applType,         // MQTM-APPLTYPE     PIC S9(9) BINARY
        String applId,        // MQTM-APPLID       PIC X(256)
        String envData,       // MQTM-ENVDATA      PIC X(128)
        String userData) {    // MQTM-USERDATA     PIC X(128)

    public MqTriggerMessage {
        qName = Cobol.alpha(qName, 48);
        processName = Cobol.alpha(processName, 48);
        triggerData = Cobol.alpha(triggerData, 64);
        applId = Cobol.alpha(applId, 256);
        envData = Cobol.alpha(envData, 128);
        userData = Cobol.alpha(userData, 128);
    }

    public static MqTriggerMessage forQueue(String qName, String triggerData) {
        return new MqTriggerMessage(qName, "", triggerData, 0, "", "", "");
    }
}
