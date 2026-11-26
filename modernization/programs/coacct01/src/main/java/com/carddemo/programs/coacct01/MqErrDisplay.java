package com.carddemo.programs.coacct01;

import com.carddemo.programs.coacct01.support.CobolText;

/**
 * MQ-ERR-DISPLAY (113 bytes): the diagnostic record written to CARD.DEMO.ERROR or DISPLAYed.
 * Fields persist between paragraphs exactly like the COBOL group (success paths update the codes).
 */
public final class MqErrDisplay {

    public static final int LENGTH = 113;

    private String errorPara;            // MQ-ERROR-PARA          PIC X(25)
    private String applReturnMessage;    // MQ-APPL-RETURN-MESSAGE PIC X(25)
    private long applConditionCode;      // MQ-APPL-CONDITION-CODE PIC 9(02)
    private long applReasonCode;         // MQ-APPL-REASON-CODE    PIC 9(05)
    private String applQueueName;        // MQ-APPL-QUEUE-NAME     PIC X(48)

    public MqErrDisplay() {
        initialize();
    }

    /** INITIALIZE MQ-ERR-DISPLAY: alphanumerics to SPACES, numerics to ZERO (FILLERs untouched). */
    public void initialize() {
        errorPara = CobolText.spaces(25);
        applReturnMessage = CobolText.spaces(25);
        applConditionCode = 0;
        applReasonCode = 0;
        applQueueName = CobolText.spaces(48);
    }

    public void errorPara(String v) {
        errorPara = CobolText.pic(v, 25);
    }

    public void applReturnMessage(String v) {
        applReturnMessage = CobolText.pic(v, 25);
    }

    /** STRING ... DELIMITED BY SIZE INTO MQ-APPL-RETURN-MESSAGE. */
    public void stringIntoApplReturnMessage(String... parts) {
        applReturnMessage = CobolText.stringInto(applReturnMessage, parts);
    }

    public void applConditionCode(long binary) {
        applConditionCode = CobolText.unsignedDisplay(binary, 2);
    }

    public void applReasonCode(long binary) {
        applReasonCode = CobolText.unsignedDisplay(binary, 5);
    }

    public void applQueueName(String v) {
        applQueueName = CobolText.pic(v, 48);
    }

    public String errorPara() {
        return errorPara;
    }

    public String applReturnMessage() {
        return applReturnMessage;
    }

    public long applConditionCode() {
        return applConditionCode;
    }

    public long applReasonCode() {
        return applReasonCode;
    }

    public String applQueueName() {
        return applQueueName;
    }

    /** The 113-byte group as moved to ERROR-MESSAGE or DISPLAYed. */
    public String render() {
        return errorPara + "  " + applReturnMessage + "  " + CobolText.digits(applConditionCode, 2)
                + "  " + CobolText.digits(applReasonCode, 5) + "  " + applQueueName;
    }
}
