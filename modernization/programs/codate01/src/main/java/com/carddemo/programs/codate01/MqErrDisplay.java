package com.carddemo.programs.codate01;

import static com.carddemo.programs.codate01.CobolFields.alnum;
import static com.carddemo.programs.codate01.CobolFields.unsignedDisplay;

/**
 * WORKING-STORAGE 01 MQ-ERR-DISPLAY (113 bytes). Written to CARD.DEMO.ERROR by 9000-ERROR and
 * DISPLAYed when the error queue itself cannot be used.
 */
public final class MqErrDisplay {

    public static final int LENGTH = 113;
    private static final String FILLER = "  ";

    /** MQ-ERROR-PARA PIC X(25). */
    private String errorPara = alnum("", 25);
    /** MQ-APPL-RETURN-MESSAGE PIC X(25). */
    private String applReturnMessage = alnum("", 25);
    /** MQ-APPL-CONDITION-CODE PIC 9(02). */
    private String applConditionCode = "00";
    /** MQ-APPL-REASON-CODE PIC 9(05). */
    private String applReasonCode = "00000";
    /** MQ-APPL-QUEUE-NAME PIC X(48). */
    private String applQueueName = alnum("", 48);

    /** INITIALIZE MQ-ERR-DISPLAY (FILLERs keep VALUE SPACES). */
    public void initialize() {
        errorPara = alnum("", 25);
        applReturnMessage = alnum("", 25);
        applConditionCode = "00";
        applReasonCode = "00000";
        applQueueName = alnum("", 48);
    }

    public void errorPara(String v) { errorPara = alnum(v, 25); }
    public void applReturnMessage(String v) { applReturnMessage = alnum(v, 25); }
    /** STRING ... INTO MQ-APPL-RETURN-MESSAGE (no padding: untouched trailing bytes are kept). */
    public void stringIntoApplReturnMessage(String... parts) {
        applReturnMessage = CobolFields.stringInto(applReturnMessage, parts);
    }
    public void applConditionCode(int binary) { applConditionCode = unsignedDisplay(binary, 2); }
    public void applReasonCode(int binary) { applReasonCode = unsignedDisplay(binary, 5); }
    public void applQueueName(String v) { applQueueName = alnum(v, 48); }

    public String errorPara() { return errorPara; }
    public String applReturnMessage() { return applReturnMessage; }
    public String applConditionCode() { return applConditionCode; }
    public String applReasonCode() { return applReasonCode; }
    public String applQueueName() { return applQueueName; }

    public String render() {
        return errorPara + FILLER + applReturnMessage + FILLER + applConditionCode + FILLER
                + applReasonCode + FILLER + applQueueName;
    }

    @Override
    public String toString() {
        return render();
    }
}
