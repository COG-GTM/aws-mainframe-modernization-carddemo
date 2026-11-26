package com.carddemo.programs.coactupc.copybook;

/**
 * Working-storage items of the including program that the edit-routine copybooks reference:
 * WS-EDIT-VARIABLE-NAME, the INPUT-ERROR flag and WS-RETURN-MSG (88 WS-RETURN-MSG-OFF).
 */
public interface EditMessages {

    String editVariableName();

    boolean isInputError();

    void setInputError();

    boolean isReturnMsgOff();

    /** STRING ... DELIMITED BY SIZE INTO WS-RETURN-MSG. */
    void stringIntoReturnMsg(String... parts);
}
