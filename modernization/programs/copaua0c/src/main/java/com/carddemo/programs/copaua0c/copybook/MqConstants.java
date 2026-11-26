package com.carddemo.programs.copaua0c.copybook;

/** CMQV - the IBM MQ constants referenced by COPAUA0C (values from the IBM MQ COBOL copybook). */
public final class MqConstants {

    public static final int MQCC_OK = 0;
    public static final int MQCC_WARNING = 1;
    public static final int MQCC_FAILED = 2;

    public static final int MQRC_NONE = 0;
    public static final int MQRC_NO_MSG_AVAILABLE = 2033;
    public static final int MQRC_TRUNCATED_MSG_ACCEPTED = 2079;
    public static final int MQRC_TRUNCATED_MSG_FAILED = 2080;

    public static final int MQOT_Q = 1;
    public static final int MQOO_INPUT_SHARED = 2;
    public static final int MQCO_NONE = 0;

    public static final int MQGMO_WAIT = 1;
    public static final int MQGMO_ACCEPT_TRUNCATED_MSG = 64;
    public static final int MQGMO_NO_SYNCPOINT = 4;
    public static final int MQGMO_FAIL_IF_QUIESCING = 8192;
    public static final int MQGMO_CONVERT = 16384;

    public static final int MQPMO_NO_SYNCPOINT = 4;
    public static final int MQPMO_DEFAULT_CONTEXT = 32;

    public static final int MQMT_REPLY = 2;
    public static final int MQPER_NOT_PERSISTENT = 0;

    public static final String MQFMT_STRING = "MQSTR   ";
    public static final String MQMI_NONE = "\0".repeat(24);
    public static final String MQCI_NONE = "\0".repeat(24);

    private MqConstants() {
    }
}
