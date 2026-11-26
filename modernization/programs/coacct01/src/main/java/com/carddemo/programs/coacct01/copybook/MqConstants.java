package com.carddemo.programs.coacct01.copybook;

/**
 * CMQV - IBM MQ named constants (MQ-CONSTANTS). IBM-supplied copybook, not in the repo: only the
 * constants COACCT01 references (plus the reason codes its error paths can observe) are carried.
 */
public final class MqConstants {

    private MqConstants() {
    }

    public static final int MQCC_OK = 0;
    public static final int MQCC_WARNING = 1;
    public static final int MQCC_FAILED = 2;

    public static final int MQRC_NONE = 0;
    public static final int MQRC_HOBJ_ERROR = 2019;
    public static final int MQRC_NO_MSG_AVAILABLE = 2033;
    public static final int MQRC_TRUNCATED_MSG_FAILED = 2080;
    public static final int MQRC_UNKNOWN_OBJECT_NAME = 2085;
    public static final int MQRC_Q_FULL = 2053;
    public static final int MQRC_CONNECTION_BROKEN = 2009;

    public static final int MQOO_INPUT_SHARED = 2;
    public static final int MQOO_OUTPUT = 16;
    public static final int MQOO_SAVE_ALL_CONTEXT = 128;
    public static final int MQOO_PASS_ALL_CONTEXT = 512;
    public static final int MQOO_FAIL_IF_QUIESCING = 8192;

    public static final int MQGMO_NO_WAIT = 0;
    public static final int MQGMO_WAIT = 1;
    public static final int MQGMO_SYNCPOINT = 2;
    public static final int MQGMO_FAIL_IF_QUIESCING = 8192;
    public static final int MQGMO_CONVERT = 16384;

    public static final int MQPMO_NONE = 0;
    public static final int MQPMO_SYNCPOINT = 2;
    public static final int MQPMO_DEFAULT_CONTEXT = 32;
    public static final int MQPMO_FAIL_IF_QUIESCING = 8192;

    public static final int MQCO_NONE = 0;

    public static final int MQOT_Q = 1;
    public static final int MQMT_DATAGRAM = 8;
    public static final int MQEI_UNLIMITED = -1;
    public static final int MQPRI_PRIORITY_AS_Q_DEF = -1;
    public static final int MQPER_PERSISTENCE_AS_Q_DEF = 2;
    public static final int MQRO_NONE = 0;
    public static final int MQFB_NONE = 0;
    public static final int MQENC_NATIVE = 785;
    public static final int MQCCSI_Q_MGR = 0;
    public static final int MQWI_UNLIMITED = -1;
    public static final int MQMO_MATCH_MSG_ID = 1;
    public static final int MQMO_MATCH_CORREL_ID = 2;
    public static final int MQRL_UNDEFINED = -1;
    public static final int MQAT_CICS = 1;

    /** MQMI-NONE: 24 binary zeros. */
    public static final String MQMI_NONE = "\0".repeat(24);
    /** MQCI-NONE: 24 binary zeros. */
    public static final String MQCI_NONE = "\0".repeat(24);
    public static final String MQFMT_NONE = "        ";
    public static final String MQFMT_STRING = "MQSTR   ";
}
