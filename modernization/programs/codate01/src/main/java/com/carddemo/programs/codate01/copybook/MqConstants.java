package com.carddemo.programs.codate01.copybook;

/**
 * CMQV (IBM MQ constants copybook, 01 MQ-CONSTANTS). Only the subset CODATE01 references plus the
 * completion/reason codes its error paths can observe. Values are the IBM MQ documented constants.
 */
public final class MqConstants {

    private MqConstants() {
    }

    // Completion codes (MQCC-*)
    public static final int MQCC_OK = 0;
    public static final int MQCC_WARNING = 1;
    public static final int MQCC_FAILED = 2;

    // Reason codes (MQRC-*)
    public static final int MQRC_NONE = 0;
    public static final int MQRC_GET_INHIBITED = 2016;
    public static final int MQRC_HOBJ_ERROR = 2019;
    public static final int MQRC_NO_MSG_AVAILABLE = 2033;
    public static final int MQRC_NOT_AUTHORIZED = 2035;
    public static final int MQRC_PUT_INHIBITED = 2051;
    public static final int MQRC_Q_FULL = 2053;
    public static final int MQRC_TRUNCATED_MSG_FAILED = 2080;
    public static final int MQRC_UNKNOWN_OBJECT_NAME = 2085;
    public static final int MQRC_Q_MGR_QUIESCING = 2161;

    // Open options (MQOO-*)
    public static final int MQOO_INPUT_SHARED = 2;
    public static final int MQOO_OUTPUT = 16;
    public static final int MQOO_SAVE_ALL_CONTEXT = 128;
    public static final int MQOO_PASS_ALL_CONTEXT = 512;
    public static final int MQOO_FAIL_IF_QUIESCING = 8192;

    // Get-message options (MQGMO-*)
    public static final int MQGMO_NO_WAIT = 0;
    public static final int MQGMO_WAIT = 1;
    public static final int MQGMO_SYNCPOINT = 2;
    public static final int MQGMO_FAIL_IF_QUIESCING = 8192;
    public static final int MQGMO_CONVERT = 16384;

    // Put-message options (MQPMO-*)
    public static final int MQPMO_NONE = 0;
    public static final int MQPMO_SYNCPOINT = 2;
    public static final int MQPMO_DEFAULT_CONTEXT = 32;
    public static final int MQPMO_FAIL_IF_QUIESCING = 8192;

    // Close options (MQCO-*)
    public static final int MQCO_NONE = 0;

    // Match options (MQMO-*)
    public static final int MQMO_MATCH_MSG_ID = 1;
    public static final int MQMO_MATCH_CORREL_ID = 2;

    // Message types, persistence, object types, misc defaults
    public static final int MQMT_DATAGRAM = 8;
    public static final int MQPER_PERSISTENCE_AS_Q_DEF = 2;
    public static final int MQPRI_PRIORITY_AS_Q_DEF = -1;
    public static final int MQEI_UNLIMITED = -1;
    public static final int MQENC_NATIVE = 785;
    public static final int MQOT_Q = 1;
    public static final int MQWI_UNLIMITED = -1;
    public static final int MQRL_UNDEFINED = -1;
    public static final int MQOL_UNDEFINED = -1;
    public static final int MQACTP_NEW = 0;
    public static final int MQPRT_RESPONSE_AS_PARENT = 0;
    public static final int MQHO_UNUSABLE_HOBJ = -1;

    /** MQCCSI-Q-MGR: use the queue manager's coded character set. */
    public static final int MQCCSI_Q_MGR = 0;

    /** MQFMT-STRING PIC X(8) VALUE 'MQSTR   '. */
    public static final String MQFMT_STRING = "MQSTR   ";
    /** MQFMT-NONE PIC X(8) VALUE SPACES. */
    public static final String MQFMT_NONE = "        ";

    /** MQMI-NONE PIC X(24) VALUE LOW-VALUES. */
    public static final String MQMI_NONE = "\0".repeat(24);
    /** MQCI-NONE PIC X(24) VALUE LOW-VALUES. */
    public static final String MQCI_NONE = "\0".repeat(24);
    /** MQGI-NONE PIC X(24) VALUE LOW-VALUES. */
    public static final String MQGI_NONE = "\0".repeat(24);
    /** MQACT-NONE PIC X(32) VALUE LOW-VALUES. */
    public static final String MQACT_NONE = "\0".repeat(32);
    /** MQMTOK-NONE PIC X(16) VALUE LOW-VALUES. */
    public static final String MQMTOK_NONE = "\0".repeat(16);
    /** MQSID-NONE PIC X(40) VALUE LOW-VALUES. */
    public static final String MQSID_NONE = "\0".repeat(40);
}
