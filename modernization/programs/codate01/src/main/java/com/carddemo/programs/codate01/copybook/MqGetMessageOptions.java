package com.carddemo.programs.codate01.copybook;

import static com.carddemo.programs.codate01.CobolFields.alnum;

/** CMQGMOV - IBM MQ get-message options (01 MQ-GET-MESSAGE-OPTIONS), version-4 layout with initial VALUEs. */
public final class MqGetMessageOptions {

    /** MQGMO-STRUCID PIC X(4) VALUE 'GMO '. */
    public String strucId = "GMO ";
    /** MQGMO-VERSION PIC S9(9) BINARY VALUE 1. */
    public int version = 1;
    /** MQGMO-OPTIONS PIC S9(9) BINARY VALUE 0 (MQGMO-NO-WAIT). */
    public int options = MqConstants.MQGMO_NO_WAIT;
    /** MQGMO-WAITINTERVAL PIC S9(9) BINARY VALUE 0 (milliseconds). */
    public int waitInterval = 0;
    /** MQGMO-SIGNAL1 PIC S9(9) BINARY VALUE 0. */
    public int signal1 = 0;
    /** MQGMO-SIGNAL2 PIC S9(9) BINARY VALUE 0. */
    public int signal2 = 0;
    /** MQGMO-RESOLVEDQNAME PIC X(48) VALUE SPACES. */
    private String resolvedQName = alnum("", 48);
    /** MQGMO-MATCHOPTIONS PIC S9(9) BINARY VALUE 3. */
    public int matchOptions = MqConstants.MQMO_MATCH_MSG_ID + MqConstants.MQMO_MATCH_CORREL_ID;
    /** MQGMO-GROUPSTATUS PIC X VALUE ' ' (MQGS-NOT-IN-GROUP). */
    public char groupStatus = ' ';
    /** MQGMO-SEGMENTSTATUS PIC X VALUE ' ' (MQSS-NOT-A-SEGMENT). */
    public char segmentStatus = ' ';
    /** MQGMO-SEGMENTATION PIC X VALUE ' ' (MQSEG-INHIBITED). */
    public char segmentation = ' ';
    /** MQGMO-RESERVED1 PIC X VALUE ' '. */
    public char reserved1 = ' ';
    /** MQGMO-MSGTOKEN PIC X(16) VALUE LOW-VALUES. */
    private String msgToken = MqConstants.MQMTOK_NONE;
    /** MQGMO-RETURNEDLENGTH PIC S9(9) BINARY VALUE -1. */
    public int returnedLength = MqConstants.MQRL_UNDEFINED;
    /** MQGMO-RESERVED2 PIC S9(9) BINARY VALUE 0. */
    public int reserved2 = 0;
    /** MQGMO-MSGHANDLE PIC S9(18) BINARY VALUE 0. */
    public long msgHandle = 0;

    public String resolvedQName() { return resolvedQName; }
    public void resolvedQName(String v) { resolvedQName = alnum(v, 48); }
    public String msgToken() { return msgToken; }
    public void msgToken(String v) { msgToken = alnum(v, 16); }
}
