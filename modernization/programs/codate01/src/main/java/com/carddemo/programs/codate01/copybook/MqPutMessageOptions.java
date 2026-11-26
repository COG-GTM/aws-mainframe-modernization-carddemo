package com.carddemo.programs.codate01.copybook;

import static com.carddemo.programs.codate01.CobolFields.alnum;

/** CMQPMOV - IBM MQ put-message options (01 MQ-PUT-MESSAGE-OPTIONS), version-3 layout with initial VALUEs. */
public final class MqPutMessageOptions {

    /** MQPMO-STRUCID PIC X(4) VALUE 'PMO '. */
    public String strucId = "PMO ";
    /** MQPMO-VERSION PIC S9(9) BINARY VALUE 1. */
    public int version = 1;
    /** MQPMO-OPTIONS PIC S9(9) BINARY VALUE 0 (MQPMO-NONE). */
    public int options = MqConstants.MQPMO_NONE;
    /** MQPMO-TIMEOUT PIC S9(9) BINARY VALUE -1. */
    public int timeout = -1;
    /** MQPMO-CONTEXT PIC S9(9) BINARY VALUE 0. */
    public int context = 0;
    /** MQPMO-KNOWNDESTCOUNT PIC S9(9) BINARY VALUE 0. */
    public int knownDestCount = 0;
    /** MQPMO-UNKNOWNDESTCOUNT PIC S9(9) BINARY VALUE 0. */
    public int unknownDestCount = 0;
    /** MQPMO-INVALIDDESTCOUNT PIC S9(9) BINARY VALUE 0. */
    public int invalidDestCount = 0;
    /** MQPMO-RESOLVEDQNAME PIC X(48) VALUE SPACES. */
    private String resolvedQName = alnum("", 48);
    /** MQPMO-RESOLVEDQMGRNAME PIC X(48) VALUE SPACES. */
    private String resolvedQMgrName = alnum("", 48);
    /** MQPMO-RECSPRESENT PIC S9(9) BINARY VALUE 0. */
    public int recsPresent = 0;
    /** MQPMO-PUTMSGRECFIELDS PIC S9(9) BINARY VALUE 0. */
    public int putMsgRecFields = 0;
    /** MQPMO-PUTMSGRECOFFSET PIC S9(9) BINARY VALUE 0. */
    public int putMsgRecOffset = 0;
    /** MQPMO-RESPONSERECOFFSET PIC S9(9) BINARY VALUE 0. */
    public int responseRecOffset = 0;
    /** MQPMO-PUTMSGRECPTR POINTER VALUE NULL. */
    public long putMsgRecPtr = 0;
    /** MQPMO-RESPONSERECPTR POINTER VALUE NULL. */
    public long responseRecPtr = 0;
    /** MQPMO-ORIGINALMSGHANDLE PIC S9(18) BINARY VALUE 0. */
    public long originalMsgHandle = 0;
    /** MQPMO-NEWMSGHANDLE PIC S9(18) BINARY VALUE 0. */
    public long newMsgHandle = 0;
    /** MQPMO-ACTION PIC S9(9) BINARY VALUE 0 (MQACTP-NEW). */
    public int action = MqConstants.MQACTP_NEW;
    /** MQPMO-PUBLEVEL PIC S9(9) BINARY VALUE 9. */
    public int pubLevel = 9;

    public String resolvedQName() { return resolvedQName; }
    public void resolvedQName(String v) { resolvedQName = alnum(v, 48); }
    public String resolvedQMgrName() { return resolvedQMgrName; }
    public void resolvedQMgrName(String v) { resolvedQMgrName = alnum(v, 48); }
}
