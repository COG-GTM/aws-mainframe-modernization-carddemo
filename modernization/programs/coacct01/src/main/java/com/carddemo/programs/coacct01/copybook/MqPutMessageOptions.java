package com.carddemo.programs.coacct01.copybook;

import com.carddemo.programs.coacct01.support.CobolText;

/** CMQPMOV - MQPMO put-message options (MQ-PUT-MESSAGE-OPTIONS), versions 1-2 with IBM defaults. */
public final class MqPutMessageOptions {

    public String strucId = "PMO ";                          // MQPMO-STRUCID          PIC X(4)
    public int version = 1;                                  // MQPMO-VERSION
    public int options = MqConstants.MQPMO_NONE;             // MQPMO-OPTIONS
    public int timeout = -1;                                 // MQPMO-TIMEOUT
    public int context = 0;                                  // MQPMO-CONTEXT (MQHOBJ)
    public int knownDestCount = 0;                           // MQPMO-KNOWNDESTCOUNT
    public int unknownDestCount = 0;                         // MQPMO-UNKNOWNDESTCOUNT
    public int invalidDestCount = 0;                         // MQPMO-INVALIDDESTCOUNT
    public String resolvedQName = CobolText.spaces(48);      // MQPMO-RESOLVEDQNAME    PIC X(48)
    public String resolvedQMgrName = CobolText.spaces(48);   // MQPMO-RESOLVEDQMGRNAME PIC X(48)
    public int recsPresent = 0;                              // MQPMO-RECSPRESENT      (v2)
    public int putMsgRecFields = 0;                          // MQPMO-PUTMSGRECFIELDS  (v2)
    public int putMsgRecOffset = 0;                          // MQPMO-PUTMSGRECOFFSET  (v2)
    public int responseRecOffset = 0;                        // MQPMO-RESPONSERECOFFSET (v2)
    public long putMsgRecPtr = 0;                            // MQPMO-PUTMSGRECPTR     (v2)
    public long responseRecPtr = 0;                          // MQPMO-RESPONSERECPTR   (v2)
}
