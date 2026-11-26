package com.carddemo.programs.coacct01.copybook;

import com.carddemo.programs.coacct01.support.CobolText;

/** CMQGMOV - MQGMO get-message options (MQ-GET-MESSAGE-OPTIONS), versions 1-3 with IBM defaults. */
public final class MqGetMessageOptions {

    public String strucId = "GMO ";                          // MQGMO-STRUCID       PIC X(4)
    public int version = 1;                                  // MQGMO-VERSION
    public int options = MqConstants.MQGMO_NO_WAIT;          // MQGMO-OPTIONS
    public int waitInterval = 0;                             // MQGMO-WAITINTERVAL  (milliseconds)
    public int signal1 = 0;                                  // MQGMO-SIGNAL1
    public int signal2 = 0;                                  // MQGMO-SIGNAL2
    public String resolvedQName = CobolText.spaces(48);      // MQGMO-RESOLVEDQNAME PIC X(48)
    public int matchOptions = MqConstants.MQMO_MATCH_MSG_ID
            + MqConstants.MQMO_MATCH_CORREL_ID;              // MQGMO-MATCHOPTIONS  (v2)
    public String groupStatus = " ";                         // MQGMO-GROUPSTATUS   PIC X (v2)
    public String segmentStatus = " ";                       // MQGMO-SEGMENTSTATUS PIC X (v2)
    public String segmentation = " ";                        // MQGMO-SEGMENTATION  PIC X (v2)
    public String reserved1 = " ";                           // MQGMO-RESERVED1     PIC X (v2)
    public String msgToken = "\0".repeat(16);                // MQGMO-MSGTOKEN      PIC X(16) (v3)
    public int returnedLength = MqConstants.MQRL_UNDEFINED;  // MQGMO-RETURNEDLENGTH (v3)
}
