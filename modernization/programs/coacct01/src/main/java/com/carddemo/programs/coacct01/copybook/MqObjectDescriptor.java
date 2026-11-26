package com.carddemo.programs.coacct01.copybook;

import com.carddemo.programs.coacct01.support.CobolText;

/** CMQODV - MQOD object descriptor (MQ-OBJECT-DESCRIPTOR), versions 1-3 with IBM defaults. */
public final class MqObjectDescriptor {

    public String strucId = "OD  ";                          // MQOD-STRUCID           PIC X(4)
    public int version = 1;                                  // MQOD-VERSION
    public int objectType = MqConstants.MQOT_Q;              // MQOD-OBJECTTYPE
    public String objectName = CobolText.spaces(48);         // MQOD-OBJECTNAME        PIC X(48)
    public String objectQMgrName = CobolText.spaces(48);     // MQOD-OBJECTQMGRNAME    PIC X(48)
    public String dynamicQName = CobolText.pic("CSQ.*", 48); // MQOD-DYNAMICQNAME      PIC X(48)
    public String alternateUserId = CobolText.spaces(12);    // MQOD-ALTERNATEUSERID   PIC X(12)
    public int recsPresent = 0;                              // MQOD-RECSPRESENT       (v2)
    public int knownDestCount = 0;                           // MQOD-KNOWNDESTCOUNT    (v2)
    public int unknownDestCount = 0;                         // MQOD-UNKNOWNDESTCOUNT  (v2)
    public int invalidDestCount = 0;                         // MQOD-INVALIDDESTCOUNT  (v2)
    public int objectRecOffset = 0;                          // MQOD-OBJECTRECOFFSET   (v2)
    public int responseRecOffset = 0;                        // MQOD-RESPONSERECOFFSET (v2)
    public long objectRecPtr = 0;                            // MQOD-OBJECTRECPTR      (v2)
    public long responseRecPtr = 0;                          // MQOD-RESPONSERECPTR    (v2)
    public String alternateSecurityId = "\0".repeat(40);     // MQOD-ALTERNATESECURITYID PIC X(40) (v3)
    public String resolvedQName = CobolText.spaces(48);      // MQOD-RESOLVEDQNAME     PIC X(48) (v3)
    public String resolvedQMgrName = CobolText.spaces(48);   // MQOD-RESOLVEDQMGRNAME  PIC X(48) (v3)
}
