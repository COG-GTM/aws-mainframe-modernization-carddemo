package com.carddemo.programs.coacct01.copybook;

import com.carddemo.programs.coacct01.support.CobolText;

import static com.carddemo.programs.coacct01.copybook.MqConstants.*;

/**
 * CMQMDV - MQMD message descriptor (MQ-MESSAGE-DESCRIPTOR), version 1 fields plus the version 2
 * extension, with the IBM default VALUEs. Mutable because MQGET/MQPUT update it in place.
 * Byte fields (MSGID, CORRELID, ACCOUNTINGTOKEN, GROUPID) are held as 1 char per byte.
 */
public final class MqMessageDescriptor {

    public String strucId = "MD  ";                         // MQMD-STRUCID         PIC X(4)
    public int version = 1;                                 // MQMD-VERSION         PIC S9(9) BINARY
    public int report = MQRO_NONE;                          // MQMD-REPORT
    public int msgType = MQMT_DATAGRAM;                     // MQMD-MSGTYPE
    public int expiry = MQEI_UNLIMITED;                     // MQMD-EXPIRY
    public int feedback = MQFB_NONE;                        // MQMD-FEEDBACK
    public int encoding = MQENC_NATIVE;                     // MQMD-ENCODING
    public int codedCharSetId = MQCCSI_Q_MGR;               // MQMD-CODEDCHARSETID
    public String format = MQFMT_NONE;                      // MQMD-FORMAT          PIC X(8)
    public int priority = MQPRI_PRIORITY_AS_Q_DEF;          // MQMD-PRIORITY
    public int persistence = MQPER_PERSISTENCE_AS_Q_DEF;    // MQMD-PERSISTENCE
    public String msgId = MQMI_NONE;                        // MQMD-MSGID           PIC X(24)
    public String correlId = MQCI_NONE;                     // MQMD-CORRELID        PIC X(24)
    public int backoutCount = 0;                            // MQMD-BACKOUTCOUNT
    public String replyToQ = CobolText.spaces(48);          // MQMD-REPLYTOQ        PIC X(48)
    public String replyToQMgr = CobolText.spaces(48);       // MQMD-REPLYTOQMGR     PIC X(48)
    public String userIdentifier = CobolText.spaces(12);    // MQMD-USERIDENTIFIER  PIC X(12)
    public String accountingToken = "\0".repeat(32);        // MQMD-ACCOUNTINGTOKEN PIC X(32)
    public String applIdentityData = CobolText.spaces(32);  // MQMD-APPLIDENTITYDATA PIC X(32)
    public int putApplType = 0;                             // MQMD-PUTAPPLTYPE
    public String putApplName = CobolText.spaces(28);       // MQMD-PUTAPPLNAME     PIC X(28)
    public String putDate = CobolText.spaces(8);            // MQMD-PUTDATE         PIC X(8)
    public String putTime = CobolText.spaces(8);            // MQMD-PUTTIME         PIC X(8)
    public String applOriginData = CobolText.spaces(4);     // MQMD-APPLORIGINDATA  PIC X(4)
    public String groupId = "\0".repeat(24);                // MQMD-GROUPID         PIC X(24)  (v2)
    public int msgSeqNumber = 1;                            // MQMD-MSGSEQNUMBER    (v2)
    public int offset = 0;                                  // MQMD-OFFSET          (v2)
    public int msgFlags = 0;                                // MQMD-MSGFLAGS        (v2)
    public int originalLength = -1;                         // MQMD-ORIGINALLENGTH  (v2)
}
