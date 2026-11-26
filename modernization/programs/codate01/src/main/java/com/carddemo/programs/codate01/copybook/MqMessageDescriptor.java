package com.carddemo.programs.codate01.copybook;

import static com.carddemo.programs.codate01.CobolFields.alnum;

/**
 * CMQMDV - IBM MQ message descriptor (01 MQ-MESSAGE-DESCRIPTOR), version-2 layout with the copybook's
 * initial VALUEs. Binary PIC S9(9) BINARY fields map to int; PIC X(n) fields keep their exact length.
 */
public final class MqMessageDescriptor {

    public static final int LENGTH = 364;

    /** MQMD-STRUCID PIC X(4) VALUE 'MD  '. */
    public String strucId = "MD  ";
    /** MQMD-VERSION PIC S9(9) BINARY VALUE 1. */
    public int version = 1;
    /** MQMD-REPORT PIC S9(9) BINARY VALUE 0. */
    public int report = 0;
    /** MQMD-MSGTYPE PIC S9(9) BINARY VALUE 8. */
    public int msgType = MqConstants.MQMT_DATAGRAM;
    /** MQMD-EXPIRY PIC S9(9) BINARY VALUE -1. */
    public int expiry = MqConstants.MQEI_UNLIMITED;
    /** MQMD-FEEDBACK PIC S9(9) BINARY VALUE 0. */
    public int feedback = 0;
    /** MQMD-ENCODING PIC S9(9) BINARY VALUE 785. */
    public int encoding = MqConstants.MQENC_NATIVE;
    /** MQMD-CODEDCHARSETID PIC S9(9) BINARY VALUE 0. */
    public int codedCharSetId = MqConstants.MQCCSI_Q_MGR;
    /** MQMD-FORMAT PIC X(8) VALUE SPACES. */
    private String format = MqConstants.MQFMT_NONE;
    /** MQMD-PRIORITY PIC S9(9) BINARY VALUE -1. */
    public int priority = MqConstants.MQPRI_PRIORITY_AS_Q_DEF;
    /** MQMD-PERSISTENCE PIC S9(9) BINARY VALUE 2. */
    public int persistence = MqConstants.MQPER_PERSISTENCE_AS_Q_DEF;
    /** MQMD-MSGID PIC X(24) VALUE LOW-VALUES. */
    private String msgId = MqConstants.MQMI_NONE;
    /** MQMD-CORRELID PIC X(24) VALUE LOW-VALUES. */
    private String correlId = MqConstants.MQCI_NONE;
    /** MQMD-BACKOUTCOUNT PIC S9(9) BINARY VALUE 0. */
    public int backoutCount = 0;
    /** MQMD-REPLYTOQ PIC X(48) VALUE SPACES. */
    private String replyToQ = alnum("", 48);
    /** MQMD-REPLYTOQMGR PIC X(48) VALUE SPACES. */
    private String replyToQMgr = alnum("", 48);
    /** MQMD-USERIDENTIFIER PIC X(12) VALUE SPACES. */
    private String userIdentifier = alnum("", 12);
    /** MQMD-ACCOUNTINGTOKEN PIC X(32) VALUE LOW-VALUES. */
    private String accountingToken = MqConstants.MQACT_NONE;
    /** MQMD-APPLIDENTITYDATA PIC X(32) VALUE SPACES. */
    private String applIdentityData = alnum("", 32);
    /** MQMD-PUTAPPLTYPE PIC S9(9) BINARY VALUE 0. */
    public int putApplType = 0;
    /** MQMD-PUTAPPLNAME PIC X(28) VALUE SPACES. */
    private String putApplName = alnum("", 28);
    /** MQMD-PUTDATE PIC X(8) VALUE SPACES. */
    private String putDate = alnum("", 8);
    /** MQMD-PUTTIME PIC X(8) VALUE SPACES. */
    private String putTime = alnum("", 8);
    /** MQMD-APPLORIGINDATA PIC X(4) VALUE SPACES. */
    private String applOriginData = alnum("", 4);
    /** MQMD-GROUPID PIC X(24) VALUE LOW-VALUES. */
    private String groupId = MqConstants.MQGI_NONE;
    /** MQMD-MSGSEQNUMBER PIC S9(9) BINARY VALUE 1. */
    public int msgSeqNumber = 1;
    /** MQMD-OFFSET PIC S9(9) BINARY VALUE 0. */
    public int offset = 0;
    /** MQMD-MSGFLAGS PIC S9(9) BINARY VALUE 0. */
    public int msgFlags = 0;
    /** MQMD-ORIGINALLENGTH PIC S9(9) BINARY VALUE -1. */
    public int originalLength = MqConstants.MQOL_UNDEFINED;

    public String format() { return format; }
    public void format(String v) { format = alnum(v, 8); }
    public String msgId() { return msgId; }
    public void msgId(String v) { msgId = alnum(v, 24); }
    public String correlId() { return correlId; }
    public void correlId(String v) { correlId = alnum(v, 24); }
    public String replyToQ() { return replyToQ; }
    public void replyToQ(String v) { replyToQ = alnum(v, 48); }
    public String replyToQMgr() { return replyToQMgr; }
    public void replyToQMgr(String v) { replyToQMgr = alnum(v, 48); }
    public String userIdentifier() { return userIdentifier; }
    public void userIdentifier(String v) { userIdentifier = alnum(v, 12); }
    public String accountingToken() { return accountingToken; }
    public void accountingToken(String v) { accountingToken = alnum(v, 32); }
    public String applIdentityData() { return applIdentityData; }
    public void applIdentityData(String v) { applIdentityData = alnum(v, 32); }
    public String putApplName() { return putApplName; }
    public void putApplName(String v) { putApplName = alnum(v, 28); }
    public String putDate() { return putDate; }
    public void putDate(String v) { putDate = alnum(v, 8); }
    public String putTime() { return putTime; }
    public void putTime(String v) { putTime = alnum(v, 8); }
    public String applOriginData() { return applOriginData; }
    public void applOriginData(String v) { applOriginData = alnum(v, 4); }
    public String groupId() { return groupId; }
    public void groupId(String v) { groupId = alnum(v, 24); }

    public MqMessageDescriptor copy() {
        return new MqMessageDescriptor().copyFrom(this);
    }

    /** Group MOVE of another MQMD into this one; returns {@code this}. */
    public MqMessageDescriptor copyFrom(MqMessageDescriptor o) {
        MqMessageDescriptor c = this;
        c.strucId = o.strucId;
        c.version = o.version;
        c.report = o.report;
        c.msgType = o.msgType;
        c.expiry = o.expiry;
        c.feedback = o.feedback;
        c.encoding = o.encoding;
        c.codedCharSetId = o.codedCharSetId;
        c.format = o.format;
        c.priority = o.priority;
        c.persistence = o.persistence;
        c.msgId = o.msgId;
        c.correlId = o.correlId;
        c.backoutCount = o.backoutCount;
        c.replyToQ = o.replyToQ;
        c.replyToQMgr = o.replyToQMgr;
        c.userIdentifier = o.userIdentifier;
        c.accountingToken = o.accountingToken;
        c.applIdentityData = o.applIdentityData;
        c.putApplType = o.putApplType;
        c.putApplName = o.putApplName;
        c.putDate = o.putDate;
        c.putTime = o.putTime;
        c.applOriginData = o.applOriginData;
        c.groupId = o.groupId;
        c.msgSeqNumber = o.msgSeqNumber;
        c.offset = o.offset;
        c.msgFlags = o.msgFlags;
        c.originalLength = o.originalLength;
        return c;
    }
}
