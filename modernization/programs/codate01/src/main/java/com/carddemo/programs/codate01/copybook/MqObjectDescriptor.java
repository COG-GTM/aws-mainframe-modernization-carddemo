package com.carddemo.programs.codate01.copybook;

import static com.carddemo.programs.codate01.CobolFields.alnum;

/**
 * CMQODV - IBM MQ object descriptor (01 MQ-OBJECT-DESCRIPTOR), version-3 layout with initial VALUEs.
 * Version-4 MQCHARV fields (ObjectString, SelectionString, ResObjectString) and ResolvedType are
 * pointer-based and not used by CODATE01, so they are not modelled.
 */
public final class MqObjectDescriptor {

    /** MQOD-STRUCID PIC X(4) VALUE 'OD  '. */
    public String strucId = "OD  ";
    /** MQOD-VERSION PIC S9(9) BINARY VALUE 1. */
    public int version = 1;
    /** MQOD-OBJECTTYPE PIC S9(9) BINARY VALUE 1. */
    public int objectType = MqConstants.MQOT_Q;
    /** MQOD-OBJECTNAME PIC X(48) VALUE SPACES. */
    private String objectName = alnum("", 48);
    /** MQOD-OBJECTQMGRNAME PIC X(48) VALUE SPACES. */
    private String objectQMgrName = alnum("", 48);
    /** MQOD-DYNAMICQNAME PIC X(48) VALUE 'CSQ.*'. */
    private String dynamicQName = alnum("CSQ.*", 48);
    /** MQOD-ALTERNATEUSERID PIC X(12) VALUE SPACES. */
    private String alternateUserId = alnum("", 12);
    /** MQOD-RECSPRESENT PIC S9(9) BINARY VALUE 0. */
    public int recsPresent = 0;
    /** MQOD-KNOWNDESTCOUNT PIC S9(9) BINARY VALUE 0. */
    public int knownDestCount = 0;
    /** MQOD-UNKNOWNDESTCOUNT PIC S9(9) BINARY VALUE 0. */
    public int unknownDestCount = 0;
    /** MQOD-INVALIDDESTCOUNT PIC S9(9) BINARY VALUE 0. */
    public int invalidDestCount = 0;
    /** MQOD-OBJECTRECOFFSET PIC S9(9) BINARY VALUE 0. */
    public int objectRecOffset = 0;
    /** MQOD-RESPONSERECOFFSET PIC S9(9) BINARY VALUE 0. */
    public int responseRecOffset = 0;
    /** MQOD-OBJECTRECPTR POINTER VALUE NULL. */
    public long objectRecPtr = 0;
    /** MQOD-RESPONSERECPTR POINTER VALUE NULL. */
    public long responseRecPtr = 0;
    /** MQOD-ALTERNATESECURITYID PIC X(40) VALUE LOW-VALUES. */
    private String alternateSecurityId = MqConstants.MQSID_NONE;
    /** MQOD-RESOLVEDQNAME PIC X(48) VALUE SPACES. */
    private String resolvedQName = alnum("", 48);
    /** MQOD-RESOLVEDQMGRNAME PIC X(48) VALUE SPACES. */
    private String resolvedQMgrName = alnum("", 48);

    public String objectName() { return objectName; }
    public void objectName(String v) { objectName = alnum(v, 48); }
    public String objectQMgrName() { return objectQMgrName; }
    public void objectQMgrName(String v) { objectQMgrName = alnum(v, 48); }
    public String dynamicQName() { return dynamicQName; }
    public void dynamicQName(String v) { dynamicQName = alnum(v, 48); }
    public String alternateUserId() { return alternateUserId; }
    public void alternateUserId(String v) { alternateUserId = alnum(v, 12); }
    public String alternateSecurityId() { return alternateSecurityId; }
    public void alternateSecurityId(String v) { alternateSecurityId = alnum(v, 40); }
    public String resolvedQName() { return resolvedQName; }
    public void resolvedQName(String v) { resolvedQName = alnum(v, 48); }
    public String resolvedQMgrName() { return resolvedQMgrName; }
    public void resolvedQMgrName(String v) { resolvedQMgrName = alnum(v, 48); }
}
