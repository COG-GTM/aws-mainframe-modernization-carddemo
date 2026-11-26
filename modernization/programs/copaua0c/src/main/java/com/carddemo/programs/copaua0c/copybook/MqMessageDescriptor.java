package com.carddemo.programs.copaua0c.copybook;

import com.carddemo.programs.copaua0c.support.Cobol;

/** CMQMDV - MQMD fields used by COPAUA0C (MQM-MD-REQUEST / MQM-MD-REPLY). */
public final class MqMessageDescriptor {

    private int msgType;
    private int expiry = -1;
    private String format = Cobol.spaces(8);
    private int persistence;
    private String msgId = MqConstants.MQMI_NONE;
    private String correlId = MqConstants.MQCI_NONE;
    private String replyToQ = Cobol.spaces(48);
    private String replyToQMgr = Cobol.spaces(48);

    public int msgType() { return msgType; }
    public void msgType(int v) { msgType = v; }
    public int expiry() { return expiry; }
    public void expiry(int v) { expiry = v; }
    public String format() { return format; }
    public void format(String v) { format = Cobol.alpha(v, 8); }
    public int persistence() { return persistence; }
    public void persistence(int v) { persistence = v; }
    public String msgId() { return msgId; }
    public void msgId(String v) { msgId = Cobol.alpha(v, 24); }
    public String correlId() { return correlId; }
    public void correlId(String v) { correlId = Cobol.alpha(v, 24); }
    public String replyToQ() { return replyToQ; }
    public void replyToQ(String v) { replyToQ = Cobol.alpha(v, 48); }
    public String replyToQMgr() { return replyToQMgr; }
    public void replyToQMgr(String v) { replyToQMgr = Cobol.alpha(v, 48); }

    public MqMessageDescriptor copy() {
        MqMessageDescriptor c = new MqMessageDescriptor();
        c.msgType = msgType;
        c.expiry = expiry;
        c.format = format;
        c.persistence = persistence;
        c.msgId = msgId;
        c.correlId = correlId;
        c.replyToQ = replyToQ;
        c.replyToQMgr = replyToQMgr;
        return c;
    }
}
