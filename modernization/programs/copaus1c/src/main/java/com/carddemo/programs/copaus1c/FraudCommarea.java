package com.carddemo.programs.copaus1c;

import com.carddemo.programs.copaus1c.copybook.Cobol;
import com.carddemo.programs.copaus1c.copybook.PendingAuthDetails;

/** WS-FRAUD-DATA - COMMAREA passed on EXEC CICS LINK PROGRAM('COPAUS2C'). */
public final class FraudCommarea {

    /** 88-levels on WS-FRD-ACTION. */
    public enum Action {
        WS_REPORT_FRAUD('F'), WS_REMOVE_FRAUD('R');

        public final char code;

        Action(char code) {
            this.code = code;
        }
    }

    private String acctId = Cobol.lowValues(11);                            // WS-FRD-ACCT-ID   9(11)
    private long custId;                                                    // WS-FRD-CUST-ID   9(09)
    private PendingAuthDetails fraudAuthRecord = PendingAuthDetails.lowValues(); // WS-FRAUD-AUTH-RECORD X(200)
    private char action = Cobol.LOW_VALUE;                                  // WS-FRD-ACTION        X(01)
    private char updateStatus = Cobol.LOW_VALUE;                            // WS-FRD-UPDATE-STATUS X(01)
    private String actMsg = Cobol.lowValues(50);                            // WS-FRD-ACT-MSG       X(50)

    public boolean isReportFraud() { return action == Action.WS_REPORT_FRAUD.code; }
    public boolean isRemoveFraud() { return action == Action.WS_REMOVE_FRAUD.code; }
    public boolean isUpdateSuccess() { return updateStatus == 'S'; }
    public boolean isUpdateFailed() { return updateStatus == 'F'; }
    public void setUpdateSuccess() { updateStatus = 'S'; }
    public void setUpdateFailed() { updateStatus = 'F'; }

    public String getAcctId() { return acctId; }
    public void setAcctId(String raw) { acctId = Cobol.x(raw, 11); }
    public long getCustId() { return custId; }
    public void setCustId(long v) { custId = Math.abs(v) % 1_000_000_000L; }
    public PendingAuthDetails getFraudAuthRecord() { return fraudAuthRecord; }
    public void setFraudAuthRecord(PendingAuthDetails v) { fraudAuthRecord = v.copy(); }
    public char getAction() { return action; }
    public void setAction(Action a) { action = a.code; }
    public char getUpdateStatus() { return updateStatus; }
    public void setUpdateStatus(char v) { updateStatus = v; }
    public String getActMsg() { return actMsg; }
    public void setActMsg(String v) { actMsg = Cobol.x(v, 50); }
}
