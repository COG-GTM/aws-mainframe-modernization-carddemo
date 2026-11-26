package com.carddemo.programs.copaua0c.copybook;

import static com.carddemo.programs.copaua0c.support.Cobol.alpha;
import static com.carddemo.programs.copaua0c.support.Cobol.spaces;

import com.carddemo.programs.copaua0c.support.Cobol;
import java.math.BigDecimal;

/** CCPAURLY - 01 PENDING-AUTH-RESPONSE. */
public final class PendingAuthResponse {

    public static final int CARD_NUM_LEN = 16;
    public static final int TRANSACTION_ID_LEN = 15;
    public static final int AUTH_ID_CODE_LEN = 6;
    public static final int AUTH_RESP_CODE_LEN = 2;
    public static final int AUTH_RESP_REASON_LEN = 4;

    private String cardNum = spaces(CARD_NUM_LEN);                 // PIC X(16)
    private String transactionId = spaces(TRANSACTION_ID_LEN);     // PIC X(15)
    private String authIdCode = spaces(AUTH_ID_CODE_LEN);          // PIC X(06)
    private String authRespCode = spaces(AUTH_RESP_CODE_LEN);      // PIC X(02)
    private String authRespReason = spaces(AUTH_RESP_REASON_LEN);  // PIC X(04)
    private String approvedAmt = Cobol.editPlus10v2(BigDecimal.ZERO); // PIC +9(10).99

    public String cardNum() { return cardNum; }
    public void cardNum(String v) { cardNum = alpha(v, CARD_NUM_LEN); }
    public String transactionId() { return transactionId; }
    public void transactionId(String v) { transactionId = alpha(v, TRANSACTION_ID_LEN); }
    public String authIdCode() { return authIdCode; }
    public void authIdCode(String v) { authIdCode = alpha(v, AUTH_ID_CODE_LEN); }
    public String authRespCode() { return authRespCode; }
    public void authRespCode(String v) { authRespCode = alpha(v, AUTH_RESP_CODE_LEN); }
    public String authRespReason() { return authRespReason; }
    public void authRespReason(String v) { authRespReason = alpha(v, AUTH_RESP_REASON_LEN); }
    public String approvedAmtEdited() { return approvedAmt; }
    public BigDecimal approvedAmt() { return Cobol.deEditPlus10v2(approvedAmt); }
    public void approvedAmt(BigDecimal v) { approvedAmt = Cobol.editPlus10v2(v); }
    /** MOVE PA-RQ-TRANSACTION-AMT TO PA-RL-APPROVED-AMT (same edited picture: byte copy). */
    public void approvedAmtEdited(String edited) { approvedAmt = edited; }
}
