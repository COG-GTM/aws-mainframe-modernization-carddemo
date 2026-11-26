package com.carddemo.programs.copaua0c.copybook;

import static com.carddemo.programs.copaua0c.support.Cobol.alpha;
import static com.carddemo.programs.copaua0c.support.Cobol.numericDisplay;
import static com.carddemo.programs.copaua0c.support.Cobol.spaces;

import com.carddemo.programs.copaua0c.support.Cobol;
import java.math.BigDecimal;

/**
 * CIPAUDTY - 01 PENDING-AUTH-DETAILS, IMS child segment PAUTDTL1 (200 bytes incl. FILLER X(17)).
 * Mutable because COPAUA0C keeps the I/O area in WORKING-STORAGE across messages.
 */
public final class PendingAuthDetails {

    public static final int SEGMENT_LENGTH = 200;

    /** PA-MATCH-STATUS 88-levels. */
    public enum MatchStatus {
        PENDING('P'), AUTH_DECLINED('D'), PENDING_EXPIRED('E'), MATCHED_WITH_TRAN('M');

        private final char code;

        MatchStatus(char code) { this.code = code; }

        public char code() { return code; }
    }

    /** PA-AUTH-FRAUD 88-levels. */
    public enum AuthFraud {
        CONFIRMED('F'), REMOVED('R');

        private final char code;

        AuthFraud(char code) { this.code = code; }

        public char code() { return code; }
    }

    // 05 PA-AUTHORIZATION-KEY
    private int authDate9c;                                   // PA-AUTH-DATE-9C  S9(05) COMP-3
    private long authTime9c;                                  // PA-AUTH-TIME-9C  S9(09) COMP-3
    private String authOrigDate = spaces(6);                  // X(06)
    private String authOrigTime = spaces(6);                  // X(06)
    private String cardNum = spaces(16);                      // X(16)
    private String authType = spaces(4);                      // X(04)
    private String cardExpiryDate = spaces(4);                // X(04)
    private String messageType = spaces(6);                   // X(06)
    private String messageSource = spaces(6);                 // X(06)
    private String authIdCode = spaces(6);                    // X(06)
    private String authRespCode = spaces(2);                  // X(02)  88 PA-AUTH-APPROVED '00'
    private String authRespReason = spaces(4);                // X(04)
    private String processingCode = numericDisplay("", 6);    // 9(06)
    private BigDecimal transactionAmt = BigDecimal.ZERO.setScale(2); // S9(10)V99 COMP-3
    private BigDecimal approvedAmt = BigDecimal.ZERO.setScale(2);    // S9(10)V99 COMP-3
    private String merchantCatagoryCode = spaces(4);          // X(04)
    private String acqrCountryCode = spaces(3);               // X(03)
    private String posEntryMode = numericDisplay("", 2);      // 9(02)
    private String merchantId = spaces(15);                   // X(15)
    private String merchantName = spaces(22);                 // X(22)
    private String merchantCity = spaces(13);                 // X(13)
    private String merchantState = spaces(2);                 // X(02)
    private String merchantZip = spaces(9);                   // X(09)
    private String transactionId = spaces(15);                // X(15)
    private char matchStatus = ' ';                           // X(01)
    private char authFraud = ' ';                             // X(01)
    private String fraudRptDate = spaces(8);                  // X(08)

    public int authDate9c() { return authDate9c; }
    public void authDate9c(long v) { authDate9c = (int) (v % 100_000L); }
    public long authTime9c() { return authTime9c; }
    public void authTime9c(long v) { authTime9c = v % 1_000_000_000L; }
    public String authOrigDate() { return authOrigDate; }
    public void authOrigDate(String v) { authOrigDate = alpha(v, 6); }
    public String authOrigTime() { return authOrigTime; }
    public void authOrigTime(String v) { authOrigTime = alpha(v, 6); }
    public String cardNum() { return cardNum; }
    public void cardNum(String v) { cardNum = alpha(v, 16); }
    public String authType() { return authType; }
    public void authType(String v) { authType = alpha(v, 4); }
    public String cardExpiryDate() { return cardExpiryDate; }
    public void cardExpiryDate(String v) { cardExpiryDate = alpha(v, 4); }
    public String messageType() { return messageType; }
    public void messageType(String v) { messageType = alpha(v, 6); }
    public String messageSource() { return messageSource; }
    public void messageSource(String v) { messageSource = alpha(v, 6); }
    public String authIdCode() { return authIdCode; }
    public void authIdCode(String v) { authIdCode = alpha(v, 6); }
    public String authRespCode() { return authRespCode; }
    public void authRespCode(String v) { authRespCode = alpha(v, 2); }
    public boolean isAuthApproved() { return "00".equals(authRespCode); }
    public String authRespReason() { return authRespReason; }
    public void authRespReason(String v) { authRespReason = alpha(v, 4); }
    public String processingCode() { return processingCode; }
    public void processingCode(String v) { processingCode = numericDisplay(v, 6); }
    public BigDecimal transactionAmt() { return transactionAmt; }
    public void transactionAmt(BigDecimal v) { transactionAmt = Cobol.truncate(v, 10, 2); }
    public BigDecimal approvedAmt() { return approvedAmt; }
    public void approvedAmt(BigDecimal v) { approvedAmt = Cobol.truncate(v, 10, 2); }
    public String merchantCatagoryCode() { return merchantCatagoryCode; }
    public void merchantCatagoryCode(String v) { merchantCatagoryCode = alpha(v, 4); }
    public String acqrCountryCode() { return acqrCountryCode; }
    public void acqrCountryCode(String v) { acqrCountryCode = alpha(v, 3); }
    public String posEntryMode() { return posEntryMode; }
    public void posEntryMode(String v) { posEntryMode = numericDisplay(v, 2); }
    public String merchantId() { return merchantId; }
    public void merchantId(String v) { merchantId = alpha(v, 15); }
    public String merchantName() { return merchantName; }
    public void merchantName(String v) { merchantName = alpha(v, 22); }
    public String merchantCity() { return merchantCity; }
    public void merchantCity(String v) { merchantCity = alpha(v, 13); }
    public String merchantState() { return merchantState; }
    public void merchantState(String v) { merchantState = alpha(v, 2); }
    public String merchantZip() { return merchantZip; }
    public void merchantZip(String v) { merchantZip = alpha(v, 9); }
    public String transactionId() { return transactionId; }
    public void transactionId(String v) { transactionId = alpha(v, 15); }
    public char matchStatusCode() { return matchStatus; }
    public void matchStatus(MatchStatus v) { matchStatus = v.code(); }
    public boolean isMatchPending() { return matchStatus == MatchStatus.PENDING.code(); }
    public boolean isMatchAuthDeclined() { return matchStatus == MatchStatus.AUTH_DECLINED.code(); }
    public boolean isMatchPendingExpired() { return matchStatus == MatchStatus.PENDING_EXPIRED.code(); }
    public boolean isMatchedWithTran() { return matchStatus == MatchStatus.MATCHED_WITH_TRAN.code(); }
    public char authFraudCode() { return authFraud; }
    public void authFraud(char v) { authFraud = v; }
    public boolean isFraudConfirmed() { return authFraud == AuthFraud.CONFIRMED.code(); }
    public boolean isFraudRemoved() { return authFraud == AuthFraud.REMOVED.code(); }
    public String fraudRptDate() { return fraudRptDate; }
    public void fraudRptDate(String v) { fraudRptDate = alpha(v, 8); }

    public PendingAuthDetails copy() {
        PendingAuthDetails c = new PendingAuthDetails();
        c.authDate9c = authDate9c;
        c.authTime9c = authTime9c;
        c.authOrigDate = authOrigDate;
        c.authOrigTime = authOrigTime;
        c.cardNum = cardNum;
        c.authType = authType;
        c.cardExpiryDate = cardExpiryDate;
        c.messageType = messageType;
        c.messageSource = messageSource;
        c.authIdCode = authIdCode;
        c.authRespCode = authRespCode;
        c.authRespReason = authRespReason;
        c.processingCode = processingCode;
        c.transactionAmt = transactionAmt;
        c.approvedAmt = approvedAmt;
        c.merchantCatagoryCode = merchantCatagoryCode;
        c.acqrCountryCode = acqrCountryCode;
        c.posEntryMode = posEntryMode;
        c.merchantId = merchantId;
        c.merchantName = merchantName;
        c.merchantCity = merchantCity;
        c.merchantState = merchantState;
        c.merchantZip = merchantZip;
        c.transactionId = transactionId;
        c.matchStatus = matchStatus;
        c.authFraud = authFraud;
        c.fraudRptDate = fraudRptDate;
        return c;
    }
}
