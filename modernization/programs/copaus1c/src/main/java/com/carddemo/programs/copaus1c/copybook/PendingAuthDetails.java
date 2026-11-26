package com.carddemo.programs.copaus1c.copybook;

import java.math.BigDecimal;

/**
 * CIPAUDTY - IMS segment PAUTDTL1 (PENDING-AUTH-DETAILS), 200 bytes.
 * Alphanumeric fields hold their exact fixed-width content; COMP-3 amounts are BigDecimal scale 2.
 */
public final class PendingAuthDetails {

    public static final int LENGTH = 200;

    /** 88-levels on PA-MATCH-STATUS. */
    public enum MatchStatus {
        PA_MATCH_PENDING('P'), PA_MATCH_AUTH_DECLINED('D'), PA_MATCH_PENDING_EXPIRED('E'), PA_MATCHED_WITH_TRAN('M');

        public final char code;

        MatchStatus(char code) {
            this.code = code;
        }
    }

    /** 88-levels on PA-AUTH-FRAUD. */
    public enum FraudStatus {
        PA_FRAUD_CONFIRMED('F'), PA_FRAUD_REMOVED('R');

        public final char code;

        FraudStatus(char code) {
            this.code = code;
        }
    }

    private AuthorizationKey authorizationKey;      // PA-AUTHORIZATION-KEY (PA-AUTH-DATE-9C / PA-AUTH-TIME-9C)
    private String authOrigDate;                     // PIC X(06)
    private String authOrigTime;                     // PIC X(06)
    private String cardNum;                          // PIC X(16)
    private String authType;                         // PIC X(04)
    private String cardExpiryDate;                   // PIC X(04)
    private String messageType;                      // PIC X(06)
    private String messageSource;                    // PIC X(06)
    private String authIdCode;                       // PIC X(06)
    private String authRespCode;                     // PIC X(02)  88 PA-AUTH-APPROVED '00'
    private String authRespReason;                   // PIC X(04)
    private String processingCode;                   // PIC 9(06) DISPLAY - raw digits, may hold SPACES
    private BigDecimal transactionAmt;               // PIC S9(10)V99 COMP-3
    private BigDecimal approvedAmt;                  // PIC S9(10)V99 COMP-3
    private String merchantCategoryCode;             // PIC X(04)
    private String acqrCountryCode;                  // PIC X(03)
    private String posEntryMode;                     // PIC 9(02) DISPLAY - raw digits, may hold SPACES
    private String merchantId;                       // PIC X(15)
    private String merchantName;                     // PIC X(22)
    private String merchantCity;                     // PIC X(13)
    private String merchantState;                    // PIC X(02)
    private String merchantZip;                      // PIC X(09)
    private String transactionId;                    // PIC X(15)
    private char matchStatus;                        // PIC X(01)
    private char authFraud;                          // PIC X(01)
    private String fraudRptDate;                     // PIC X(08)
    private String filler;                           // FILLER PIC X(17) - kept, it is carried through REPL

    private PendingAuthDetails() {
    }

    /** Uninitialised WORKING-STORAGE (no VALUE clause) modelled as LOW-VALUES / zero. */
    public static PendingAuthDetails lowValues() {
        PendingAuthDetails d = new PendingAuthDetails();
        d.authorizationKey = AuthorizationKey.LOW_VALUES;
        d.authOrigDate = Cobol.lowValues(6);
        d.authOrigTime = Cobol.lowValues(6);
        d.cardNum = Cobol.lowValues(16);
        d.authType = Cobol.lowValues(4);
        d.cardExpiryDate = Cobol.lowValues(4);
        d.messageType = Cobol.lowValues(6);
        d.messageSource = Cobol.lowValues(6);
        d.authIdCode = Cobol.lowValues(6);
        d.authRespCode = Cobol.lowValues(2);
        d.authRespReason = Cobol.lowValues(4);
        d.processingCode = Cobol.lowValues(6);
        d.transactionAmt = BigDecimal.ZERO.setScale(2);
        d.approvedAmt = BigDecimal.ZERO.setScale(2);
        d.merchantCategoryCode = Cobol.lowValues(4);
        d.acqrCountryCode = Cobol.lowValues(3);
        d.posEntryMode = Cobol.lowValues(2);
        d.merchantId = Cobol.lowValues(15);
        d.merchantName = Cobol.lowValues(22);
        d.merchantCity = Cobol.lowValues(13);
        d.merchantState = Cobol.lowValues(2);
        d.merchantZip = Cobol.lowValues(9);
        d.transactionId = Cobol.lowValues(15);
        d.matchStatus = Cobol.LOW_VALUE;
        d.authFraud = Cobol.LOW_VALUE;
        d.fraudRptDate = Cobol.lowValues(8);
        d.filler = Cobol.lowValues(17);
        return d;
    }

    /** A record with every alphanumeric field SPACES and numerics zero (INITIALIZE semantics). */
    public static PendingAuthDetails blank() {
        PendingAuthDetails d = fromEbcdic(new byte[LENGTH]);
        d.authOrigDate = Cobol.spaces(6);
        d.authOrigTime = Cobol.spaces(6);
        d.cardNum = Cobol.spaces(16);
        d.authType = Cobol.spaces(4);
        d.cardExpiryDate = Cobol.spaces(4);
        d.messageType = Cobol.spaces(6);
        d.messageSource = Cobol.spaces(6);
        d.authIdCode = Cobol.spaces(6);
        d.authRespCode = Cobol.spaces(2);
        d.authRespReason = Cobol.spaces(4);
        d.merchantCategoryCode = Cobol.spaces(4);
        d.acqrCountryCode = Cobol.spaces(3);
        d.merchantId = Cobol.spaces(15);
        d.merchantName = Cobol.spaces(22);
        d.merchantCity = Cobol.spaces(13);
        d.merchantState = Cobol.spaces(2);
        d.merchantZip = Cobol.spaces(9);
        d.transactionId = Cobol.spaces(15);
        d.matchStatus = ' ';
        d.processingCode = "000000";
        d.posEntryMode = "00";
        d.authFraud = ' ';
        d.fraudRptDate = Cobol.spaces(8);
        d.filler = Cobol.spaces(17);
        d.authorizationKey = AuthorizationKey.of(0, 0);
        return d;
    }

    /** Decode the 200-byte EBCDIC segment image. */
    public static PendingAuthDetails fromEbcdic(byte[] rec) {
        if (rec.length != LENGTH) {
            throw new IllegalArgumentException("PAUTDTL1 segment must be 200 bytes, got " + rec.length);
        }
        PendingAuthDetails d = new PendingAuthDetails();
        byte[] key = new byte[AuthorizationKey.LENGTH];
        System.arraycopy(rec, 0, key, 0, key.length);
        d.authorizationKey = AuthorizationKey.ofBytes(key);
        d.authOrigDate = Cobol.readX(rec, 8, 6);
        d.authOrigTime = Cobol.readX(rec, 14, 6);
        d.cardNum = Cobol.readX(rec, 20, 16);
        d.authType = Cobol.readX(rec, 36, 4);
        d.cardExpiryDate = Cobol.readX(rec, 40, 4);
        d.messageType = Cobol.readX(rec, 44, 6);
        d.messageSource = Cobol.readX(rec, 50, 6);
        d.authIdCode = Cobol.readX(rec, 56, 6);
        d.authRespCode = Cobol.readX(rec, 62, 2);
        d.authRespReason = Cobol.readX(rec, 64, 4);
        d.processingCode = Cobol.readX(rec, 68, 6);
        d.transactionAmt = Cobol.readPacked(rec, 74, 7, 2);
        d.approvedAmt = Cobol.readPacked(rec, 81, 7, 2);
        d.merchantCategoryCode = Cobol.readX(rec, 88, 4);
        d.acqrCountryCode = Cobol.readX(rec, 92, 3);
        d.posEntryMode = Cobol.readX(rec, 95, 2);
        d.merchantId = Cobol.readX(rec, 97, 15);
        d.merchantName = Cobol.readX(rec, 112, 22);
        d.merchantCity = Cobol.readX(rec, 134, 13);
        d.merchantState = Cobol.readX(rec, 147, 2);
        d.merchantZip = Cobol.readX(rec, 149, 9);
        d.transactionId = Cobol.readX(rec, 158, 15);
        d.matchStatus = Cobol.readX(rec, 173, 1).charAt(0);
        d.authFraud = Cobol.readX(rec, 174, 1).charAt(0);
        d.fraudRptDate = Cobol.readX(rec, 175, 8);
        d.filler = Cobol.readX(rec, 183, 17);
        return d;
    }

    /** Encode to the 200-byte EBCDIC segment image (the PIC X(200) WS-FRAUD-AUTH-RECORD view). */
    public byte[] toEbcdic() {
        byte[] rec = new byte[LENGTH];
        System.arraycopy(authorizationKey.bytes(), 0, rec, 0, AuthorizationKey.LENGTH);
        Cobol.writeX(rec, 8, 6, authOrigDate);
        Cobol.writeX(rec, 14, 6, authOrigTime);
        Cobol.writeX(rec, 20, 16, cardNum);
        Cobol.writeX(rec, 36, 4, authType);
        Cobol.writeX(rec, 40, 4, cardExpiryDate);
        Cobol.writeX(rec, 44, 6, messageType);
        Cobol.writeX(rec, 50, 6, messageSource);
        Cobol.writeX(rec, 56, 6, authIdCode);
        Cobol.writeX(rec, 62, 2, authRespCode);
        Cobol.writeX(rec, 64, 4, authRespReason);
        Cobol.writeX(rec, 68, 6, processingCode);
        Cobol.writePacked(rec, 74, 7, 2, transactionAmt, 12);
        Cobol.writePacked(rec, 81, 7, 2, approvedAmt, 12);
        Cobol.writeX(rec, 88, 4, merchantCategoryCode);
        Cobol.writeX(rec, 92, 3, acqrCountryCode);
        Cobol.writeX(rec, 95, 2, posEntryMode);
        Cobol.writeX(rec, 97, 15, merchantId);
        Cobol.writeX(rec, 112, 22, merchantName);
        Cobol.writeX(rec, 134, 13, merchantCity);
        Cobol.writeX(rec, 147, 2, merchantState);
        Cobol.writeX(rec, 149, 9, merchantZip);
        Cobol.writeX(rec, 158, 15, transactionId);
        Cobol.writeX(rec, 173, 1, String.valueOf(matchStatus));
        Cobol.writeX(rec, 174, 1, String.valueOf(authFraud));
        Cobol.writeX(rec, 175, 8, fraudRptDate);
        Cobol.writeX(rec, 183, 17, filler);
        return rec;
    }

    public PendingAuthDetails copy() {
        PendingAuthDetails d = new PendingAuthDetails();
        d.authorizationKey = authorizationKey;
        d.authOrigDate = authOrigDate;
        d.authOrigTime = authOrigTime;
        d.cardNum = cardNum;
        d.authType = authType;
        d.cardExpiryDate = cardExpiryDate;
        d.messageType = messageType;
        d.messageSource = messageSource;
        d.authIdCode = authIdCode;
        d.authRespCode = authRespCode;
        d.authRespReason = authRespReason;
        d.processingCode = processingCode;
        d.transactionAmt = transactionAmt;
        d.approvedAmt = approvedAmt;
        d.merchantCategoryCode = merchantCategoryCode;
        d.acqrCountryCode = acqrCountryCode;
        d.posEntryMode = posEntryMode;
        d.merchantId = merchantId;
        d.merchantName = merchantName;
        d.merchantCity = merchantCity;
        d.merchantState = merchantState;
        d.merchantZip = merchantZip;
        d.transactionId = transactionId;
        d.matchStatus = matchStatus;
        d.authFraud = authFraud;
        d.fraudRptDate = fraudRptDate;
        d.filler = filler;
        return d;
    }

    // ---------------------------------------------------------------- 88-levels

    public boolean isAuthApproved() {
        return "00".equals(authRespCode);
    }

    public boolean isFraudConfirmed() {
        return authFraud == FraudStatus.PA_FRAUD_CONFIRMED.code;
    }

    public boolean isFraudRemoved() {
        return authFraud == FraudStatus.PA_FRAUD_REMOVED.code;
    }

    public void setFraudStatus(FraudStatus status) {
        this.authFraud = status.code;
    }

    public boolean hasMatchStatus(MatchStatus status) {
        return matchStatus == status.code;
    }

    // ---------------------------------------------------------------- accessors

    public AuthorizationKey getAuthorizationKey() { return authorizationKey; }
    public void setAuthorizationKey(AuthorizationKey v) { authorizationKey = v; }
    public int getAuthDate9c() { return authorizationKey.authDate9c(); }
    public long getAuthTime9c() { return authorizationKey.authTime9c(); }
    public String getAuthOrigDate() { return authOrigDate; }
    public void setAuthOrigDate(String v) { authOrigDate = Cobol.x(v, 6); }
    public String getAuthOrigTime() { return authOrigTime; }
    public void setAuthOrigTime(String v) { authOrigTime = Cobol.x(v, 6); }
    public String getCardNum() { return cardNum; }
    public void setCardNum(String v) { cardNum = Cobol.x(v, 16); }
    public String getAuthType() { return authType; }
    public void setAuthType(String v) { authType = Cobol.x(v, 4); }
    public String getCardExpiryDate() { return cardExpiryDate; }
    public void setCardExpiryDate(String v) { cardExpiryDate = Cobol.x(v, 4); }
    public String getMessageType() { return messageType; }
    public void setMessageType(String v) { messageType = Cobol.x(v, 6); }
    public String getMessageSource() { return messageSource; }
    public void setMessageSource(String v) { messageSource = Cobol.x(v, 6); }
    public String getAuthIdCode() { return authIdCode; }
    public void setAuthIdCode(String v) { authIdCode = Cobol.x(v, 6); }
    public String getAuthRespCode() { return authRespCode; }
    public void setAuthRespCode(String v) { authRespCode = Cobol.x(v, 2); }
    public String getAuthRespReason() { return authRespReason; }
    public void setAuthRespReason(String v) { authRespReason = Cobol.x(v, 4); }
    public long getProcessingCode() { return Cobol.displayValue(processingCode); }
    public String getProcessingCodeDisplay() { return processingCode; }
    public void setProcessingCode(long v) { processingCode = Cobol.nine(v, 6); }
    public void setProcessingCodeDisplay(String raw) { processingCode = Cobol.x(raw, 6); }
    public BigDecimal getTransactionAmt() { return transactionAmt; }
    public void setTransactionAmt(BigDecimal v) { transactionAmt = v.setScale(2, java.math.RoundingMode.DOWN); }
    public BigDecimal getApprovedAmt() { return approvedAmt; }
    public void setApprovedAmt(BigDecimal v) { approvedAmt = v.setScale(2, java.math.RoundingMode.DOWN); }
    public String getMerchantCategoryCode() { return merchantCategoryCode; }
    public void setMerchantCategoryCode(String v) { merchantCategoryCode = Cobol.x(v, 4); }
    public String getAcqrCountryCode() { return acqrCountryCode; }
    public void setAcqrCountryCode(String v) { acqrCountryCode = Cobol.x(v, 3); }
    public long getPosEntryMode() { return Cobol.displayValue(posEntryMode); }
    public String getPosEntryModeDisplay() { return posEntryMode; }
    public void setPosEntryMode(long v) { posEntryMode = Cobol.nine(v, 2); }
    public void setPosEntryModeDisplay(String raw) { posEntryMode = Cobol.x(raw, 2); }
    public String getMerchantId() { return merchantId; }
    public void setMerchantId(String v) { merchantId = Cobol.x(v, 15); }
    public String getMerchantName() { return merchantName; }
    public void setMerchantName(String v) { merchantName = Cobol.x(v, 22); }
    public String getMerchantCity() { return merchantCity; }
    public void setMerchantCity(String v) { merchantCity = Cobol.x(v, 13); }
    public String getMerchantState() { return merchantState; }
    public void setMerchantState(String v) { merchantState = Cobol.x(v, 2); }
    public String getMerchantZip() { return merchantZip; }
    public void setMerchantZip(String v) { merchantZip = Cobol.x(v, 9); }
    public String getTransactionId() { return transactionId; }
    public void setTransactionId(String v) { transactionId = Cobol.x(v, 15); }
    public char getMatchStatus() { return matchStatus; }
    public void setMatchStatus(char v) { matchStatus = v; }
    public char getAuthFraud() { return authFraud; }
    public void setAuthFraud(char v) { authFraud = v; }
    public String getFraudRptDate() { return fraudRptDate; }
    public void setFraudRptDate(String v) { fraudRptDate = Cobol.x(v, 8); }
    public String getFiller() { return filler; }
    public void setFiller(String v) { filler = Cobol.x(v, 17); }
}
