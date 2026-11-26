package com.carddemo.programs.copaua0c.copybook;

import static com.carddemo.programs.copaua0c.support.Cobol.alpha;
import static com.carddemo.programs.copaua0c.support.Cobol.numericDisplay;
import static com.carddemo.programs.copaua0c.support.Cobol.spaces;

import com.carddemo.programs.copaua0c.support.Cobol;
import java.math.BigDecimal;

/**
 * CCPAURQY - 01 PENDING-AUTH-REQUEST (MQ request message, comma-delimited on the wire).
 * Mutable because COPAUA0C keeps it in WORKING-STORAGE across messages.
 */
public final class PendingAuthRequest {

    public static final int AUTH_DATE_LEN = 6;
    public static final int AUTH_TIME_LEN = 6;
    public static final int CARD_NUM_LEN = 16;
    public static final int AUTH_TYPE_LEN = 4;
    public static final int CARD_EXPIRY_DATE_LEN = 4;
    public static final int MESSAGE_TYPE_LEN = 6;
    public static final int MESSAGE_SOURCE_LEN = 6;
    public static final int PROCESSING_CODE_LEN = 6;
    public static final int TRANSACTION_AMT_LEN = 14;
    public static final int MERCHANT_CATAGORY_CODE_LEN = 4;
    public static final int ACQR_COUNTRY_CODE_LEN = 3;
    public static final int POS_ENTRY_MODE_LEN = 2;
    public static final int MERCHANT_ID_LEN = 15;
    public static final int MERCHANT_NAME_LEN = 22;
    public static final int MERCHANT_CITY_LEN = 13;
    public static final int MERCHANT_STATE_LEN = 2;
    public static final int MERCHANT_ZIP_LEN = 9;
    public static final int TRANSACTION_ID_LEN = 15;

    private String authDate = spaces(AUTH_DATE_LEN);                       // PIC X(06)
    private String authTime = spaces(AUTH_TIME_LEN);                       // PIC X(06)
    private String cardNum = spaces(CARD_NUM_LEN);                         // PIC X(16)
    private String authType = spaces(AUTH_TYPE_LEN);                       // PIC X(04)
    private String cardExpiryDate = spaces(CARD_EXPIRY_DATE_LEN);          // PIC X(04)
    private String messageType = spaces(MESSAGE_TYPE_LEN);                 // PIC X(06)
    private String messageSource = spaces(MESSAGE_SOURCE_LEN);             // PIC X(06)
    private String processingCode = numericDisplay("", PROCESSING_CODE_LEN); // PIC 9(06)
    private String transactionAmt = Cobol.editPlus10v2(BigDecimal.ZERO);   // PIC +9(10).99
    private String merchantCatagoryCode = spaces(MERCHANT_CATAGORY_CODE_LEN); // PIC X(04)
    private String acqrCountryCode = spaces(ACQR_COUNTRY_CODE_LEN);        // PIC X(03)
    private String posEntryMode = numericDisplay("", POS_ENTRY_MODE_LEN);  // PIC 9(02)
    private String merchantId = spaces(MERCHANT_ID_LEN);                   // PIC X(15)
    private String merchantName = spaces(MERCHANT_NAME_LEN);               // PIC X(22)
    private String merchantCity = spaces(MERCHANT_CITY_LEN);               // PIC X(13)
    private String merchantState = spaces(MERCHANT_STATE_LEN);             // PIC X(02)
    private String merchantZip = spaces(MERCHANT_ZIP_LEN);                 // PIC X(09)
    private String transactionId = spaces(TRANSACTION_ID_LEN);             // PIC X(15)

    public String authDate() { return authDate; }
    public void authDate(String v) { authDate = alpha(v, AUTH_DATE_LEN); }
    public String authTime() { return authTime; }
    public void authTime(String v) { authTime = alpha(v, AUTH_TIME_LEN); }
    public String cardNum() { return cardNum; }
    public void cardNum(String v) { cardNum = alpha(v, CARD_NUM_LEN); }
    public String authType() { return authType; }
    public void authType(String v) { authType = alpha(v, AUTH_TYPE_LEN); }
    public String cardExpiryDate() { return cardExpiryDate; }
    public void cardExpiryDate(String v) { cardExpiryDate = alpha(v, CARD_EXPIRY_DATE_LEN); }
    public String messageType() { return messageType; }
    public void messageType(String v) { messageType = alpha(v, MESSAGE_TYPE_LEN); }
    public String messageSource() { return messageSource; }
    public void messageSource(String v) { messageSource = alpha(v, MESSAGE_SOURCE_LEN); }
    public String processingCode() { return processingCode; }
    public void processingCode(String v) { processingCode = numericDisplay(v, PROCESSING_CODE_LEN); }

    /** The edited picture text, e.g. {@code +0000000125.50}. */
    public String transactionAmtEdited() { return transactionAmt; }
    public BigDecimal transactionAmt() { return Cobol.deEditPlus10v2(transactionAmt); }
    /** COMPUTE PA-RQ-TRANSACTION-AMT = ...: truncates to S9(10)V99 and edits. */
    public void transactionAmt(BigDecimal v) { transactionAmt = Cobol.editPlus10v2(v); }

    public String merchantCatagoryCode() { return merchantCatagoryCode; }
    public void merchantCatagoryCode(String v) { merchantCatagoryCode = alpha(v, MERCHANT_CATAGORY_CODE_LEN); }
    public String acqrCountryCode() { return acqrCountryCode; }
    public void acqrCountryCode(String v) { acqrCountryCode = alpha(v, ACQR_COUNTRY_CODE_LEN); }
    public String posEntryMode() { return posEntryMode; }
    public void posEntryMode(String v) { posEntryMode = numericDisplay(v, POS_ENTRY_MODE_LEN); }
    public String merchantId() { return merchantId; }
    public void merchantId(String v) { merchantId = alpha(v, MERCHANT_ID_LEN); }
    public String merchantName() { return merchantName; }
    public void merchantName(String v) { merchantName = alpha(v, MERCHANT_NAME_LEN); }
    public String merchantCity() { return merchantCity; }
    public void merchantCity(String v) { merchantCity = alpha(v, MERCHANT_CITY_LEN); }
    public String merchantState() { return merchantState; }
    public void merchantState(String v) { merchantState = alpha(v, MERCHANT_STATE_LEN); }
    public String merchantZip() { return merchantZip; }
    public void merchantZip(String v) { merchantZip = alpha(v, MERCHANT_ZIP_LEN); }
    public String transactionId() { return transactionId; }
    public void transactionId(String v) { transactionId = alpha(v, TRANSACTION_ID_LEN); }
}
