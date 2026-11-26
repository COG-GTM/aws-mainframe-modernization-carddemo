package com.carddemo.programs.copaus2c.copybook;

import java.math.BigDecimal;

/**
 * AUTHFRDS.dcl - DCLGEN host structure DCLAUTHFRDS for table CARDDEMO.AUTHFRDS (26 columns).
 */
public record AuthFrdsRow(
        String cardNum,               // CARD-NUM               PIC X(16)        CHAR(16) NOT NULL
        String authTs,                // AUTH-TS                PIC X(26)        TIMESTAMP NOT NULL
        String authType,              // AUTH-TYPE              PIC X(4)
        String cardExpiryDate,        // CARD-EXPIRY-DATE       PIC X(4)
        String messageType,           // MESSAGE-TYPE           PIC X(6)
        String messageSource,         // MESSAGE-SOURCE         PIC X(6)
        String authIdCode,            // AUTH-ID-CODE           PIC X(6)
        String authRespCode,          // AUTH-RESP-CODE         PIC X(2)
        String authRespReason,        // AUTH-RESP-REASON       PIC X(4)
        String processingCode,        // PROCESSING-CODE        PIC X(6)
        BigDecimal transactionAmt,    // TRANSACTION-AMT        PIC S9(10)V9(2) COMP-3   DECIMAL(12,2)
        BigDecimal approvedAmt,       // APPROVED-AMT           PIC S9(10)V9(2) COMP-3   DECIMAL(12,2)
        String merchantCatagoryCode,  // MERCHANT-CATAGORY-CODE PIC X(4)
        String acqrCountryCode,       // ACQR-COUNTRY-CODE      PIC X(3)
        short posEntryMode,           // POS-ENTRY-MODE         PIC S9(4) COMP   SMALLINT
        String merchantId,            // MERCHANT-ID            PIC X(15)
        short merchantNameLen,        // MERCHANT-NAME-LEN      PIC S9(4) COMP   (49-level VARCHAR length)
        String merchantNameText,      // MERCHANT-NAME-TEXT     PIC X(22)        (49-level VARCHAR text)
        String merchantCity,          // MERCHANT-CITY          PIC X(13)
        String merchantState,         // MERCHANT-STATE         PIC X(2)
        String merchantZip,           // MERCHANT-ZIP           PIC X(9)
        String transactionId,         // TRANSACTION-ID         PIC X(15)
        String matchStatus,           // MATCH-STATUS           PIC X(1)
        String authFraud,             // AUTH-FRAUD             PIC X(1)
        String fraudRptDate,          // FRAUD-RPT-DATE         PIC X(10)        DATE
        long acctId,                  // ACCT-ID                PIC S9(11)V COMP-3   DECIMAL(11,0)
        long custId) {                // CUST-ID                PIC S9(9)V COMP-3    DECIMAL(9,0)

    public AuthFrdsRow {
        cardNum = Pic.x(cardNum, 16);
        authTs = Pic.x(authTs, 26);
        authType = Pic.x(authType, 4);
        cardExpiryDate = Pic.x(cardExpiryDate, 4);
        messageType = Pic.x(messageType, 6);
        messageSource = Pic.x(messageSource, 6);
        authIdCode = Pic.x(authIdCode, 6);
        authRespCode = Pic.x(authRespCode, 2);
        authRespReason = Pic.x(authRespReason, 4);
        processingCode = Pic.x(processingCode, 6);
        transactionAmt = Pic.decimal(transactionAmt == null ? BigDecimal.ZERO : transactionAmt, 10, 2);
        approvedAmt = Pic.decimal(approvedAmt == null ? BigDecimal.ZERO : approvedAmt, 10, 2);
        merchantCatagoryCode = Pic.x(merchantCatagoryCode, 4);
        acqrCountryCode = Pic.x(acqrCountryCode, 3);
        posEntryMode = (short) Pic.signed(posEntryMode, 4);
        merchantId = Pic.x(merchantId, 15);
        merchantNameLen = (short) Pic.signed(merchantNameLen, 4);
        merchantNameText = Pic.x(merchantNameText, 22);
        merchantCity = Pic.x(merchantCity, 13);
        merchantState = Pic.x(merchantState, 2);
        merchantZip = Pic.x(merchantZip, 9);
        transactionId = Pic.x(transactionId, 15);
        matchStatus = Pic.x(matchStatus, 1);
        authFraud = Pic.x(authFraud, 1);
        fraudRptDate = Pic.x(fraudRptDate, 10);
        acctId = Pic.signed(acctId, 11);
        custId = Pic.signed(custId, 9);
    }

    /** Value bound to the VARCHAR(22) MERCHANT_NAME column: the first MERCHANT-NAME-LEN characters. */
    public String merchantName() {
        int len = Math.max(0, Math.min(merchantNameLen, merchantNameText.length()));
        return merchantNameText.substring(0, len);
    }
}
