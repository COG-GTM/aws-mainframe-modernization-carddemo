package com.carddemo.interest.io.layout;

import com.carddemo.interest.io.RecordLayout;

/**
 * The five copybook record layouts consumed or produced by the interest-calculation sliver.
 *
 * <p>Each layout is a one-to-one transcription of a copybook under {@code app/cpy}; the field
 * names below are the COBOL data names in lower camel case, and the constants are the only place
 * in the module where record offsets exist.
 */
public final class CardDemoLayouts {

    private CardDemoLayouts() {
    }

    /**
     * {@code CVTRA01Y} — transaction category balance, {@code RECLN 50}
     * ({@code app/cpy/CVTRA01Y.cpy:4-10}). Input via DD {@code TCATBALF}.
     */
    public static final RecordLayout TRANSACTION_CATEGORY_BALANCE = RecordLayout
            .named("CVTRA01Y TRAN-CAT-BAL-RECORD")
            .unsignedNumber("accountId", 11)     // TRANCAT-ACCT-ID  PIC 9(11)
            .alphanumeric("typeCode", 2)         // TRANCAT-TYPE-CD  PIC X(02)
            .unsignedNumber("categoryCode", 4)   // TRANCAT-CD       PIC 9(04)
            .signedDecimal("balance", 11, 2)     // TRAN-CAT-BAL     PIC S9(09)V99
            .filler(22)
            .build(50);

    /**
     * {@code CVACT03Y} — card cross-reference, {@code RECLN 50}
     * ({@code app/cpy/CVACT03Y.cpy:4-8}). Input via DD {@code XREFFILE}.
     */
    public static final RecordLayout CARD_XREF = RecordLayout
            .named("CVACT03Y CARD-XREF-RECORD")
            .alphanumeric("cardNumber", 16)      // XREF-CARD-NUM  PIC X(16)
            .unsignedNumber("customerId", 9)     // XREF-CUST-ID   PIC 9(09)
            .unsignedNumber("accountId", 11)     // XREF-ACCT-ID   PIC 9(11)
            .filler(14)
            .build(50);

    /**
     * {@code CVTRA02Y} — disclosure group (interest rates), {@code RECLN 50}
     * ({@code app/cpy/CVTRA02Y.cpy:4-10}). Input via DD {@code DISCGRP}.
     */
    public static final RecordLayout DISCLOSURE_GROUP = RecordLayout
            .named("CVTRA02Y DIS-GROUP-RECORD")
            .alphanumeric("accountGroupId", 10)  // DIS-ACCT-GROUP-ID  PIC X(10)
            .alphanumeric("transactionTypeCode", 2) // DIS-TRAN-TYPE-CD PIC X(02)
            .unsignedNumber("transactionCategoryCode", 4) // DIS-TRAN-CAT-CD PIC 9(04)
            .signedDecimal("interestRate", 6, 2) // DIS-INT-RATE  PIC S9(04)V99
            .filler(28)
            .build(50);

    /**
     * {@code CVACT01Y} — account master, {@code RECLN 300}
     * ({@code app/cpy/CVACT01Y.cpy:4-17}). Input/output via DD {@code ACCTFILE}.
     */
    public static final RecordLayout ACCOUNT = RecordLayout
            .named("CVACT01Y ACCOUNT-RECORD")
            .unsignedNumber("accountId", 11)        // ACCT-ID                PIC 9(11)
            .alphanumeric("activeStatus", 1)        // ACCT-ACTIVE-STATUS     PIC X(01)
            .signedDecimal("currentBalance", 12, 2) // ACCT-CURR-BAL          PIC S9(10)V99
            .signedDecimal("creditLimit", 12, 2)    // ACCT-CREDIT-LIMIT      PIC S9(10)V99
            .signedDecimal("cashCreditLimit", 12, 2)// ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
            .alphanumeric("openDate", 10)           // ACCT-OPEN-DATE         PIC X(10)
            .alphanumeric("expirationDate", 10)     // ACCT-EXPIRAION-DATE    PIC X(10)
            .alphanumeric("reissueDate", 10)        // ACCT-REISSUE-DATE      PIC X(10)
            .signedDecimal("currentCycleCredit", 12, 2) // ACCT-CURR-CYC-CREDIT PIC S9(10)V99
            .signedDecimal("currentCycleDebit", 12, 2)  // ACCT-CURR-CYC-DEBIT  PIC S9(10)V99
            .alphanumeric("addressZip", 10)         // ACCT-ADDR-ZIP          PIC X(10)
            .alphanumeric("groupId", 10)            // ACCT-GROUP-ID          PIC X(10)
            .filler(178)
            .build(300);

    /**
     * {@code CVTRA05Y} — transaction record, {@code RECLN 350}
     * ({@code app/cpy/CVTRA05Y.cpy:4-18}). Output via DD {@code TRANSACT}.
     */
    public static final RecordLayout TRANSACTION = RecordLayout
            .named("CVTRA05Y TRAN-RECORD")
            .alphanumeric("transactionId", 16)   // TRAN-ID            PIC X(16)
            .alphanumeric("typeCode", 2)         // TRAN-TYPE-CD       PIC X(02)
            .unsignedNumber("categoryCode", 4)   // TRAN-CAT-CD        PIC 9(04)
            .alphanumeric("source", 10)          // TRAN-SOURCE        PIC X(10)
            .alphanumeric("description", 100)    // TRAN-DESC          PIC X(100)
            .signedDecimal("amount", 11, 2)      // TRAN-AMT           PIC S9(09)V99
            .unsignedNumber("merchantId", 9)     // TRAN-MERCHANT-ID   PIC 9(09)
            .alphanumeric("merchantName", 50)    // TRAN-MERCHANT-NAME PIC X(50)
            .alphanumeric("merchantCity", 50)    // TRAN-MERCHANT-CITY PIC X(50)
            .alphanumeric("merchantZip", 10)     // TRAN-MERCHANT-ZIP  PIC X(10)
            .alphanumeric("cardNumber", 16)      // TRAN-CARD-NUM      PIC X(16)
            .alphanumeric("originTimestamp", 26) // TRAN-ORIG-TS       PIC X(26)
            .alphanumeric("processTimestamp", 26)// TRAN-PROC-TS       PIC X(26)
            .filler(20)
            .build(350);
}
