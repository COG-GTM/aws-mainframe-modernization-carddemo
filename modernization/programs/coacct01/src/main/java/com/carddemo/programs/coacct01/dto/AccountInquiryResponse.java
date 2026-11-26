package com.carddemo.programs.coacct01.dto;

import com.carddemo.programs.coacct01.copybook.AccountRecord;
import com.carddemo.programs.coacct01.support.CobolText;
import com.carddemo.programs.coacct01.support.ZonedDecimal;

import java.math.BigDecimal;

/**
 * WS-ACCT-RESPONSE - the labelled 252-byte reply built from ACCOUNT-RECORD. Amounts are
 * PIC S9(10)V99 DISPLAY, so they appear in the reply as zoned decimal with an overpunched sign.
 */
public record AccountInquiryResponse(
        long acctId,                 // WS-ACCT-ID                PIC 9(11)
        String activeStatus,         // WS-ACCT-ACTIVE-STATUS     PIC X(01)
        BigDecimal currBal,          // WS-ACCT-CURR-BAL          PIC S9(10)V99
        BigDecimal creditLimit,      // WS-ACCT-CREDIT-LIMIT      PIC S9(10)V99
        BigDecimal cashCreditLimit,  // WS-ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
        String openDate,             // WS-ACCT-OPEN-DATE         PIC X(10)
        String expirationDate,       // WS-ACCT-EXPIRAION-DATE    PIC X(10)
        String reissueDate,          // WS-ACCT-REISSUE-DATE      PIC X(10)
        BigDecimal currCycCredit,    // WS-ACCT-CURR-CYC-CREDIT   PIC S9(10)V99
        BigDecimal currCycDebit,     // WS-ACCT-CURR-CYC-DEBIT    PIC S9(10)V99
        String groupId) {            // WS-ACCT-GROUP-ID          PIC X(10)

    public static final String ACCT_LBL = "ACCOUNT ID : ";
    public static final String STATUS_LBL = "ACCOUNT STATUS : ";
    public static final String CURR_BAL_LBL = "BALANCE : ";
    public static final String CRDT_LMT_LBL = "CREDIT LIMIT : ";
    public static final String CASH_LIMIT_LBL = "CASH LIMIT : ";
    public static final String OPEN_DATE_LBL = "OPEN DATE : ";
    public static final String EXPR_DATE_LBL = "EXPR DATE : ";
    public static final String REISSUE_DT_LBL = "REIS DATE : ";
    public static final String CURR_CYC_CREDIT_LBL = "CREDIT BAL : ";
    public static final String CURR_CYC_DEBIT_LBL = "DEBIT BAL : ";
    public static final String ACCT_GRP_LBL = "GROUP ID : ";
    public static final int LENGTH = 252;

    public AccountInquiryResponse {
        acctId = CobolText.unsignedDisplay(acctId, 11);
        activeStatus = CobolText.pic(activeStatus, 1);
        currBal = ZonedDecimal.truncate(currBal, 10, 2);
        creditLimit = ZonedDecimal.truncate(creditLimit, 10, 2);
        cashCreditLimit = ZonedDecimal.truncate(cashCreditLimit, 10, 2);
        openDate = CobolText.pic(openDate, 10);
        expirationDate = CobolText.pic(expirationDate, 10);
        reissueDate = CobolText.pic(reissueDate, 10);
        currCycCredit = ZonedDecimal.truncate(currCycCredit, 10, 2);
        currCycDebit = ZonedDecimal.truncate(currCycDebit, 10, 2);
        groupId = CobolText.pic(groupId, 10);
    }

    /** Initial VALUEs: numerics ZEROES, alphanumerics SPACES. */
    public static AccountInquiryResponse initial() {
        return new AccountInquiryResponse(0, "", BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO,
                "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, "");
    }

    /** The MOVE ACCT-... TO WS-ACCT-... block under WHEN DFHRESP(NORMAL). */
    public static AccountInquiryResponse from(AccountRecord r) {
        return new AccountInquiryResponse(r.acctId(), r.activeStatus(), r.currBal(), r.creditLimit(),
                r.cashCreditLimit(), r.openDate(), r.expirationDate(), r.reissueDate(),
                r.currCycCredit(), r.currCycDebit(), r.groupId());
    }

    public String toMessage() {
        return ACCT_LBL + CobolText.digits(acctId, 11)
                + STATUS_LBL + activeStatus
                + CURR_BAL_LBL + ZonedDecimal.format(currBal, 10, 2)
                + CRDT_LMT_LBL + ZonedDecimal.format(creditLimit, 10, 2)
                + CASH_LIMIT_LBL + ZonedDecimal.format(cashCreditLimit, 10, 2)
                + OPEN_DATE_LBL + openDate
                + EXPR_DATE_LBL + expirationDate
                + REISSUE_DT_LBL + reissueDate
                + CURR_CYC_CREDIT_LBL + ZonedDecimal.format(currCycCredit, 10, 2)
                + CURR_CYC_DEBIT_LBL + ZonedDecimal.format(currCycDebit, 10, 2)
                + ACCT_GRP_LBL + groupId;
    }
}
