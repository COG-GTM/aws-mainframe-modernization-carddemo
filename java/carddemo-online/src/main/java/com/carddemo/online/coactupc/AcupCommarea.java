package com.carddemo.online.coactupc;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;

/**
 * COACTUPC {@code WS-THIS-PROGCOMMAREA} (lines 652-849, 873 bytes): the program's own commarea area, carried after
 * CARDDEMO-COMMAREA in WS-COMMAREA. {@link #OLD} holds the values fetched from the files, {@link #NEW} the values
 * received from the screen.
 */
final class AcupCommarea extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("WS-THIS-PROGCOMMAREA");

    static {
        B.group("ACCT-UPDATE-SCREEN-DATA");
    }

    /** ACUP-CHANGE-ACTION PIC X(1). */
    static final AlphanumericField CHANGE_ACTION = B.x("ACUP-CHANGE-ACTION", 1);

    static {
        B.endGroup();
    }

    /** ACUP-OLD-DETAILS. */
    static final Details OLD = new Details("ACUP-OLD-", false);
    /** ACUP-NEW-DETAILS. */
    static final Details NEW = new Details("ACUP-NEW-", true);

    static final RecordLayout LAYOUT = B.build();
    static final int LENGTH = LAYOUT.length();

    /** 88-levels on ACUP-CHANGE-ACTION. */
    static final String DETAILS_NOT_FETCHED = "\u0000";
    static final String SHOW_DETAILS = "S";
    static final String CHANGES_NOT_OK = "E";
    static final String CHANGES_OK_NOT_CONFIRMED = "N";
    static final String CHANGES_OKAYED_AND_DONE = "C";
    static final String CHANGES_OKAYED_LOCK_ERROR = "L";
    static final String CHANGES_OKAYED_BUT_FAILED = "F";

    AcupCommarea() {
        super(LENGTH);
    }

    /**
     * ACUP-OLD-DETAILS / ACUP-NEW-DETAILS (identical except ACUP-NEW-CUST-SSN-X, which is a group of three parts).
     * Fields ending in {@code N} are the PIC 9 / S9(10)V99 REDEFINES of the preceding PIC X item.
     */
    static final class Details {
        final AlphanumericField group;
        final AlphanumericField acctData;
        final AlphanumericField acctIdX;
        final NumericField acctId;
        final AlphanumericField activeStatus;
        final AlphanumericField currBal;
        final NumericField currBalN;
        final AlphanumericField creditLimit;
        final NumericField creditLimitN;
        final AlphanumericField cashCreditLimit;
        final NumericField cashCreditLimitN;
        final AlphanumericField openDate;
        final AlphanumericField openYear;
        final AlphanumericField openMon;
        final AlphanumericField openDay;
        final AlphanumericField expirationDate;
        final AlphanumericField expYear;
        final AlphanumericField expMon;
        final AlphanumericField expDay;
        final AlphanumericField reissueDate;
        final AlphanumericField reissueYear;
        final AlphanumericField reissueMon;
        final AlphanumericField reissueDay;
        final AlphanumericField currCycCredit;
        final NumericField currCycCreditN;
        final AlphanumericField currCycDebit;
        final NumericField currCycDebitN;
        final AlphanumericField groupId;
        final AlphanumericField custData;
        final AlphanumericField custIdX;
        final NumericField custId;
        final AlphanumericField firstName;
        final AlphanumericField middleName;
        final AlphanumericField lastName;
        final AlphanumericField addrLine1;
        final AlphanumericField addrLine2;
        final AlphanumericField addrLine3;
        final AlphanumericField stateCd;
        final AlphanumericField countryCd;
        final AlphanumericField zip;
        final AlphanumericField phone1;
        final AlphanumericField phone1a;
        final AlphanumericField phone1b;
        final AlphanumericField phone1c;
        final AlphanumericField phone2;
        final AlphanumericField phone2a;
        final AlphanumericField phone2b;
        final AlphanumericField phone2c;
        final AlphanumericField ssnX;
        final AlphanumericField ssn1;
        final AlphanumericField ssn2;
        final AlphanumericField ssn3;
        final NumericField ssn;
        final AlphanumericField govtIssuedId;
        final AlphanumericField dob;
        final AlphanumericField dobYear;
        final AlphanumericField dobMon;
        final AlphanumericField dobDay;
        final AlphanumericField eftAccountId;
        final AlphanumericField priHolderInd;
        final AlphanumericField ficoScoreX;
        final NumericField ficoScore;

        private Details(String p, boolean isNew) {
            B.group(p + "DETAILS");
            B.group(p + "ACCT-DATA");
            acctIdX = B.x(p + "ACCT-ID-X", 11);
            B.redefines(acctIdX);
            acctId = B.zoned(p + "ACCT-ID", 11, 0, false);
            activeStatus = B.x(p + "ACTIVE-STATUS", 1);
            currBal = B.x(p + "CURR-BAL", 12);
            B.redefines(currBal);
            currBalN = B.zoned(p + "CURR-BAL-N", 10, 2, true);
            creditLimit = B.x(p + "CREDIT-LIMIT", 12);
            B.redefines(creditLimit);
            creditLimitN = B.zoned(p + "CREDIT-LIMIT-N", 10, 2, true);
            cashCreditLimit = B.x(p + "CASH-CREDIT-LIMIT", 12);
            B.redefines(cashCreditLimit);
            cashCreditLimitN = B.zoned(p + "CASH-CREDIT-LIMIT-N", 10, 2, true);
            openDate = B.x(p + "OPEN-DATE", 8);
            B.redefines(openDate);
            openYear = B.x(p + "OPEN-YEAR", 4);
            openMon = B.x(p + "OPEN-MON", 2);
            openDay = B.x(p + "OPEN-DAY", 2);
            expirationDate = B.x(p + "EXPIRAION-DATE", 8);
            B.redefines(expirationDate);
            expYear = B.x(p + "EXP-YEAR", 4);
            expMon = B.x(p + "EXP-MON", 2);
            expDay = B.x(p + "EXP-DAY", 2);
            reissueDate = B.x(p + "REISSUE-DATE", 8);
            B.redefines(reissueDate);
            reissueYear = B.x(p + "REISSUE-YEAR", 4);
            reissueMon = B.x(p + "REISSUE-MON", 2);
            reissueDay = B.x(p + "REISSUE-DAY", 2);
            currCycCredit = B.x(p + "CURR-CYC-CREDIT", 12);
            B.redefines(currCycCredit);
            currCycCreditN = B.zoned(p + "CURR-CYC-CREDIT-N", 10, 2, true);
            currCycDebit = B.x(p + "CURR-CYC-DEBIT", 12);
            B.redefines(currCycDebit);
            currCycDebitN = B.zoned(p + "CURR-CYC-DEBIT-N", 10, 2, true);
            groupId = B.x(p + "GROUP-ID", 10);
            acctData = B.endGroup();
            B.group(p + "CUST-DATA");
            custIdX = B.x(p + "CUST-ID-X", 9);
            B.redefines(custIdX);
            custId = B.zoned(p + "CUST-ID", 9, 0, false);
            firstName = B.x(p + "CUST-FIRST-NAME", 25);
            middleName = B.x(p + "CUST-MIDDLE-NAME", 25);
            lastName = B.x(p + "CUST-LAST-NAME", 25);
            addrLine1 = B.x(p + "CUST-ADDR-LINE-1", 50);
            addrLine2 = B.x(p + "CUST-ADDR-LINE-2", 50);
            addrLine3 = B.x(p + "CUST-ADDR-LINE-3", 50);
            stateCd = B.x(p + "CUST-ADDR-STATE-CD", 2);
            countryCd = B.x(p + "CUST-ADDR-COUNTRY-CD", 3);
            zip = B.x(p + "CUST-ADDR-ZIP", 10);
            phone1 = B.x(p + "CUST-PHONE-NUM-1", 15);
            B.redefines(phone1);
            B.filler(1);
            phone1a = B.x(p + "CUST-PHONE-NUM-1A", 3);
            B.filler(1);
            phone1b = B.x(p + "CUST-PHONE-NUM-1B", 3);
            B.filler(1);
            phone1c = B.x(p + "CUST-PHONE-NUM-1C", 4);
            B.filler(2);
            phone2 = B.x(p + "CUST-PHONE-NUM-2", 15);
            B.redefines(phone2);
            B.filler(1);
            phone2a = B.x(p + "CUST-PHONE-NUM-2A", 3);
            B.filler(1);
            phone2b = B.x(p + "CUST-PHONE-NUM-2B", 3);
            B.filler(1);
            phone2c = B.x(p + "CUST-PHONE-NUM-2C", 4);
            B.filler(2);
            if (isNew) {
                B.group(p + "CUST-SSN-X");
                ssn1 = B.x(p + "CUST-SSN-1", 3);
                ssn2 = B.x(p + "CUST-SSN-2", 2);
                ssn3 = B.x(p + "CUST-SSN-3", 4);
                ssnX = B.endGroup();
            } else {
                ssnX = B.x(p + "CUST-SSN-X", 9);
                ssn1 = null;
                ssn2 = null;
                ssn3 = null;
            }
            B.redefines(ssnX);
            ssn = B.zoned(p + "CUST-SSN", 9, 0, false);
            govtIssuedId = B.x(p + "CUST-GOVT-ISSUED-ID", 20);
            dob = B.x(p + "CUST-DOB-YYYY-MM-DD", 8);
            B.redefines(dob);
            dobYear = B.x(p + "CUST-DOB-YEAR", 4);
            dobMon = B.x(p + "CUST-DOB-MON", 2);
            dobDay = B.x(p + "CUST-DOB-DAY", 2);
            eftAccountId = B.x(p + "CUST-EFT-ACCOUNT-ID", 10);
            priHolderInd = B.x(p + "CUST-PRI-HOLDER-IND", 1);
            ficoScoreX = B.x(p + "CUST-FICO-SCORE-X", 3);
            B.redefines(ficoScoreX);
            ficoScore = B.zoned(p + "CUST-FICO-SCORE", 3, 0, false);
            custData = B.endGroup();
            group = B.endGroup();
        }
    }
}
