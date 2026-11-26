package com.carddemo.programs.cbtrn01c.copybook;

import java.math.BigDecimal;

/** Copybook CVACT01Y: 01 ACCOUNT-RECORD (RECLN 300). */
public record AccountRecord(
        long acctId,                    // ACCT-ID                PIC 9(11)
        String activeStatus,            // ACCT-ACTIVE-STATUS     PIC X(01)
        BigDecimal currBal,             // ACCT-CURR-BAL          PIC S9(10)V99
        BigDecimal creditLimit,         // ACCT-CREDIT-LIMIT      PIC S9(10)V99
        BigDecimal cashCreditLimit,     // ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
        String openDate,                // ACCT-OPEN-DATE         PIC X(10)
        String expiraionDate,           // ACCT-EXPIRAION-DATE    PIC X(10) (sic, as in copybook)
        String reissueDate,             // ACCT-REISSUE-DATE      PIC X(10)
        BigDecimal currCycCredit,       // ACCT-CURR-CYC-CREDIT   PIC S9(10)V99
        BigDecimal currCycDebit,        // ACCT-CURR-CYC-DEBIT    PIC S9(10)V99
        String addrZip,                 // ACCT-ADDR-ZIP          PIC X(10)
        String groupId,                 // ACCT-GROUP-ID          PIC X(10)
        String filler) {                // FILLER                 PIC X(178)

    public static final int LENGTH = 300;

    public AccountRecord {
        acctId = Pic.unsigned(acctId, 11);
        activeStatus = Pic.alpha(activeStatus, 1);
        currBal = Pic.signed(currBal, 10, 2);
        creditLimit = Pic.signed(creditLimit, 10, 2);
        cashCreditLimit = Pic.signed(cashCreditLimit, 10, 2);
        openDate = Pic.alpha(openDate, 10);
        expiraionDate = Pic.alpha(expiraionDate, 10);
        reissueDate = Pic.alpha(reissueDate, 10);
        currCycCredit = Pic.signed(currCycCredit, 10, 2);
        currCycDebit = Pic.signed(currCycDebit, 10, 2);
        addrZip = Pic.alpha(addrZip, 10);
        groupId = Pic.alpha(groupId, 10);
        filler = Pic.alpha(filler, 178);
    }

    public static AccountRecord initial() {
        return new AccountRecord(0, "", BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO, "", "", "",
                BigDecimal.ZERO, BigDecimal.ZERO, "", "", "");
    }

    /** MOVE ... TO ACCT-ID. */
    public AccountRecord withAcctId(long newAcctId) {
        return new AccountRecord(newAcctId, activeStatus, currBal, creditLimit, cashCreditLimit, openDate,
                expiraionDate, reissueDate, currCycCredit, currCycDebit, addrZip, groupId, filler);
    }

    public String acctIdDisplay() {
        return Pic.formatUnsigned(acctId, 11);
    }

    public static final RecordCodec<AccountRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public AccountRecord parse(String image) {
            Pic.Reader r = new Pic.Reader(image, LENGTH);
            return new AccountRecord(r.n(11), r.x(1), r.s(10, 2), r.s(10, 2), r.s(10, 2), r.x(10), r.x(10),
                    r.x(10), r.s(10, 2), r.s(10, 2), r.x(10), r.x(10), r.x(178));
        }

        @Override
        public String format(AccountRecord a) {
            return Pic.formatUnsigned(a.acctId, 11) + a.activeStatus
                    + Pic.formatSigned(a.currBal, 10, 2) + Pic.formatSigned(a.creditLimit, 10, 2)
                    + Pic.formatSigned(a.cashCreditLimit, 10, 2) + a.openDate + a.expiraionDate + a.reissueDate
                    + Pic.formatSigned(a.currCycCredit, 10, 2) + Pic.formatSigned(a.currCycDebit, 10, 2)
                    + a.addrZip + a.groupId + a.filler;
        }
    };

    public String toFixedWidth() {
        return CODEC.format(this);
    }
}
