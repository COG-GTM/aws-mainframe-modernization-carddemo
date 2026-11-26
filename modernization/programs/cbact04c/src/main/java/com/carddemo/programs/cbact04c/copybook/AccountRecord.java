package com.carddemo.programs.cbact04c.copybook;

import java.math.BigDecimal;

/**
 * CVACT01Y - ACCOUNT-RECORD (RECLN 300), account master (ACCTFILE KSDS).
 *
 * <pre>
 * 05 ACCT-ID                  PIC 9(11).
 * 05 ACCT-ACTIVE-STATUS       PIC X(01).
 * 05 ACCT-CURR-BAL            PIC S9(10)V99.
 * 05 ACCT-CREDIT-LIMIT        PIC S9(10)V99.
 * 05 ACCT-CASH-CREDIT-LIMIT   PIC S9(10)V99.
 * 05 ACCT-OPEN-DATE           PIC X(10).
 * 05 ACCT-EXPIRAION-DATE      PIC X(10).
 * 05 ACCT-REISSUE-DATE        PIC X(10).
 * 05 ACCT-CURR-CYC-CREDIT     PIC S9(10)V99.
 * 05 ACCT-CURR-CYC-DEBIT      PIC S9(10)V99.
 * 05 ACCT-ADDR-ZIP            PIC X(10).
 * 05 ACCT-GROUP-ID            PIC X(10).
 * 05 FILLER                   PIC X(178).
 * </pre>
 * The copybook's misspelt ACCT-EXPIRAION-DATE is kept as {@code acctExpiraionDate} for traceability.
 */
public record AccountRecord(
        long acctId,
        String acctActiveStatus,
        BigDecimal acctCurrBal,
        BigDecimal acctCreditLimit,
        BigDecimal acctCashCreditLimit,
        String acctOpenDate,
        String acctExpiraionDate,
        String acctReissueDate,
        BigDecimal acctCurrCycCredit,
        BigDecimal acctCurrCycDebit,
        String acctAddrZip,
        String acctGroupId,
        String filler) {

    public static final int LENGTH = 300;

    public AccountRecord {
        acctActiveStatus = Pic.alpha(acctActiveStatus, 1);
        acctCurrBal = Pic.truncate(acctCurrBal, 10, 2);
        acctCreditLimit = Pic.truncate(acctCreditLimit, 10, 2);
        acctCashCreditLimit = Pic.truncate(acctCashCreditLimit, 10, 2);
        acctOpenDate = Pic.alpha(acctOpenDate, 10);
        acctExpiraionDate = Pic.alpha(acctExpiraionDate, 10);
        acctReissueDate = Pic.alpha(acctReissueDate, 10);
        acctCurrCycCredit = Pic.truncate(acctCurrCycCredit, 10, 2);
        acctCurrCycDebit = Pic.truncate(acctCurrCycDebit, 10, 2);
        acctAddrZip = Pic.alpha(acctAddrZip, 10);
        acctGroupId = Pic.alpha(acctGroupId, 10);
        filler = Pic.alpha(filler, 178);
    }

    public AccountRecord withAcctCurrBal(BigDecimal v) {
        return new AccountRecord(acctId, acctActiveStatus, v, acctCreditLimit, acctCashCreditLimit,
                acctOpenDate, acctExpiraionDate, acctReissueDate, acctCurrCycCredit, acctCurrCycDebit,
                acctAddrZip, acctGroupId, filler);
    }

    public AccountRecord withAcctCurrCycCredit(BigDecimal v) {
        return new AccountRecord(acctId, acctActiveStatus, acctCurrBal, acctCreditLimit, acctCashCreditLimit,
                acctOpenDate, acctExpiraionDate, acctReissueDate, v, acctCurrCycDebit,
                acctAddrZip, acctGroupId, filler);
    }

    public AccountRecord withAcctCurrCycDebit(BigDecimal v) {
        return new AccountRecord(acctId, acctActiveStatus, acctCurrBal, acctCreditLimit, acctCashCreditLimit,
                acctOpenDate, acctExpiraionDate, acctReissueDate, acctCurrCycCredit, v,
                acctAddrZip, acctGroupId, filler);
    }

    public static final RecordCodec<AccountRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public AccountRecord decode(String image) {
            Pic.Cursor c = new Pic.Cursor(image, LENGTH);
            return new AccountRecord(
                    Pic.parseUnsigned(c.take(11)),
                    c.take(1),
                    Pic.parseSignedZoned(c.take(12), 2),
                    Pic.parseSignedZoned(c.take(12), 2),
                    Pic.parseSignedZoned(c.take(12), 2),
                    c.take(10),
                    c.take(10),
                    c.take(10),
                    Pic.parseSignedZoned(c.take(12), 2),
                    Pic.parseSignedZoned(c.take(12), 2),
                    c.take(10),
                    c.take(10),
                    c.take(178));
        }

        @Override
        public String encode(AccountRecord r) {
            return Pic.unsigned(r.acctId, 11)
                    + r.acctActiveStatus
                    + Pic.signedZoned(r.acctCurrBal, 10, 2)
                    + Pic.signedZoned(r.acctCreditLimit, 10, 2)
                    + Pic.signedZoned(r.acctCashCreditLimit, 10, 2)
                    + r.acctOpenDate
                    + r.acctExpiraionDate
                    + r.acctReissueDate
                    + Pic.signedZoned(r.acctCurrCycCredit, 10, 2)
                    + Pic.signedZoned(r.acctCurrCycDebit, 10, 2)
                    + r.acctAddrZip
                    + r.acctGroupId
                    + r.filler;
        }
    };
}
