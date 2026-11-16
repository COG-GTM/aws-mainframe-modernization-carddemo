package com.carddemo.batch.model;

import com.carddemo.batch.io.FieldWriter;
import com.carddemo.batch.io.FixedWidthCodec;

import java.math.BigDecimal;

/** {@code ACCOUNT-RECORD} from copybook CVACT01Y (300 bytes), ACCTDATA KSDS keyed by {@code ACCT-ID}. */
public record AccountRecord(
        long acctId,                       // PIC 9(11)
        String acctActiveStatus,           // PIC X(01)
        BigDecimal acctCurrBal,            // PIC S9(10)V99
        BigDecimal acctCreditLimit,        // PIC S9(10)V99
        BigDecimal acctCashCreditLimit,    // PIC S9(10)V99
        String acctOpenDate,               // PIC X(10)
        String acctExpiraionDate,          // PIC X(10) (sic, as in the copybook)
        String acctReissueDate,            // PIC X(10)
        BigDecimal acctCurrCycCredit,      // PIC S9(10)V99
        BigDecimal acctCurrCycDebit,       // PIC S9(10)V99
        String acctAddrZip,                // PIC X(10)
        String acctGroupId,                // PIC X(10)
        String filler) {                   // PIC X(178)

    public static final int LENGTH = 300;

    public static final FixedWidthCodec<AccountRecord> CODEC = FixedWidthCodec.of(LENGTH,
            r -> new AccountRecord(
                    r.unsignedLong(11),
                    r.text(1),
                    r.signedDecimal(12, 2),
                    r.signedDecimal(12, 2),
                    r.signedDecimal(12, 2),
                    r.text(10),
                    r.text(10),
                    r.text(10),
                    r.signedDecimal(12, 2),
                    r.signedDecimal(12, 2),
                    r.text(10),
                    r.text(10),
                    r.text(178)),
            a -> new FieldWriter(LENGTH)
                    .unsigned(a.acctId, 11)
                    .text(a.acctActiveStatus, 1)
                    .signedDecimal(a.acctCurrBal, 12, 2)
                    .signedDecimal(a.acctCreditLimit, 12, 2)
                    .signedDecimal(a.acctCashCreditLimit, 12, 2)
                    .text(a.acctOpenDate, 10)
                    .text(a.acctExpiraionDate, 10)
                    .text(a.acctReissueDate, 10)
                    .signedDecimal(a.acctCurrCycCredit, 12, 2)
                    .signedDecimal(a.acctCurrCycDebit, 12, 2)
                    .text(a.acctAddrZip, 10)
                    .text(a.acctGroupId, 10)
                    .text(a.filler, 178)
                    .toString());

    public AccountRecord withAcctCurrBal(BigDecimal v) {
        return new AccountRecord(acctId, acctActiveStatus, v, acctCreditLimit, acctCashCreditLimit, acctOpenDate,
                acctExpiraionDate, acctReissueDate, acctCurrCycCredit, acctCurrCycDebit, acctAddrZip, acctGroupId,
                filler);
    }

    public AccountRecord withAcctCurrCycCredit(BigDecimal v) {
        return new AccountRecord(acctId, acctActiveStatus, acctCurrBal, acctCreditLimit, acctCashCreditLimit,
                acctOpenDate, acctExpiraionDate, acctReissueDate, v, acctCurrCycDebit, acctAddrZip, acctGroupId,
                filler);
    }

    public AccountRecord withAcctCurrCycDebit(BigDecimal v) {
        return new AccountRecord(acctId, acctActiveStatus, acctCurrBal, acctCreditLimit, acctCashCreditLimit,
                acctOpenDate, acctExpiraionDate, acctReissueDate, acctCurrCycCredit, v, acctAddrZip, acctGroupId,
                filler);
    }
}
