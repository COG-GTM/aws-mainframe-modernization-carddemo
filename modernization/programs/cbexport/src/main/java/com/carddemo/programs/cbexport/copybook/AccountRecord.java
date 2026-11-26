package com.carddemo.programs.cbexport.copybook;

import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getSignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getUnsignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putSignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putUnsignedZoned;

import java.math.BigDecimal;

/**
 * CVACT01Y - 01 ACCOUNT-RECORD (RECLN 300). All fields USAGE DISPLAY; S9(10)V99 fields carry a
 * trailing overpunch sign and scale 2.
 *
 * @param acctId            ACCT-ID PIC 9(11)
 * @param activeStatus      ACCT-ACTIVE-STATUS PIC X(01)
 * @param currBal           ACCT-CURR-BAL PIC S9(10)V99
 * @param creditLimit       ACCT-CREDIT-LIMIT PIC S9(10)V99
 * @param cashCreditLimit   ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
 * @param openDate          ACCT-OPEN-DATE PIC X(10)
 * @param expiraionDate     ACCT-EXPIRAION-DATE PIC X(10) (sic)
 * @param reissueDate       ACCT-REISSUE-DATE PIC X(10)
 * @param currCycCredit     ACCT-CURR-CYC-CREDIT PIC S9(10)V99
 * @param currCycDebit      ACCT-CURR-CYC-DEBIT PIC S9(10)V99
 * @param addrZip           ACCT-ADDR-ZIP PIC X(10)
 * @param groupId           ACCT-GROUP-ID PIC X(10)
 * @param filler            FILLER PIC X(178)
 */
public record AccountRecord(
        long acctId,
        String activeStatus,
        BigDecimal currBal,
        BigDecimal creditLimit,
        BigDecimal cashCreditLimit,
        String openDate,
        String expiraionDate,
        String reissueDate,
        BigDecimal currCycCredit,
        BigDecimal currCycDebit,
        String addrZip,
        String groupId,
        String filler) {

    public static final int LENGTH = 300;
    public static final int SCALE = 2;

    public static AccountRecord parse(byte[] r) {
        return new AccountRecord(
                getUnsignedZoned(r, 0, 11),
                getAlnum(r, 11, 1),
                getSignedZoned(r, 12, 12, SCALE),
                getSignedZoned(r, 24, 12, SCALE),
                getSignedZoned(r, 36, 12, SCALE),
                getAlnum(r, 48, 10),
                getAlnum(r, 58, 10),
                getAlnum(r, 68, 10),
                getSignedZoned(r, 78, 12, SCALE),
                getSignedZoned(r, 90, 12, SCALE),
                getAlnum(r, 102, 10),
                getAlnum(r, 112, 10),
                getAlnum(r, 122, 178));
    }

    public byte[] toBytes() {
        byte[] r = new byte[LENGTH];
        putUnsignedZoned(r, 0, 11, acctId);
        putAlnum(r, 11, 1, activeStatus);
        putSignedZoned(r, 12, 12, SCALE, currBal);
        putSignedZoned(r, 24, 12, SCALE, creditLimit);
        putSignedZoned(r, 36, 12, SCALE, cashCreditLimit);
        putAlnum(r, 48, 10, openDate);
        putAlnum(r, 58, 10, expiraionDate);
        putAlnum(r, 68, 10, reissueDate);
        putSignedZoned(r, 78, 12, SCALE, currCycCredit);
        putSignedZoned(r, 90, 12, SCALE, currCycDebit);
        putAlnum(r, 102, 10, addrZip);
        putAlnum(r, 112, 10, groupId);
        putAlnum(r, 122, 178, filler);
        return r;
    }
}
