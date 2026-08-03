package com.carddemo.interestcalc.copybook;

import java.math.BigDecimal;

/**
 * Copybook {@code CVTRA01Y} - {@code TRAN-CAT-BAL-RECORD}, RECLN 50.
 *
 * <pre>
 * 05 TRAN-CAT-KEY.
 *    10 TRANCAT-ACCT-ID   PIC 9(11).   bytes   1-11
 *    10 TRANCAT-TYPE-CD   PIC X(02).   bytes  12-13
 *    10 TRANCAT-CD        PIC 9(04).   bytes  14-17
 * 05 TRAN-CAT-BAL         PIC S9(09)V99. bytes 18-28
 * 05 FILLER               PIC X(22).   bytes 29-50
 * </pre>
 *
 * @param accountId {@code TRANCAT-ACCT-ID}; {@code PIC 9(11)} is a zero padded string, not an int
 * @param typeCode {@code TRANCAT-TYPE-CD}
 * @param categoryCode {@code TRANCAT-CD}, zero padded
 * @param balance {@code TRAN-CAT-BAL}, scale 2
 * @param filler bytes 29-50, kept so the record image can be reproduced verbatim
 */
public record TranCatBalanceRecord(String accountId, String typeCode, String categoryCode, BigDecimal balance,
                                   String filler) {

    public static final int LENGTH = 50;

    public static TranCatBalanceRecord parse(String raw) {
        String rec = FixedWidth.record(raw, LENGTH);
        return new TranCatBalanceRecord(
                FixedWidth.field(rec, 0, 11),
                FixedWidth.field(rec, 11, 2),
                FixedWidth.field(rec, 13, 4),
                ZonedDecimal.decode(FixedWidth.field(rec, 17, 11), 9, 2),
                FixedWidth.field(rec, 28, 22));
    }

    /** The 50 byte record image, as the mainline {@code DISPLAY TRAN-CAT-BAL-RECORD} shows it. */
    public String format() {
        return FixedWidth.unsigned(accountId, 11)
                + FixedWidth.alphanumeric(typeCode, 2)
                + FixedWidth.unsigned(categoryCode, 4)
                + ZonedDecimal.encode(balance, 9, 2)
                + FixedWidth.alphanumeric(filler, 22);
    }

    /** The VSAM {@code RECORD KEY} of TCATBALF: account id + type code + category code. */
    public String key() {
        return accountId + typeCode + categoryCode;
    }
}
