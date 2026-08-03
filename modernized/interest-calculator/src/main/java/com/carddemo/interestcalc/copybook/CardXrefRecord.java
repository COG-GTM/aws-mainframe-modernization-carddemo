package com.carddemo.interestcalc.copybook;

/**
 * Copybook {@code CVACT03Y} - {@code CARD-XREF-RECORD}, RECLN 50.
 *
 * <pre>
 * 05 XREF-CARD-NUM   PIC X(16).   bytes  1-16
 * 05 XREF-CUST-ID    PIC 9(09).   bytes 17-25
 * 05 XREF-ACCT-ID    PIC 9(11).   bytes 26-36
 * 05 FILLER          PIC X(14).   bytes 37-50
 * </pre>
 *
 * <p>{@code XREF-CARD-NUM} is the VSAM primary key and {@code XREF-ACCT-ID} the alternate key
 * that CBACT04C reads through ({@code KEY IS FD-XREF-ACCT-ID}).
 */
public record CardXrefRecord(String cardNumber, String customerId, String accountId) {

    public static final int LENGTH = 50;

    public static CardXrefRecord parse(String raw) {
        String rec = FixedWidth.record(raw, LENGTH);
        return new CardXrefRecord(
                FixedWidth.field(rec, 0, 16),
                FixedWidth.field(rec, 16, 9),
                FixedWidth.field(rec, 25, 11));
    }
}
