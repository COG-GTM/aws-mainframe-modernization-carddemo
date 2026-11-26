package com.carddemo.programs.copaus0c.copybook;

import com.carddemo.programs.copaus0c.cobol.PackedDecimal;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.List;

/** CIPAUSMY - PENDING-AUTH-SUMMARY, IMS root segment PAUTSUM0 (100 bytes). FILLER X(34) dropped. */
public record PendingAuthSummary(
        long paAcctId,                 // PA-ACCT-ID           PIC S9(11) COMP-3
        long paCustId,                 // PA-CUST-ID           PIC 9(09)
        String paAuthStatus,           // PA-AUTH-STATUS       PIC X(01)
        List<String> paAccountStatus,  // PA-ACCOUNT-STATUS    PIC X(02) OCCURS 5 TIMES
        BigDecimal paCreditLimit,      // PA-CREDIT-LIMIT      PIC S9(09)V99 COMP-3
        BigDecimal paCashLimit,        // PA-CASH-LIMIT        PIC S9(09)V99 COMP-3
        BigDecimal paCreditBalance,    // PA-CREDIT-BALANCE    PIC S9(09)V99 COMP-3
        BigDecimal paCashBalance,      // PA-CASH-BALANCE      PIC S9(09)V99 COMP-3
        int paApprovedAuthCnt,         // PA-APPROVED-AUTH-CNT PIC S9(04) COMP
        int paDeclinedAuthCnt,         // PA-DECLINED-AUTH-CNT PIC S9(04) COMP
        BigDecimal paApprovedAuthAmt,  // PA-APPROVED-AUTH-AMT PIC S9(09)V99 COMP-3
        BigDecimal paDeclinedAuthAmt) { // PA-DECLINED-AUTH-AMT PIC S9(09)V99 COMP-3

    public static final int SEGMENT_LENGTH = 100;

    public PendingAuthSummary {
        paAccountStatus = List.copyOf(paAccountStatus);
        if (paAccountStatus.size() != 5) {
            throw new IllegalArgumentException("PA-ACCOUNT-STATUS OCCURS 5 TIMES");
        }
    }

    /** Decodes the segment image (EBCDIC display fields, big-endian COMP, packed COMP-3). */
    public static PendingAuthSummary decode(byte[] seg, int off, Charset ebcdic) {
        String[] status = new String[5];
        for (int i = 0; i < 5; i++) {
            status[i] = new String(seg, off + 16 + i * 2, 2, ebcdic);
        }
        return new PendingAuthSummary(
                PackedDecimal.decodeLong(seg, off, 11),
                Long.parseLong(new String(seg, off + 6, 9, ebcdic)),
                new String(seg, off + 15, 1, ebcdic),
                List.of(status),
                PackedDecimal.decode(seg, off + 26, 11, 2),
                PackedDecimal.decode(seg, off + 32, 11, 2),
                PackedDecimal.decode(seg, off + 38, 11, 2),
                PackedDecimal.decode(seg, off + 44, 11, 2),
                comp2(seg, off + 50),
                comp2(seg, off + 52),
                PackedDecimal.decode(seg, off + 54, 11, 2),
                PackedDecimal.decode(seg, off + 60, 11, 2));
    }

    private static int comp2(byte[] b, int off) {
        return (short) (((b[off] & 0xFF) << 8) | (b[off + 1] & 0xFF));
    }
}
