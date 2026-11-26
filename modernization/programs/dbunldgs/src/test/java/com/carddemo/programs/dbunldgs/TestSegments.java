package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.CobolCodec;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthDetails;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthSummary;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;

final class TestSegments {
    private TestSegments() {
    }

    static PendingAuthSummary summary(long acctId) {
        return new PendingAuthSummary(BigDecimal.valueOf(acctId), acctId, "A",
                List.of("00", "  ", "  ", "  ", "  "),
                new BigDecimal("5000.00"), new BigDecimal("1000.00"), new BigDecimal("-12.34"),
                BigDecimal.ZERO, (short) 2, (short) 1, new BigDecimal("150.25"), new BigDecimal("75.00"), "");
    }

    static byte[] summaryBytes(long acctId) {
        return summary(acctId).toBytes(CobolCodec.EBCDIC);
    }

    /** A root whose PA-ACCT-ID is spaces (non-numeric), like the last root of the sample unload. */
    static byte[] nonNumericSummaryBytes() {
        byte[] b = summaryBytes(0);
        Arrays.fill(b, 0, PendingAuthSummary.ACCT_ID_LENGTH, (byte) 0x40);
        return b;
    }

    static PendingAuthDetails detail(int seq) {
        return new PendingAuthDetails(BigDecimal.valueOf(99999 - 23300), BigDecimal.valueOf(999999999L - seq),
                "231027", "0412" + String.format("%02d", seq % 100), "4111111111111111", "0100", "1225",
                "0100  ", "POS   ", "A" + seq, "00", "0000", 10, new BigDecimal("12.34"), new BigDecimal("12.34"),
                "5411", "USA", 1, "MERCH" + seq, "Merchant " + seq, "Sydney", "NS", "2000", "TX" + seq,
                "P", " ", "", "");
    }

    static byte[] detailBytes(int seq) {
        return detail(seq).toBytes(CobolCodec.EBCDIC);
    }
}
