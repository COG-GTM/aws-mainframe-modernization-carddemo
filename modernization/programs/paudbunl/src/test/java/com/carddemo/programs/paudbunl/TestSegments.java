package com.carddemo.programs.paudbunl;

import com.carddemo.programs.paudbunl.copybook.PendingAuthDetails;
import com.carddemo.programs.paudbunl.copybook.PendingAuthSummary;
import com.carddemo.programs.paudbunl.support.CobolData;

import java.math.BigDecimal;
import java.util.List;

/** Builds EBCDIC PAUTSUM0 / PAUTDTL1 segments for tests. */
public final class TestSegments {

    private TestSegments() {
    }

    public static PendingAuthSummary summary(long acctId) {
        return new PendingAuthSummary(BigDecimal.valueOf(acctId), 100000000L + acctId, "A",
                List.of("01", "02", "  ", "  ", "00"),
                new BigDecimal("5000.00"), new BigDecimal("1000.00"),
                new BigDecimal("-123.45"), new BigDecimal("0.00"),
                3, 1, new BigDecimal("250.75"), new BigDecimal("99.99"));
    }

    public static byte[] summaryBytes(long acctId) {
        return summary(acctId).toBytes(CobolData.EBCDIC);
    }

    public static PendingAuthDetails details(int authDate9c, long authTime9c, String matchStatus) {
        return new PendingAuthDetails(BigDecimal.valueOf(authDate9c), BigDecimal.valueOf(authTime9c),
                "261126", "101500", "4111111111111111", "0100", "1228", "0100  ", "POS   ", "A1B2C3",
                "00", "0000", 0L, new BigDecimal("12.34"), new BigDecimal("12.34"), "5411", "USA", 5,
                "MERCHANT0000001", "CORNER STORE          ", "SPRINGFIELD  ", "IL", "62701    ", "TXN000000000001",
                matchStatus, " ", "        ");
    }

    public static byte[] detailsBytes(int authDate9c, long authTime9c) {
        return details(authDate9c, authTime9c, "P").toBytes(CobolData.EBCDIC);
    }
}
