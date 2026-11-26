package com.carddemo.programs.cbpaup0c.copybook;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cbpaup0c.ims.HdUnloadFile;
import com.carddemo.programs.cbpaup0c.ims.InMemoryPautDb;

import java.io.InputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.junit.jupiter.api.Test;

class CopybookCodecTest {

    static HdUnloadFile fixture() throws Exception {
        try (InputStream in = CopybookCodecTest.class.getResourceAsStream("/fixtures/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat")) {
            return HdUnloadFile.read(in, CobolCodec.EBCDIC);
        }
    }

    @Test
    void firstSampleSummaryDecodesPerCipausmy() throws Exception {
        InMemoryPautDb db = fixture().database();
        PendingAuthSummary s = PendingAuthSummary.fromBytes(db.roots().get(0).summary().data(), CobolCodec.EBCDIC);
        assertEquals(1, s.accountId());
        assertEquals("000000001", s.customerId());
        assertEquals(new BigDecimal("2022.00"), s.creditLimit());
        assertEquals(new BigDecimal("1020.00"), s.cashLimit());
        assertEquals(new BigDecimal("9.44"), s.creditBalance());
        assertEquals(6, s.approvedAuthCount());
        assertEquals(0, s.declinedAuthCount());
        assertEquals(new BigDecimal("9.44"), s.approvedAuthAmount());
        assertEquals("        00", String.join("", s.accountStatuses()));
    }

    @Test
    void firstSampleDetailDecodesPerCipaudty() throws Exception {
        InMemoryPautDb db = fixture().database();
        PendingAuthDetails d = PendingAuthDetails.fromBytes(db.roots().get(0).details().get(0).data(), CobolCodec.EBCDIC);
        assertEquals(23300, 99999 - d.authDate9c());
        assertEquals("231027", d.authOrigDate());
        assertEquals("00", d.authRespCode());
        assertTrue(d.isAuthApproved());
        assertEquals(new BigDecimal("1.24"), d.transactionAmt());
        assertEquals(new BigDecimal("1.24"), d.approvedAmt());
        assertTrue(d.isMatchPending());
        assertFalse(d.isFraudConfirmed());
        assertEquals(16, d.cardNum().length());
    }

    @Test
    void everySampleSegmentRoundTripsByteForByte() throws Exception {
        InMemoryPautDb db = fixture().database();
        int checked = 0;
        for (InMemoryPautDb.Root r : db.roots()) {
            byte[] raw = r.summary().data();
            PendingAuthSummary s = PendingAuthSummary.fromBytes(raw, CobolCodec.EBCDIC);
            if (s.approvedAuthCount() != 0x4040) { // last sample root is all X'40' (invalid packed signs)
                assertArrayEquals(raw, s.toBytes(CobolCodec.EBCDIC));
                checked++;
            }
            for (InMemoryPautDb.Segment d : r.details()) {
                assertArrayEquals(d.data(), PendingAuthDetails.fromBytes(d.data(), CobolCodec.EBCDIC).toBytes(CobolCodec.EBCDIC));
                checked++;
            }
        }
        assertEquals(21 + 202, checked);
    }

    @Test
    void segmentLengthsMatchDbd() {
        assertEquals(100, new PendingAuthSummary().toBytes(CobolCodec.EBCDIC).length);
        assertEquals(200, PendingAuthDetails.of(1, 1, "00", BigDecimal.ONE, BigDecimal.ONE).toBytes(CobolCodec.EBCDIC).length);
    }

    @Test
    void eightyEightLevels() {
        PendingAuthDetails d = PendingAuthDetails.of(1, 1, "05", BigDecimal.ONE, BigDecimal.ONE);
        assertFalse(d.isAuthApproved());
        assertEquals(PendingAuthDetails.MatchStatus.PENDING_EXPIRED, PendingAuthDetails.MatchStatus.of("E").orElseThrow());
        assertEquals(PendingAuthDetails.AuthFraud.FRAUD_REMOVED, PendingAuthDetails.AuthFraud.of("R").orElseThrow());
        assertTrue(PendingAuthDetails.MatchStatus.of("X").isEmpty());
    }

    @Test
    void packedDecimalCodec() {
        byte[] b = new byte[6];
        CobolCodec.encodePacked(new BigDecimal("-1234.56"), 2, b, 0, 6);
        assertArrayEquals(new byte[] {0x00, 0x00, 0x01, 0x23, 0x45, 0x6D}, b);
        assertEquals(new BigDecimal("-1234.56"), CobolCodec.decodePacked(b, 0, 6, 2));
        // Preferred sign F/C/A/E are positive, B/D negative.
        assertEquals(BigInteger.valueOf(12), CobolCodec.decodePackedUnscaled(new byte[] {0x01, 0x2F}, 0, 2));
        assertEquals(BigInteger.valueOf(-12), CobolCodec.decodePackedUnscaled(new byte[] {0x01, 0x2B}, 0, 2));
        assertThrows(IllegalArgumentException.class, () -> CobolCodec.decodePackedUnscaled(new byte[] {(byte) 0xA1, 0x2C}, 0, 2));
    }

    @Test
    void halfwordIsSignedBigEndian() {
        byte[] b = new byte[2];
        CobolCodec.encodeHalfword(-2, b, 0);
        assertEquals(-2, CobolCodec.decodeHalfword(b, 0));
        assertEquals(0x4040, CobolCodec.decodeHalfword(new byte[] {0x40, 0x40}, 0));
    }
}
