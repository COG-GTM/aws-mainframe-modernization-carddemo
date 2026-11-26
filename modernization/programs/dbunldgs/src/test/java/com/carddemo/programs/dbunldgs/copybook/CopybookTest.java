package com.carddemo.programs.dbunldgs.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    private static final java.nio.charset.Charset CS = CobolCodec.EBCDIC;

    @Test
    void packedDecimalEncodeDecode() {
        byte[] b = new byte[6];
        CobolCodec.encodePacked(new BigDecimal("12345.67"), b, 0, 11, 2);
        assertEquals("00001234567C", CobolCodec.hex(b, 0, 6));
        assertEquals(new BigDecimal("12345.67"), CobolCodec.decodePacked(b, 0, 11, 2));
        CobolCodec.encodePacked(new BigDecimal("-0.05"), b, 0, 11, 2);
        assertEquals("00000000005D", CobolCodec.hex(b, 0, 6));
        assertEquals(new BigDecimal("-0.05"), CobolCodec.decodePacked(b, 0, 11, 2));
    }

    @Test
    void packedMoveTruncatesFractionAndHighOrderDigits() {
        byte[] b = new byte[3];
        CobolCodec.encodePacked(new BigDecimal("123456"), b, 0, 5, 0);
        assertEquals("23456C", CobolCodec.hex(b, 0, 3));
        byte[] c = new byte[6];
        CobolCodec.encodePacked(new BigDecimal("1.999"), c, 0, 11, 2);
        assertEquals(new BigDecimal("1.99"), CobolCodec.decodePacked(c, 0, 11, 2));
    }

    @Test
    void isNumericOnPackedField() {
        assertTrue(CobolCodec.isNumericPacked(new byte[] {0x00, 0x1C}, 0, 2));
        assertTrue(CobolCodec.isNumericPacked(new byte[] {0x00, 0x1F}, 0, 2));
        assertTrue(CobolCodec.isNumericPacked(new byte[] {0x00, 0x1D}, 0, 2));
        assertFalse(CobolCodec.isNumericPacked(new byte[] {0x40, 0x40}, 0, 2));
        assertFalse(CobolCodec.isNumericPacked(new byte[] {0x00, 0x19}, 0, 2));
        assertFalse(CobolCodec.isNumericPacked(new byte[] {0x0A, 0x1C}, 0, 2));
        assertThrows(CobolDataException.class, () -> CobolCodec.decodePacked(new byte[] {0x40, 0x40, 0x40}, 0, 5, 0));
    }

    @Test
    void zonedAndAlphanumericMoves() {
        byte[] b = new byte[4];
        CobolCodec.encodeZoned(123456, b, 0, 4, StandardCharsets.US_ASCII);
        assertEquals("3456", new String(b, StandardCharsets.US_ASCII));
        CobolCodec.encodeZoned(7, b, 0, 4, CS);
        assertEquals(7, CobolCodec.decodeZoned(b, 0, 4, CS));
        assertArrayEquals(new byte[] {(byte) 0xF0, (byte) 0xF0, (byte) 0xF0, (byte) 0xF7}, b);
        assertThrows(CobolDataException.class, () -> CobolCodec.decodeZoned(new byte[] {0x40}, 0, 1, CS));
        CobolCodec.encodeAlpha("ABCDEF", b, 0, 4, CS);
        assertEquals("ABCD", CobolCodec.decodeAlpha(b, 0, 4, CS));
        CobolCodec.encodeAlpha("A", b, 0, 4, CS);
        assertEquals("A   ", CobolCodec.decodeAlpha(b, 0, 4, CS));
        assertEquals("AB", CobolCodec.pic("ABC", 2));
        assertEquals("A  ", CobolCodec.pic("A", 3));
    }

    @Test
    void binaryCompFields() {
        byte[] b = new byte[4];
        CobolCodec.encodeHalfword((short) -2, b, 0);
        assertEquals((short) -2, CobolCodec.decodeHalfword(b, 0));
        CobolCodec.encodeFullword(70000, b, 0);
        assertEquals(70000, CobolCodec.decodeFullword(b, 0));
    }

    @Test
    void summaryLayoutIs100BytesWithFieldOffsets() {
        PendingAuthSummary s = new PendingAuthSummary(new BigDecimal("12345678901"), 987654321L, "A",
                List.of("01", "02", "03", "04", "05"), new BigDecimal("999999999.99"), new BigDecimal("-1.00"),
                BigDecimal.ZERO, new BigDecimal("0.01"), (short) 3, (short) -1, new BigDecimal("10.00"),
                new BigDecimal("20.00"), "BOMM");
        byte[] b = s.toBytes(CS);
        assertEquals(100, b.length);
        assertEquals("12345678901C", CobolCodec.hex(b, 0, 6));
        assertEquals("987654321", CobolCodec.decodeAlpha(b, 6, 9, CS));
        assertEquals("A", CobolCodec.decodeAlpha(b, 15, 1, CS));
        assertEquals("0102030405", CobolCodec.decodeAlpha(b, 16, 10, CS));
        assertEquals("99999999999C", CobolCodec.hex(b, 26, 6));
        assertEquals("00000000100D", CobolCodec.hex(b, 32, 6));
        assertEquals("0003FFFF", CobolCodec.hex(b, 50, 4));
        assertEquals("BOMM" + " ".repeat(30), CobolCodec.decodeAlpha(b, 66, 34, CS));
        assertEquals(s, PendingAuthSummary.fromBytes(b, CS));
        assertTrue(PendingAuthSummary.isAcctIdNumeric(b));
        assertThrows(IllegalArgumentException.class, () -> PendingAuthSummary.fromBytes(new byte[99], CS));
        assertThrows(IllegalArgumentException.class, () -> new PendingAuthSummary(BigDecimal.ONE, 1, "A",
                List.of("00"), BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO,
                (short) 0, (short) 0, BigDecimal.ZERO, BigDecimal.ZERO, ""));
    }

    @Test
    void detailsLayoutIs200BytesWith88Levels() {
        PendingAuthDetails d = new PendingAuthDetails(new BigDecimal("76699"), new BigDecimal("998747444"),
                "231027", "041252", "9680294154603697", "0100", "1122", "1234", "102030", "041252", "00", "0000",
                10, new BigDecimal("1234567890.12"), new BigDecimal("-0.01"), "5442", "USA", 1, "M1",
                "Amazon.com", "Wilmington", "DE", "19801", "T1", "M", "F", "20231027", "");
        byte[] b = d.toBytes(CS);
        assertEquals(200, b.length);
        assertEquals("76699C", CobolCodec.hex(b, 0, 3));
        assertEquals("998747444C", CobolCodec.hex(b, 3, 5));
        assertEquals("000010", CobolCodec.decodeAlpha(b, 68, 6, CS));
        assertEquals("0123456789012C", CobolCodec.hex(b, 74, 7));
        assertEquals("0000000000001D", CobolCodec.hex(b, 81, 7));
        assertEquals("01", CobolCodec.decodeAlpha(b, 95, 2, CS));
        assertEquals("MF20231027", CobolCodec.decodeAlpha(b, 173, 10, CS));
        assertEquals(d, PendingAuthDetails.fromBytes(b, CS));
        assertTrue(d.isPaAuthApproved());
        assertEquals(PendingAuthDetails.MatchStatus.PA_MATCHED_WITH_TRAN, d.matchStatus().orElseThrow());
        assertEquals(PendingAuthDetails.AuthFraud.PA_FRAUD_CONFIRMED, d.authFraud().orElseThrow());
        assertEquals(PendingAuthDetails.MatchStatus.PA_MATCH_AUTH_DECLINED, PendingAuthDetails.MatchStatus.of("D").orElseThrow());
        assertEquals(PendingAuthDetails.MatchStatus.PA_MATCH_PENDING_EXPIRED, PendingAuthDetails.MatchStatus.of("E").orElseThrow());
        assertEquals(PendingAuthDetails.AuthFraud.PA_FRAUD_REMOVED, PendingAuthDetails.AuthFraud.of("R").orElseThrow());
        assertTrue(PendingAuthDetails.MatchStatus.of(" ").isEmpty());
        assertTrue(PendingAuthDetails.AuthFraud.of(" ").isEmpty());
    }

    @Test
    void declinedResponseIsNotApproved() {
        PendingAuthDetails d = PendingAuthDetails.fromBytes(new PendingAuthDetails(BigDecimal.ONE, BigDecimal.ONE,
                "", "", "", "", "", "", "", "", "05", "", 0, BigDecimal.ZERO, BigDecimal.ZERO, "", "", 0, "", "", "",
                "", "", "", "D", " ", "", "").toBytes(CS), CS);
        assertFalse(d.isPaAuthApproved());
        assertEquals("05", d.paAuthRespCode());
    }

    @Test
    void pcbMasksMatchPsbDligsamp() {
        PautbPcb paut = new PautbPcb();
        assertEquals("DBPAUTP0", paut.getDbdName());
        assertEquals("GOTP", paut.getPcbProcopt());
        assertEquals(255, paut.keyFbLength());
        PasflPcb pasfl = new PasflPcb();
        assertEquals("PASFLDBD", pasfl.getDbdName());
        assertEquals("LS  ", pasfl.getPcbProcopt());
        assertEquals(100, pasfl.keyFbLength());
        PadflPcb padfl = new PadflPcb();
        assertEquals("PADFLDBD", padfl.getDbdName());
        assertEquals(255, padfl.keyFbLength());
        assertTrue(paut.statusIsSpaces());
        paut.setPcbStatus("GB");
        assertFalse(paut.statusIsSpaces());
        paut.initializeStatus();
        assertEquals("  ", paut.getPcbStatus());
        paut.setKeyFb(new byte[] {0x00, 0x1C}, CS);
        assertEquals(2, paut.getKeyFbName());
        assertEquals(0x1C, paut.getKeyFb()[1]);
        assertEquals((byte) 0x40, paut.getKeyFb()[2]);
    }

    @Test
    void imsFunctionCodesArePic4() {
        for (String f : List.of(ImsFunctionCodes.FUNC_GU, ImsFunctionCodes.FUNC_GHU, ImsFunctionCodes.FUNC_GN,
                ImsFunctionCodes.FUNC_GHN, ImsFunctionCodes.FUNC_GNP, ImsFunctionCodes.FUNC_GHNP,
                ImsFunctionCodes.FUNC_REPL, ImsFunctionCodes.FUNC_ISRT, ImsFunctionCodes.FUNC_DLET)) {
            assertEquals(4, f.length());
        }
        assertEquals("GN  ", ImsFunctionCodes.FUNC_GN);
        assertEquals("GNP ", ImsFunctionCodes.FUNC_GNP);
        assertEquals(4, ImsFunctionCodes.PARMCOUNT);
    }
}
