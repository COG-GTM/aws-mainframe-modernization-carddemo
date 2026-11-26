package com.carddemo.programs.cbstm03b.copybook;

import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    @Test
    void recordLengthsMatchFdsAndJcl() {
        assertEquals(350, FdTrnxFileRec.RECORD_LENGTH);
        assertEquals(16 + 16 + 318, new FdTrnxFileRec("", "", "").toRecord().length());
        assertEquals(50, new FdXrefFileRec("", "").toRecord().length());
        assertEquals(500, new FdCustFileRec("", "").toRecord().length());
        assertEquals(300, new FdAcctFileRec("", "").toRecord().length());
        assertEquals(LkM03bArea.AREA_LENGTH, 8 + 1 + 2 + 25 + 4 + 1000);
    }

    @Test
    void trnxRecordRoundTripsAndExposesKey() {
        String rec = "4000000000000001" + "0000000000000002" + "X".repeat(318);
        FdTrnxFileRec r = FdTrnxFileRec.parse(rec);
        assertEquals("40000000000000010000000000000002", r.fdTrnxsId());
        assertEquals(rec, r.toRecord());
    }

    @Test
    void parseRejectsWrongLength() {
        assertThrows(IllegalArgumentException.class, () -> FdXrefFileRec.parse("short"));
        assertThrows(IllegalArgumentException.class, () -> FdCustFileRec.parse(" ".repeat(501)));
    }

    @Test
    void custKeyMoveIsPicX9() {
        assertEquals("123      ", FdCustFileRec.toKey("123"));
        assertEquals("123456789", FdCustFileRec.toKey("1234567890"));
    }

    @Test
    void acctKeyMoveIsPic9_11() {
        assertEquals("00000000042", FdAcctFileRec.toKey("42"));
        assertEquals("23456789012", FdAcctFileRec.toKey("123456789012"));
        assertEquals(42L, new FdAcctFileRec("42", "").fdAcctIdValue());
    }

    @Test
    void operEightyEightLevels() {
        assertEquals(Optional.of(M03bOper.OPEN), M03bOper.fromCode('O'));
        assertEquals(Optional.of(M03bOper.CLOSE), M03bOper.fromCode('C'));
        assertEquals(Optional.of(M03bOper.READ), M03bOper.fromCode('R'));
        assertEquals(Optional.of(M03bOper.READ_K), M03bOper.fromCode('K'));
        assertEquals(Optional.of(M03bOper.WRITE), M03bOper.fromCode('W'));
        assertEquals(Optional.of(M03bOper.REWRITE), M03bOper.fromCode('Z'));
        assertEquals(Optional.empty(), M03bOper.fromCode('o'));

        LkM03bArea lk = new LkM03bArea().set(M03bOper.READ_K);
        assertTrue(lk.isM03bReadK());
        assertFalse(lk.isM03bRead() || lk.isM03bOpen() || lk.isM03bClose() || lk.isM03bWrite() || lk.isM03bRewrite());
    }

    @Test
    void linkageFieldsArePaddedAndTruncated() {
        LkM03bArea lk = new LkM03bArea().dd("ACCT").rc("0").key("1").fldt("abc");
        assertEquals("ACCT    ", lk.dd());
        assertEquals("0 ", lk.rc());
        assertEquals(25, lk.key().length());
        assertEquals(1000, lk.fldt().length());
        assertEquals("X".repeat(1000), lk.fldt("X".repeat(1200)).fldt());
    }

    @Test
    void keyLnIsSignedFourDigits() {
        assertEquals(2345, new LkM03bArea().keyLn(12345).keyLn());
        assertEquals(-5, new LkM03bArea().keyLn(-5).keyLn());
        assertEquals(-2345, new LkM03bArea().keyLn(-12345).keyLn());
    }

    @Test
    void fileStatusCodes() {
        assertEquals("  ", FileStatus.INITIAL.code());
        assertTrue(FileStatus.SUCCESS.isSuccessful());
        assertTrue(FileStatus.of("04").isSuccessful());
        assertFalse(FileStatus.END_OF_FILE.isSuccessful());
        assertFalse(FileStatus.RECORD_NOT_FOUND.isSuccessful());
        assertThrows(IllegalArgumentException.class, () -> FileStatus.of("0"));
    }
}
