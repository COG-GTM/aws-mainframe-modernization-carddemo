package com.carddemo.programs.cbact03c.copybook;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CardXrefRecordTest {

    private static final String IMAGE = "050002445376574000000005000000000050              ";

    @Test
    void layoutIsFiftyBytesWithCopybookOffsets() {
        assertEquals(50, CardXrefRecord.RECORD_LENGTH);
        assertEquals(CardXrefRecord.RECORD_LENGTH, CardXrefRecord.CARD_NUM_LENGTH + CardXrefRecord.CUST_ID_DIGITS
                + CardXrefRecord.ACCT_ID_DIGITS + CardXrefRecord.FILLER_LENGTH);
    }

    @Test
    void parsesFixtureRecord() {
        CardXrefRecord r = CardXrefRecord.fromRecord(IMAGE);
        assertEquals("0500024453765740", r.cardNum());
        assertEquals(50L, r.custId());
        assertEquals(50L, r.acctId());
        assertEquals(" ".repeat(14), r.filler());
        assertEquals("0500024453765740", r.key());
    }

    @Test
    void roundTripsByteForByte() {
        assertEquals(IMAGE, CardXrefRecord.fromRecord(IMAGE).toRecord());
    }

    @Test
    void shortImageIsSpacePadded() {
        CardXrefRecord r = CardXrefRecord.fromRecord(IMAGE.strip());
        assertEquals(IMAGE, r.toRecord());
    }

    @Test
    void formatsNumericFieldsZeroFilledAndAlphanumericSpacePadded() {
        CardXrefRecord r = new CardXrefRecord("4111", 123, 45);
        assertEquals("4111            00000012300000000045              ", r.toRecord());
    }

    @Test
    void numericMoveTruncatesHighOrderDigitsAndDropsSign() {
        CardXrefRecord r = new CardXrefRecord("X", 1_234_567_890L, -123_456_789_012L);
        assertEquals(234_567_890L, r.custId());
        assertEquals(23_456_789_012L, r.acctId());
    }

    @Test
    void alphanumericMoveTruncatesOnTheRight() {
        CardXrefRecord r = new CardXrefRecord("12345678901234567890", 0, 0, "F".repeat(20));
        assertEquals("1234567890123456", r.cardNum());
        assertEquals("F".repeat(14), r.filler());
    }

    @Test
    void nonNumericIdIsRejected() {
        String bad = "0500024453765740ABC000005000000000050";
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> CardXrefRecord.fromRecord(bad));
        assertTrue(e.getMessage().contains("XREF-CUST-ID"));
    }

    @Test
    void picIsNumeric() {
        assertTrue(Pic.isNumeric("00"));
        assertFalse(Pic.isNumeric("9 "));
        assertFalse(Pic.isNumeric(""));
    }
}
