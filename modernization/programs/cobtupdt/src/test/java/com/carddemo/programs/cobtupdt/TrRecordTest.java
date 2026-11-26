package com.carddemo.programs.cobtupdt;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TrRecordTest {

    @Test
    void parsesColumnsPerMnttrdb2Layout() {
        TrRecord r = TrRecord.parse("U03Credit");
        assertEquals("U", r.type());
        assertEquals("03", r.number());
        assertEquals("Credit" + " ".repeat(44), r.desc());
        assertEquals(53, r.toFixedWidth().length());
    }

    @Test
    void shortRecordIsPaddedWithSpaces() {
        TrRecord r = TrRecord.parse("D");
        assertEquals("D", r.type());
        assertEquals("  ", r.number());
        assertEquals(" ".repeat(50), r.desc());
    }

    @Test
    void longRecordIsTruncatedTo53() {
        TrRecord r = TrRecord.parse("A01" + "d".repeat(70));
        assertEquals("d".repeat(50), r.desc());
    }

    @Test
    void spacesConstantMatchesValueSpaces() {
        assertEquals(" ".repeat(TrRecord.RECORD_LENGTH), TrRecord.SPACES.toFixedWidth());
    }

    @Test
    void recordActionCodes() {
        assertEquals(RecordAction.ADD, RecordAction.fromCode("A").orElseThrow());
        assertEquals(RecordAction.UPDATE, RecordAction.fromCode("U").orElseThrow());
        assertEquals(RecordAction.DELETE, RecordAction.fromCode("D").orElseThrow());
        assertEquals(RecordAction.COMMENT, RecordAction.fromCode("*").orElseThrow());
        assertTrue(RecordAction.fromCode("a").isEmpty());
        assertTrue(RecordAction.fromCode(" ").isEmpty());
        assertTrue(RecordAction.fromCode(null).isEmpty());
    }
}
