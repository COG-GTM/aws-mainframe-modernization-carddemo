package com.carddemo.poc.copybook;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CardXrefRecordTest {

    private static final String FIRST_SAMPLE_RECORD =
            "0500024453765740" + "000000050" + "00000000050" + "              ";

    @Test
    void splitsFixedWidthFields() {
        CardXrefRecord rec = CardXrefRecord.fromImage(FIRST_SAMPLE_RECORD);
        assertEquals("0500024453765740", rec.getCardNum());
        assertEquals(50L, rec.getCustId());
        assertEquals(50L, rec.getAcctId());
        assertEquals(FIRST_SAMPLE_RECORD, rec.toDisplayString());
    }

    @Test
    void rejectsWrongLength() {
        assertThrows(IllegalArgumentException.class, () -> CardXrefRecord.fromImage("too short"));
    }
}
