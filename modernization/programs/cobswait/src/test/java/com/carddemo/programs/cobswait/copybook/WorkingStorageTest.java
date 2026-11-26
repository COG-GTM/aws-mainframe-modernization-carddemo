package com.carddemo.programs.cobswait.copybook;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class WorkingStorageTest {

    @Test
    void parmValueTruncatesToPicX8() {
        assertEquals("00003600", ParmValue.of("00003600      VALUE IN CENTISECONDS").value());
    }

    @Test
    void parmValueSpacePadsToPicX8() {
        assertEquals("36      ", ParmValue.of("36").value());
        assertEquals("        ", ParmValue.SPACES.value());
        assertEquals("        ", ParmValue.of(null).value());
    }

    @Test
    void parmValueRejectsWrongLength() {
        assertThrows(IllegalArgumentException.class, () -> new ParmValue("123"));
    }

    @Test
    void mvswaitTimeHoldsPic9of8Range() {
        assertEquals(99_999_999L, new MvswaitTime(MvswaitTime.MAX).centiseconds());
        assertThrows(IllegalArgumentException.class, () -> new MvswaitTime(100_000_000L));
        assertThrows(IllegalArgumentException.class, () -> new MvswaitTime(-1));
    }

    @Test
    void mvswaitTimeIsCentiseconds() {
        assertEquals(36_000L, new MvswaitTime(3600).millis());
    }
}
