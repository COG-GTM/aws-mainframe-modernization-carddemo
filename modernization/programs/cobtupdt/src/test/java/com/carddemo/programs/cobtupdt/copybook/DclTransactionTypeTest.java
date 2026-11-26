package com.carddemo.programs.cobtupdt.copybook;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class DclTransactionTypeTest {

    @Test
    void varcharHostStructure() {
        DclTransactionType d = DclTransactionType.of("01", "Purchase");
        assertEquals("01", d.trType());
        assertEquals(8, d.trDescriptionLen());
        assertEquals("Purchase" + " ".repeat(42), d.trDescriptionText());
        assertEquals("Purchase", d.trDescription());
    }

    @Test
    void picSizesAreEnforced() {
        DclTransactionType d = DclTransactionType.of("123", "x".repeat(60));
        assertEquals("12", d.trType());
        assertEquals(50, d.trDescriptionLen());
        assertEquals(50, d.trDescriptionText().length());
        assertThrows(IllegalArgumentException.class, () -> new DclTransactionType("01", (short) 51, ""));
    }

    @Test
    void picXMove() {
        assertEquals("ab   ", Pic.alphanumeric("ab", 5));
        assertEquals("abc", Pic.alphanumeric("abcdef", 3));
        assertEquals("   ", Pic.alphanumeric(null, 3));
    }
}
