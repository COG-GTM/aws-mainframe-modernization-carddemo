package com.carddemo.programs.cotrtlic.gateway;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import org.junit.jupiter.api.Test;

class InMemoryTransactionTypeTableTest {

    @Test
    void likeSupportsPercentAndUnderscore() {
        assertTrue(InMemoryTransactionTypeTable.like("Purchase", "%chas%"));
        assertTrue(InMemoryTransactionTypeTable.like("Purchase", "P_rchase"));
        assertTrue(InMemoryTransactionTypeTable.like("", "%"));
        assertFalse(InMemoryTransactionTypeTable.like("Purchase", "%pur%"), "case sensitive");
        assertFalse(InMemoryTransactionTypeTable.like("Purchase", "Purchase_"));
        assertTrue(InMemoryTransactionTypeTable.like("a.b", "a.b"), "regex metacharacters are literal");
    }

    @Test
    void wildcardHeavyPatternDoesNotBacktrack() {
        String value = "a".repeat(50);
        String pattern = "%a".repeat(25) + "%b%";
        assertTimeoutPreemptively(Duration.ofMillis(500),
                () -> assertFalse(InMemoryTransactionTypeTable.like(value, pattern)));
    }
}
