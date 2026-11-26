package com.carddemo.programs.cobtupdt;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class InMemoryTransactionTypeGatewayTest {

    @Test
    void db2Sqlcodes() {
        InMemoryTransactionTypeGateway g = new InMemoryTransactionTypeGateway();
        assertEquals(0, g.insert("01", "Purchase"));
        assertEquals(-803, g.insert("01", "Purchase"));
        assertEquals(0, g.update("01", "Buy"));
        assertEquals(100, g.update("02", "x"));
        assertEquals(0, g.delete("01"));
        assertEquals(100, g.delete("01"));
        assertTrue(g.rows().isEmpty());
    }

    @Test
    void fetchFillsDclgenStructure() {
        InMemoryTransactionTypeGateway g = new InMemoryTransactionTypeGateway().seed("05", "Refund");
        var dcl = g.fetch("05").orElseThrow();
        assertEquals("05", dcl.trType());
        assertEquals(6, dcl.trDescriptionLen());
        assertEquals("Refund", dcl.trDescription());
    }

    @Test
    void forcedSqlcodeLeavesTableUntouched() {
        InMemoryTransactionTypeGateway g = new InMemoryTransactionTypeGateway().failNextWith(-911);
        assertEquals(-911, g.insert("01", "x"));
        assertTrue(g.rows().isEmpty());
        assertEquals(0, g.insert("01", "x"));
    }
}
