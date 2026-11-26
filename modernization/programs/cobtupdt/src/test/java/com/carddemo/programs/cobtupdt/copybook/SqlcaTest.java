package com.carddemo.programs.cobtupdt.copybook;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SqlcaTest {

    @Test
    void conditionsOnSqlcode() {
        Sqlca s = new Sqlca();
        assertEquals("SQLCA   ", s.sqlcaid());
        assertEquals(136, s.sqlcabc());
        assertTrue(s.isOk());
        s.setSqlcode(100);
        assertTrue(s.isNotFound());
        assertFalse(s.isError());
        s.setSqlcode(-803);
        assertTrue(s.isError());
        assertFalse(s.isOk());
    }

    @Test
    void fieldSizes() {
        Sqlca s = new Sqlca();
        s.setSqlerrmc("TR_TYPE");
        assertEquals(70, s.sqlerrmc().length());
        assertEquals(7, s.sqlerrml());
        s.setSqlstate("235051");
        assertEquals("23505", s.sqlstate());
        s.setSqlerrd(3, 1);
        assertEquals(1, s.sqlerrd(3));
        assertEquals(' ', s.sqlwarn(0));
        s.setSqlerrp("DSNXRINS9");
        assertEquals("DSNXRINS", s.sqlerrp());
    }
}
