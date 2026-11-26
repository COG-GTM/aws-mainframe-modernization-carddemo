package com.carddemo.programs.coacct01.copybook;

import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AccountRecordTest {

    static List<String> fixtureLines() throws Exception {
        try (BufferedReader r = new BufferedReader(new InputStreamReader(
                AccountRecordTest.class.getResourceAsStream("/acctdata.txt"), StandardCharsets.US_ASCII))) {
            return r.lines().toList();
        }
    }

    @Test
    void parsesFirstFixtureRecordPerCvact01yLayout() throws Exception {
        AccountRecord a = AccountRecord.parse(fixtureLines().get(0));
        assertEquals(1L, a.acctId());
        assertEquals("Y", a.activeStatus());
        assertEquals(new BigDecimal("194.00"), a.currBal());
        assertEquals(new BigDecimal("2020.00"), a.creditLimit());
        assertEquals(new BigDecimal("1020.00"), a.cashCreditLimit());
        assertEquals("2014-11-20", a.openDate());
        assertEquals("2025-05-20", a.expirationDate());
        assertEquals("2025-05-20", a.reissueDate());
        assertEquals(new BigDecimal("0.00"), a.currCycCredit());
        assertEquals(new BigDecimal("0.00"), a.currCycDebit());
        assertEquals("A000000000", a.addrZip());
        assertEquals("          ", a.groupId());
    }

    @Test
    void everyFixtureRecordRoundTripsToTheSame300Bytes() throws Exception {
        List<String> lines = fixtureLines();
        assertEquals(50, lines.size());
        for (String line : lines) {
            String out = AccountRecord.parse(line).toFixedWidth();
            assertEquals(AccountRecord.RECORD_LENGTH, out.length());
            assertEquals(line, out);
        }
    }

    @Test
    void constructorAppliesPicTruncation() {
        AccountRecord a = new AccountRecord(123456789012L, "YES", new BigDecimal("12345678901.239"),
                BigDecimal.ONE, BigDecimal.ONE, "2020-01-01X", "", "", BigDecimal.ZERO,
                new BigDecimal("-5.5"), "", "GROUP-ID-LONG");
        assertEquals(23456789012L, a.acctId());
        assertEquals("Y", a.activeStatus());
        assertEquals(new BigDecimal("2345678901.23"), a.currBal());
        assertEquals("2020-01-01", a.openDate());
        assertEquals(new BigDecimal("-5.50"), a.currCycDebit());
        assertEquals("GROUP-ID-L", a.groupId());
        assertEquals("23456789012", a.key());
    }
}
