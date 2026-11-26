package com.carddemo.programs.copaua0c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.copaua0c.copybook.AccountRecord;
import com.carddemo.programs.copaua0c.copybook.CardXrefRecord;
import com.carddemo.programs.copaua0c.copybook.CustomerRecord;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Runs COPAUA0C against the CardDemo sample VSAM data in app/data/ASCII. */
class Copaua0cFixtureTest {

    private static final Path DATA = Path.of("..", "..", "..", "app", "data", "ASCII");
    private static final String CARD = "0500024453765740";

    private Harness h;

    private static List<String> lines(String name) throws IOException {
        return Files.readAllLines(DATA.resolve(name), StandardCharsets.UTF_8);
    }

    @BeforeEach
    void load() throws IOException {
        h = new Harness();
        lines("cardxref.txt").forEach(l -> h.xref.put(CardXrefRecord.fromFixed(l)));
        lines("acctdata.txt").forEach(l -> h.acct.put(AccountRecord.fromFixed(l)));
        lines("custdata.txt").forEach(l -> h.cust.put(CustomerRecord.fromFixed(l)));
    }

    @Test
    void sampleFilesParseToCopybookLayouts() throws IOException {
        assertEquals(50, h.xref.size());
        assertEquals(50, h.acct.size());
        assertEquals(50, h.cust.size());
        lines("acctdata.txt").forEach(l -> assertEquals(AccountRecord.RECORD_LENGTH, l.length()));
        lines("custdata.txt").forEach(l -> assertEquals(CustomerRecord.RECORD_LENGTH, l.length()));
        AccountRecord a = h.acct.read("00000000050").record();
        assertEquals(new BigDecimal("492.00"), a.currBal());
        assertEquals(new BigDecimal("6169.00"), a.creditLimit());
        assertEquals(new BigDecimal("4587.00"), a.cashCreditLimit());
        CardXrefRecord x = h.xref.read(CARD).record();
        assertEquals(50L, x.acctId());
        assertEquals(50L, x.custId());
    }

    @Test
    void sampleAccountApprovesUpToAvailableAndDeclinesAbove() {
        // Message 1 decides on ACCT master (6169.00 - 492.00); message 2 on the new summary (6169.00 - 5677.00).
        h.send(CARD, "5677.00", "TXN000000000001").send(CARD, "492.01", "TXN000000000002")
                .send(CARD, "492.00", "TXN000000000003");
        h.run();
        String payload = h.mq.replies().get(2).payload();
        assertTrue(payload.startsWith(CARD + ",TXN000000000001,101530,00,0000,       5677.00,"));
        assertTrue(payload.contains(CARD + ",TXN000000000002,101530,05,4100,          0.00,"));
        assertTrue(payload.contains(CARD + ",TXN000000000003,101530,00,0000,        492.00,"));
        assertEquals(new BigDecimal("6169.00"), h.ims.summary(50L).creditLimit());
        assertEquals(new BigDecimal("6169.00"), h.ims.summary(50L).creditBalance());
        assertEquals(new BigDecimal("4587.00"), h.ims.summary(50L).cashLimit());
        assertTrue(h.tdq.entries().isEmpty());
    }
}
