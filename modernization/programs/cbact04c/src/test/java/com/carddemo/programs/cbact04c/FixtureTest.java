package com.carddemo.programs.cbact04c;

import com.carddemo.programs.cbact04c.copybook.AccountRecord;
import com.carddemo.programs.cbact04c.copybook.CardXrefRecord;
import com.carddemo.programs.cbact04c.copybook.TranCatBalRecord;
import com.carddemo.programs.cbact04c.copybook.TranRecord;
import com.carddemo.programs.cbact04c.io.FixedWidth;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Runs the INTCALC step against the app/data/ASCII sample files (copied to src/test/resources/fixtures).
 * All sample accounts have a blank ACCT-GROUP-ID, so every lookup misses and falls back to DEFAULT/01/0001
 * (15.00%), exercising 1200-A-GET-DEFAULT-INT-RATE for every record.
 */
class FixtureTest {

    @TempDir
    Path dir;

    Path tcatbal;
    Path xref;
    Path acct;
    Path discgrp;
    Path transact;

    @BeforeEach
    void copyFixtures() throws IOException {
        tcatbal = copy("tcatbal.txt");
        xref = copy("cardxref.txt");
        acct = copy("acctdata.txt");
        discgrp = copy("discgrp.txt");
        transact = dir.resolve("systran.txt");
    }

    private Path copy(String name) throws IOException {
        Path target = dir.resolve(name);
        try (InputStream in = getClass().getResourceAsStream("/fixtures/" + name)) {
            Files.copy(in, target);
        }
        return target;
    }

    private int runJob() {
        return Cbact04cMain.execute(new String[] {
                "PARM=2022071800",
                "TCATBALF=" + tcatbal,
                "XREFFILE=" + xref,
                "ACCTFILE=" + acct,
                "DISCGRP=" + discgrp,
                "TRANSACT=" + transact
        }, Cbact04cTest.CLOCK);
    }

    @Test
    void sampleDataProducesOneZeroInterestTransactionPerCategoryBalance() throws IOException {
        List<AccountRecord> before = FixedWidth.readAll(acct, AccountRecord.CODEC);
        Map<Long, String> cardByAcct = FixedWidth.readAll(xref, CardXrefRecord.CODEC).stream()
                .collect(Collectors.toMap(CardXrefRecord::xrefAcctId, CardXrefRecord::xrefCardNum));

        assertEquals(0, runJob());

        List<TranRecord> trans = FixedWidth.readAll(transact, TranRecord.CODEC);
        assertEquals(50, trans.size());
        for (int i = 0; i < trans.size(); i++) {
            TranRecord t = trans.get(i);
            long acctId = i + 1;
            assertEquals(String.format("2022071800%06d", i + 1), t.tranId());
            assertEquals(String.format("Int. for a/c %011d", acctId), t.tranDesc().trim());
            assertEquals(new BigDecimal("0.00"), t.tranAmt());
            assertEquals(cardByAcct.get(acctId), t.tranCardNum());
            assertEquals(Cbact04cTest.TS, t.tranOrigTs());
        }
        assertTrue(Files.readString(transact, FixedWidth.CHARSET).lines().allMatch(l -> l.length() == 350));

        // Zero balances and zero cycle totals: rewrites leave every account record unchanged.
        assertEquals(before, FixedWidth.readAll(acct, AccountRecord.CODEC));
    }

    @Test
    void nonZeroBalancesOverSampleAccountsUseDefaultRateAndSkipLastAccount() throws IOException {
        List<TranCatBalRecord> balances = List.of(
                new TranCatBalRecord(1, "01", 1, new BigDecimal("1000.00")),
                new TranCatBalRecord(1, "01", 2, new BigDecimal("500.00")),
                new TranCatBalRecord(2, "02", 1, new BigDecimal("999.99")),
                new TranCatBalRecord(3, "01", 1, new BigDecimal("-240.00")),
                new TranCatBalRecord(4, "01", 1, new BigDecimal("100.00")));
        FixedWidth.writeAll(tcatbal, TranCatBalRecord.CODEC, balances);
        Map<Long, AccountRecord> before = byId(FixedWidth.readAll(acct, AccountRecord.CODEC));

        assertEquals(0, runJob());

        List<TranRecord> trans = FixedWidth.readAll(transact, TranRecord.CODEC);
        // DEFAULT 01/0001 15.00, 01/0002 25.00, 02/0001 0.00 (skipped), 01/0001 15.00, 01/0001 15.00
        assertEquals(List.of(new BigDecimal("12.50"), new BigDecimal("10.41"), new BigDecimal("-3.00"),
                new BigDecimal("1.25")), trans.stream().map(TranRecord::tranAmt).toList());

        Map<Long, AccountRecord> after = byId(FixedWidth.readAll(acct, AccountRecord.CODEC));
        assertEquals(before.get(1L).acctCurrBal().add(new BigDecimal("22.91")), after.get(1L).acctCurrBal());
        assertEquals(before.get(2L).acctCurrBal(), after.get(2L).acctCurrBal());
        assertEquals(before.get(3L).acctCurrBal().subtract(new BigDecimal("3.00")), after.get(3L).acctCurrBal());
        assertEquals(before.get(4L), after.get(4L));
    }

    @Test
    void missingInputFileAbendsWith999() {
        tcatbal = dir.resolve("does-not-exist.txt");
        assertEquals(999, runJob());
    }

    @Test
    void abendAfterTransactOpenDeletesNewTransactFile() throws IOException {
        Files.writeString(tcatbal, "99999999999010001" + "0000000100{" + "\n", FixedWidth.CHARSET);
        assertEquals(999, runJob());
        assertTrue(Files.notExists(transact));
    }

    @Test
    void overlongInputRecordAbendsWith999() throws IOException {
        Files.writeString(tcatbal, "X".repeat(51) + "\n", FixedWidth.CHARSET);
        assertEquals(999, runJob());
    }

    @Test
    void invalidNumericInIndexedFileAbendsWith999() throws IOException {
        Files.writeString(xref, "4111111111111111ABCDEFGHI00000000001\n", FixedWidth.CHARSET);
        assertEquals(999, runJob());
    }

    private static Map<Long, AccountRecord> byId(List<AccountRecord> records) {
        return records.stream().collect(Collectors.toMap(AccountRecord::acctId, Function.identity()));
    }
}
