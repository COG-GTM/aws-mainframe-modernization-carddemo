package com.carddemo.programs.cbexport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.carddemo.programs.cbexport.copybook.ExportRecord;
import com.carddemo.programs.cbexport.io.FixedLengthRecordReader;
import com.carddemo.programs.cbexport.io.ReadResult;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Runs the CLI end-to-end over the repo's sample data (app/data/ASCII), using dailytran.txt as
 * the TRANSACT file, and checks the 500-byte export file record by record.
 */
class CbexportFixtureTest {

    static final Path ASCII = Path.of("../../../app/data/ASCII");

    @TempDir
    static Path dir;
    static Path export;
    static List<ExportRecord> records;
    static String sysout;
    static int rc;

    @BeforeAll
    static void runExport() throws Exception {
        assumeTrue(Files.isDirectory(ASCII), "app/data/ASCII not found");
        export = dir.resolve("export.dat");
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        rc = CbexportMain.run(new String[] {
            "CUSTFILE=" + ASCII.resolve("custdata.txt"),
            "ACCTFILE=" + ASCII.resolve("acctdata.txt"),
            "XREFFILE=" + ASCII.resolve("cardxref.txt"),
            "TRANSACT=" + ASCII.resolve("dailytran.txt"),
            "CARDFILE=" + ASCII.resolve("carddata.txt"),
            "EXPFILE=" + export
        }, CbexportTest.CLOCK, new PrintStream(out, true, StandardCharsets.UTF_8), System.err);
        sysout = out.toString(StandardCharsets.UTF_8);

        records = new ArrayList<>();
        try (FixedLengthRecordReader r = new FixedLengthRecordReader(export, ExportRecord.LENGTH)) {
            r.open();
            for (ReadResult res = r.read(); res.record() != null; res = r.read()) {
                records.add(ExportRecord.fromBytes(res.record()));
            }
        }
    }

    @Test
    void exportsAllFiveHundredSampleRecords() throws Exception {
        assertEquals(CbexportMain.RC_OK, rc);
        assertEquals(500L * 500, Files.size(export));
        assertEquals(500, records.size());
        assertTrue(sysout.contains("CBEXPORT: Customers Exported: 000000050"));
        assertTrue(sysout.contains("CBEXPORT: Accounts Exported: 000000050"));
        assertTrue(sysout.contains("CBEXPORT: XRefs Exported: 000000050"));
        assertTrue(sysout.contains("CBEXPORT: Transactions Exported: 000000300"));
        assertTrue(sysout.contains("CBEXPORT: Cards Exported: 000000050"));
        assertTrue(sysout.contains("CBEXPORT: Total Records Exported: 000000500"));
    }

    @Test
    void recordTypesAreGroupedAndSequenceAscends() {
        StringBuilder expected = new StringBuilder();
        expected.append("C".repeat(50)).append("A".repeat(50)).append("X".repeat(50))
                .append("T".repeat(300)).append("D".repeat(50));
        StringBuilder actual = new StringBuilder();
        for (int i = 0; i < records.size(); i++) {
            actual.append(records.get(i).recType());
            assertEquals(i + 1, records.get(i).sequenceNum());
            assertEquals(CbexportTest.TIMESTAMP, records.get(i).timestamp());
        }
        assertEquals(expected.toString(), actual.toString());
    }

    @Test
    void firstCustomerMatchesCustdata() {
        var c = records.get(0).customerData();
        assertEquals(1, c.custId());
        assertEquals("Immanuel", c.firstName().strip());
        assertEquals("Kessler", c.lastName().strip());
        assertEquals("Altenwerthshire", c.addrLines().get(2).strip());
        assertEquals(20973888L, c.ssn());
        assertEquals(274, c.ficoCreditScore());
    }

    @Test
    void firstAccountMatchesAcctdata() {
        var a = records.get(50).accountData();
        assertEquals(1, a.acctId());
        assertEquals(new BigDecimal("194.00"), a.currBal());
        assertEquals(new BigDecimal("2020.00"), a.creditLimit());
        assertEquals(new BigDecimal("1020.00"), a.cashCreditLimit());
        assertEquals("2014-11-20", a.openDate());
    }

    @Test
    void xrefTransactionAndCardMatchSampleData() {
        var x = records.get(100).cardXrefData();
        assertEquals("0500024453765740", x.cardNum());
        assertEquals(50, x.custId());
        assertEquals(50, x.acctId());

        var t1 = records.get(150).transactionData();
        assertEquals("0000000000683580", t1.tranId());
        assertEquals(new BigDecimal("504.77"), t1.amt());
        assertEquals(800000000L, t1.merchantId());
        var t2 = records.get(151).transactionData();
        assertEquals(new BigDecimal("-919.00"), t2.amt());

        var d = records.get(450).cardData();
        assertEquals("0500024453765740", d.cardNum());
        assertEquals(747, d.cvvCd());
        assertEquals("Aniya Von", d.embossedName().strip());
    }
}
