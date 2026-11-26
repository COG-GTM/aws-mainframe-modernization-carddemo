package com.carddemo.programs.cbimport;

import com.carddemo.programs.cbimport.copybook.AccountRecord;
import com.carddemo.programs.cbimport.copybook.CardRecord;
import com.carddemo.programs.cbimport.copybook.CardXrefRecord;
import com.carddemo.programs.cbimport.copybook.CustomerRecord;
import com.carddemo.programs.cbimport.copybook.TranRecord;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Runs the port end to end over app/data/EBCDIC/AWS.M2.CARDDEMO.EXPORT.DATA.PS (500 records). */
class CbimportFixtureTest {

    static final Charset EBCDIC = Charset.forName("IBM037");
    static final Path EXPORT = Path.of(System.getProperty("carddemo.data.dir", "../../../app/data"),
            "EBCDIC", "AWS.M2.CARDDEMO.EXPORT.DATA.PS");

    @TempDir
    Path dir;

    final ByteArrayOutputStream sysoutBytes = new ByteArrayOutputStream();
    final PrintStream sysout = new PrintStream(sysoutBytes, true, StandardCharsets.UTF_8);

    String[] args(boolean withCardout, String... extra) {
        List<String> a = new ArrayList<>(List.of("--EXPFILE=" + EXPORT,
                "--CUSTOUT=" + dir.resolve("cust"), "--ACCTOUT=" + dir.resolve("acct"),
                "--XREFOUT=" + dir.resolve("xref"), "--TRNXOUT=" + dir.resolve("tran"),
                "--ERROUT=" + dir.resolve("err")));
        if (withCardout) {
            a.add("--CARDOUT=" + dir.resolve("card"));
        }
        a.addAll(List.of(extra));
        return a.toArray(String[]::new);
    }

    List<String> records(String name, int lrecl) throws IOException {
        byte[] bytes = Files.readAllBytes(dir.resolve(name));
        List<String> out = new ArrayList<>();
        for (int i = 0; i < bytes.length; i += lrecl) {
            out.add(new String(bytes, i, lrecl, EBCDIC));
        }
        return out;
    }

    @Test
    void importsTheSampleExportIntoFixedBlockEbcdicFiles() throws IOException {
        assertEquals(0, CbimportMain.run(args(true), CbimportTest.CLOCK, sysout));

        assertEquals(50 * 500, Files.size(dir.resolve("cust")));
        assertEquals(50 * 300, Files.size(dir.resolve("acct")));
        assertEquals(50 * 50, Files.size(dir.resolve("xref")));
        assertEquals(300 * 350, Files.size(dir.resolve("tran")));
        assertEquals(50 * 150, Files.size(dir.resolve("card")));
        assertEquals(0, Files.size(dir.resolve("err")));

        String log = sysoutBytes.toString(StandardCharsets.UTF_8);
        assertTrue(log.contains("CBIMPORT: Total Records Read: 000000500"));
        assertTrue(log.contains("CBIMPORT: Customers Imported: 000000050"));
        assertTrue(log.contains("CBIMPORT: Accounts Imported: 000000050"));
        assertTrue(log.contains("CBIMPORT: XRefs Imported: 000000050"));
        assertTrue(log.contains("CBIMPORT: Transactions Imported: 000000300"));
        assertTrue(log.contains("CBIMPORT: Cards Imported: 000000050"));
        assertTrue(log.contains("CBIMPORT: Errors Written: 000000000"));
        assertTrue(log.contains("CBIMPORT: Unknown Record Types: 000000000"));

        CustomerRecord c = CustomerRecord.parse(records("cust", 500).get(0));
        assertEquals(1, c.custId());
        assertEquals("IMMANUEL", c.firstName().strip());
        assertEquals("MATHEUS", c.lastName().strip());
        assertEquals("ALTENWERTHSHIRE", c.addrLine3().strip());
        assertEquals("NY", c.addrStateCd());
        assertEquals(20973888, c.ssn());
        assertEquals("1979-06-08", c.dobYyyyMmDd());
        assertEquals(300, c.ficoCreditScore());

        List<String> accts = records("acct", 300);
        AccountRecord a1 = AccountRecord.parse(accts.get(0));
        assertEquals(1, a1.acctId());
        assertEquals("00000000000{", accts.get(0).substring(12, 24));
        assertEquals(new BigDecimal("2020.00"), a1.creditLimit());
        assertEquals(new BigDecimal("1020.00"), a1.cashCreditLimit());
        assertEquals("2020-10-22", a1.openDate());
        assertEquals(new BigDecimal("158.00"), AccountRecord.parse(accts.get(1)).currBal());

        assertEquals("050002445376574000000005000000000050", records("xref", 50).get(0).stripTrailing());
        CardXrefRecord x = CardXrefRecord.parse(records("xref", 50).get(0));
        assertEquals(50, x.acctId());

        List<String> trans = records("tran", 350);
        TranRecord t = TranRecord.parse(trans.get(0));
        assertEquals("0000000000683580", t.tranId());
        assertEquals("0000005047G", trans.get(0).substring(132, 143));
        assertEquals(new BigDecimal("504.77"), t.amt());
        assertEquals(800000000, t.merchantId());
        assertEquals(50, trans.stream().filter(r -> TranRecord.parse(r).amt().signum() < 0).count());

        CardRecord d = CardRecord.parse(records("card", 150).get(0));
        assertEquals("0500024453765740", d.cardNum());
        assertEquals(50, d.acctId());
        assertEquals(747, d.cvvCd());
        assertEquals("Aniya Von", d.embossedName().strip());
    }

    @Test
    void asciiLineSequentialOutputMatchesAppDataAsciiStyle() throws IOException {
        assertEquals(0, CbimportMain.run(args(true, "--output-encoding=US-ASCII", "--record-separator=lf"),
                CbimportTest.CLOCK, sysout));
        List<String> lines = Files.readAllLines(dir.resolve("acct"), StandardCharsets.US_ASCII);
        assertEquals(50, lines.size());
        assertTrue(lines.get(0).startsWith("00000000001Y00000000000{00000020200{00000010200{2020-10-22"));
    }

    @Test
    void shippedJclHasNoCardoutDdSoTheStepAbendsAndOutputsAreDeleted() {
        assertEquals(CbimportMain.ABEND_EXIT_CODE, CbimportMain.run(args(false), CbimportTest.CLOCK, sysout));
        String log = sysoutBytes.toString(StandardCharsets.UTF_8);
        assertTrue(log.contains("ERROR: Cannot open CARD-OUTPUT, Status: 35"));
        assertTrue(log.contains("CBIMPORT: ABENDING PROGRAM"));
        for (String f : List.of("cust", "acct", "xref", "tran", "err")) {
            assertFalse(Files.exists(dir.resolve(f)), f);
        }
    }

    @Test
    void missingExportFileAbendsWithStatus35() {
        String[] a = args(true);
        a[0] = "--EXPFILE=" + dir.resolve("nope");
        assertEquals(CbimportMain.ABEND_EXIT_CODE, CbimportMain.run(a, CbimportTest.CLOCK, sysout));
        assertTrue(sysoutBytes.toString(StandardCharsets.UTF_8)
                .contains("ERROR: Cannot open EXPORT-INPUT, Status: 35"));
    }

    @Test
    void truncatedExportRecordAbendsOnRead() throws IOException {
        Path shortFile = dir.resolve("short");
        byte[] all = Files.readAllBytes(EXPORT);
        Files.write(shortFile, java.util.Arrays.copyOf(all, 700));
        String[] a = args(true);
        a[0] = "--EXPFILE=" + shortFile;
        assertEquals(CbimportMain.ABEND_EXIT_CODE, CbimportMain.run(a, CbimportTest.CLOCK, sysout));
        assertTrue(sysoutBytes.toString(StandardCharsets.UTF_8)
                .contains("ERROR: Reading EXPORT-INPUT, Status: 04"));
    }

    @Test
    void abendDoesNotDeleteOutputsThatExistedBeforeTheRun() throws IOException {
        Path previous = Files.writeString(dir.resolve("cust"), "previous import");
        String[] a = args(true);
        a[0] = "--EXPFILE=" + dir.resolve("nope");
        assertEquals(CbimportMain.ABEND_EXIT_CODE, CbimportMain.run(a, CbimportTest.CLOCK, sysout));
        assertEquals("previous import", Files.readString(previous));

        sysoutBytes.reset();
        assertEquals(CbimportMain.ABEND_EXIT_CODE, CbimportMain.run(args(true), CbimportTest.CLOCK, sysout));
        assertTrue(sysoutBytes.toString(StandardCharsets.UTF_8)
                .contains("ERROR: Cannot open CUSTOMER-OUTPUT, Status: 30"));
        assertEquals("previous import", Files.readString(previous));
    }

    @org.junit.jupiter.params.ParameterizedTest
    @org.junit.jupiter.params.provider.ValueSource(strings = {"--output-encoding=UTF-16", "--output-encoding=UTF-8",
            "--output-encoding=IBM273", "--export-encoding=IBM273", "--export-encoding=NO-SUCH-CHARSET"})
    void encodingsThatBreakFixedWidthOrOverpunchAreRejected(String option) {
        assertEquals(CbimportMain.USAGE_EXIT_CODE, CbimportMain.run(args(true, option), CbimportTest.CLOCK, sysout));
        assertTrue(sysoutBytes.toString(StandardCharsets.UTF_8).contains("CBIMPORT: Unsupported encoding: "));
        assertFalse(Files.exists(dir.resolve("cust")));
    }

    @Test
    void invalidArgumentIsRejected() {
        assertEquals(CbimportMain.USAGE_EXIT_CODE, CbimportMain.run(new String[] {"EXPFILE"}, CbimportTest.CLOCK, sysout));
    }
}
