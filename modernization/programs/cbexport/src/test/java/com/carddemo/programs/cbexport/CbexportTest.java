package com.carddemo.programs.cbexport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cbexport.copybook.AccountRecord;
import com.carddemo.programs.cbexport.copybook.CardRecord;
import com.carddemo.programs.cbexport.copybook.CardXrefRecord;
import com.carddemo.programs.cbexport.copybook.CustomerRecord;
import com.carddemo.programs.cbexport.copybook.ExportAccountData;
import com.carddemo.programs.cbexport.copybook.ExportRecord;
import com.carddemo.programs.cbexport.copybook.ExportRecordType;
import com.carddemo.programs.cbexport.copybook.TranRecord;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CbexportTest {

    static final ZoneId ZONE = ZoneId.of("Australia/Sydney");
    static final Clock CLOCK = Clock.fixed(
            LocalDateTime.of(2026, 11, 26, 17, 22, 5, 370_000_000).atZone(ZONE).toInstant(), ZONE);
    static final String TIMESTAMP = "2026-11-26 17:22:05.00    ";

    static final CustomerRecord CUSTOMER = new CustomerRecord(1, "Immanuel", "Madeline", "Kessler",
            "618 Deshaun Route", "Apt. 802", "Altenwerthshire", "NC", "USA", "12546",
            "(908)119-8310", "(373)693-8684", 20973888L, "00000000000049368437", "1961-06-08",
            "0053581756", "Y", 274, "");
    static final AccountRecord ACCOUNT = new AccountRecord(1, "Y", new BigDecimal("-194.00"),
            new BigDecimal("2020.00"), new BigDecimal("1020.00"), "2014-11-20", "2025-05-20", "2025-05-20",
            new BigDecimal("12.34"), new BigDecimal("-56.78"), "A000000000", "GRP1", "");
    static final CardXrefRecord XREF = new CardXrefRecord("0500024453765740", 50, 50, "");
    static final TranRecord TRAN = new TranRecord("0000000001774260", "03", 1, "OPERATOR",
            "Return item", new BigDecimal("-919.00"), 800000000L, "Nitzsche", "Fidelshire", "53378",
            "0927987108636232", "2022-06-10 19:27:53.000000", "", "");
    static final CardRecord CARD = new CardRecord("0500024453765740", 50, 747, "Aniya Von", "2023-03-09", "Y", "");

    InMemoryFiles.Reader cust;
    InMemoryFiles.Reader acct;
    InMemoryFiles.Reader xref;
    InMemoryFiles.Reader tran;
    InMemoryFiles.Reader card;
    InMemoryFiles.Writer exp;
    List<String> sysout;

    @BeforeEach
    void setUp() {
        cust = new InMemoryFiles.Reader(List.of(CUSTOMER.toBytes()));
        acct = new InMemoryFiles.Reader(List.of(ACCOUNT.toBytes()));
        xref = new InMemoryFiles.Reader(List.of(XREF.toBytes()));
        tran = new InMemoryFiles.Reader(List.of(TRAN.toBytes()));
        card = new InMemoryFiles.Reader(List.of(CARD.toBytes()));
        exp = new InMemoryFiles.Writer();
        sysout = new ArrayList<>();
    }

    Cbexport program() {
        return new Cbexport(new CbexportFiles(cust, acct, xref, tran, card, exp), CLOCK, sysout::add);
    }

    ExportRecord written(int i) {
        return ExportRecord.fromBytes(exp.written.get(i));
    }

    @Test
    void mainProcessingWritesOneRecordPerInputInFixedOrder() {
        ExportStatistics stats = program().run();

        assertEquals(new ExportStatistics(1, 1, 1, 1, 1, 5), stats);
        assertEquals(5, exp.written.size());
        String types = "";
        for (int i = 0; i < 5; i++) {
            ExportRecord r = written(i);
            types += r.recType();
            assertEquals(i + 1, r.sequenceNum());
            assertEquals(TIMESTAMP, r.timestamp());
            assertEquals("0001", r.branchId());
            assertEquals("NORTH", r.regionCode());
            assertEquals(500, exp.written.get(i).length);
        }
        assertEquals("CAXTD", types);
        assertTrue(cust.closed && acct.closed && xref.closed && tran.closed && card.closed && exp.closed);
    }

    @Test
    void displayOutputMatchesCobolSysout() {
        program().run();
        assertEquals(List.of(
                "CBEXPORT: Starting Customer Data Export",
                "CBEXPORT: Export Date: 2026-11-26",
                "CBEXPORT: Export Time: 17:22:05",
                "CBEXPORT: Processing customer records",
                "CBEXPORT: Customers exported: 000000001",
                "CBEXPORT: Processing account records",
                "CBEXPORT: Accounts exported: 000000001",
                "CBEXPORT: Processing cross-reference records",
                "CBEXPORT: Cross-references exported: 000000001",
                "CBEXPORT: Processing transaction records",
                "CBEXPORT: Transactions exported: 000000001",
                "CBEXPORT: Processing card records",
                "CBEXPORT: Cards exported: 000000001",
                "CBEXPORT: Export completed",
                "CBEXPORT: Customers Exported: 000000001",
                "CBEXPORT: Accounts Exported: 000000001",
                "CBEXPORT: XRefs Exported: 000000001",
                "CBEXPORT: Transactions Exported: 000000001",
                "CBEXPORT: Cards Exported: 000000001",
                "CBEXPORT: Total Records Exported: 000000005"), sysout);
    }

    @Test
    void generateTimestampFormatsDateTimeAndLiteralHundredths() {
        Cbexport p = program();
        p.generateTimestamp();
        assertEquals(TIMESTAMP, p.formattedTimestamp());
        ExportRecord r = new ExportRecord();
        r.setTimestamp(p.formattedTimestamp());
        assertEquals("2026-11-26", r.exportDate());
        assertEquals("17:22:05.00    ", r.exportTime());
    }

    @Test
    void stringIntoDoesNotSpaceFillTheReceiver() {
        assertEquals("abcXYZ", Cbexport.stringInto("123XYZ", "abc"));
        assertEquals("abc", Cbexport.stringInto("123", "abcdef"));
    }

    @Test
    void customerExportRecordMapsEveryField() {
        program().run();
        ExportRecord r = written(0);
        assertEquals(ExportRecordType.CUSTOMER, r.recordType());
        var c = r.customerData();
        assertEquals(1, c.custId());
        assertEquals("Immanuel", c.firstName().strip());
        assertEquals("Madeline", c.middleName().strip());
        assertEquals("Kessler", c.lastName().strip());
        assertEquals(List.of("618 Deshaun Route", "Apt. 802", "Altenwerthshire"),
                c.addrLines().stream().map(String::strip).toList());
        assertEquals("NC", c.addrStateCd());
        assertEquals("USA", c.addrCountryCd());
        assertEquals("12546", c.addrZip().strip());
        assertEquals(List.of("(908)119-8310", "(373)693-8684"), c.phoneNums().stream().map(String::strip).toList());
        assertEquals(20973888L, c.ssn());
        assertEquals("00000000000049368437", c.govtIssuedId());
        assertEquals("1961-06-08", c.dobYyyyMmDd());
        assertEquals("0053581756", c.eftAccountId());
        assertEquals("Y", c.priCardHolderInd());
        assertEquals(274, c.ficoCreditScore());
    }

    @Test
    void accountExportRecordPreservesSignsAcrossUsages() {
        program().run();
        ExportRecord r = written(1);
        assertEquals(ExportRecordType.ACCOUNT, r.recordType());
        assertEquals(new ExportAccountData(1, "Y", new BigDecimal("-194.00"), new BigDecimal("2020.00"),
                new BigDecimal("1020.00"), "2014-11-20", "2025-05-20", "2025-05-20",
                new BigDecimal("12.34"), new BigDecimal("-56.78"), "A000000000", "GRP1      "), r.accountData());
    }

    @Test
    void xrefTransactionAndCardRecordsMapFields() {
        program().run();
        var x = written(2).cardXrefData();
        assertEquals("0500024453765740", x.cardNum());
        assertEquals(50, x.custId());
        assertEquals(50, x.acctId());

        var t = written(3).transactionData();
        assertEquals("0000000001774260", t.tranId());
        assertEquals("03", t.typeCd());
        assertEquals(1, t.catCd());
        assertEquals("OPERATOR  ", t.source());
        assertEquals(new BigDecimal("-919.00"), t.amt());
        assertEquals(800000000L, t.merchantId());
        assertEquals("2022-06-10 19:27:53.000000", t.origTs());
        assertEquals(" ".repeat(26), t.procTs());

        var d = written(4).cardData();
        assertEquals("0500024453765740", d.cardNum());
        assertEquals(50, d.acctId());
        assertEquals(747, d.cvvCd());
        assertEquals("Aniya Von", d.embossedName().strip());
        assertEquals("2023-03-09", d.expiraionDate());
        assertEquals("Y", d.activeStatus());
    }

    @Test
    void fillerAreaOfEachVariantIsSpaces() {
        program().run();
        int[][] fillers = {{326, 134}, {108, 352}, {33, 427}, {320, 140}, {87, 373}};
        for (int i = 0; i < 5; i++) {
            String data = written(i).recordData();
            assertEquals(" ".repeat(fillers[i][1]), data.substring(fillers[i][0]), "record " + i);
        }
    }

    @Test
    void emptyInputsProduceNoRecordsAndZeroCounts() {
        cust = new InMemoryFiles.Reader(List.of());
        acct = new InMemoryFiles.Reader(List.of());
        xref = new InMemoryFiles.Reader(List.of());
        tran = new InMemoryFiles.Reader(List.of());
        card = new InMemoryFiles.Reader(List.of());
        assertEquals(new ExportStatistics(0, 0, 0, 0, 0, 0), program().run());
        assertTrue(exp.written.isEmpty());
        assertTrue(sysout.contains("CBEXPORT: Total Records Exported: 000000000"));
    }

    @Test
    void sequenceNumberRunsAcrossRecordTypes() {
        cust = new InMemoryFiles.Reader(List.of(CUSTOMER.toBytes(), CUSTOMER.toBytes()));
        tran = new InMemoryFiles.Reader(List.of(TRAN.toBytes(), TRAN.toBytes(), TRAN.toBytes()));
        ExportStatistics s = program().run();
        assertEquals(new ExportStatistics(2, 1, 1, 3, 1, 8), s);
        for (int i = 0; i < 8; i++) {
            assertEquals(i + 1, written(i).sequenceNum());
        }
        assertEquals('D', written(7).recType());
    }

    @Test
    void counterAddIsTruncatedToNineDigits() {
        assertEquals(1, Cbexport.addOne(0));
        assertEquals(0, Cbexport.addOne(999_999_999L));
    }

    @ParameterizedTest
    @CsvSource({
        "0, 'ERROR: Cannot open CUSTOMER-INPUT, Status: 35'",
        "1, 'ERROR: Cannot open ACCOUNT-INPUT, Status: 35'",
        "2, 'ERROR: Cannot open XREF-INPUT, Status: 35'",
        "3, 'ERROR: Cannot open TRANSACTION-INPUT, Status: 35'",
        "4, 'ERROR: Cannot open CARD-INPUT, Status: 35'",
        "5, 'ERROR: Cannot open EXPORT-OUTPUT, Status: 35'"
    })
    void openFailureAbendsWithExactMessage(int file, String message) {
        switch (file) {
            case 0 -> cust.failOpen("35");
            case 1 -> acct.failOpen("35");
            case 2 -> xref.failOpen("35");
            case 3 -> tran.failOpen("35");
            case 4 -> card.failOpen("35");
            default -> exp.failOpen("35");
        }
        CbexportAbendException e = assertThrows(CbexportAbendException.class, () -> program().run());
        assertEquals(message, e.getMessage());
        assertEquals("35", e.fileStatus());
        assertEquals(List.of("CBEXPORT: Starting Customer Data Export", message, "CBEXPORT: ABENDING PROGRAM"),
                sysout);
        assertTrue(exp.written.isEmpty());
        if (file < 5) {
            assertFalse(exp.opened, "later files are not opened after an abend");
        }
        assertFalse(cust.closed, "6000-FINALIZE does not run after CEE3ABD");
    }

    @ParameterizedTest
    @CsvSource({
        "0, 'ERROR: Reading CUSTOMER-INPUT, Status: 30', 0",
        "1, 'ERROR: Reading ACCOUNT-INPUT, Status: 30', 1",
        "2, 'ERROR: Reading XREF-INPUT, Status: 30', 2",
        "3, 'ERROR: Reading TRANSACTION-INPUT, Status: 30', 3",
        "4, 'ERROR: Reading CARD-INPUT, Status: 30', 4"
    })
    void readFailureAbendsWithExactMessage(int file, String message, int recordsBefore) {
        InMemoryFiles.Reader r = switch (file) {
            case 0 -> cust;
            case 1 -> acct;
            case 2 -> xref;
            case 3 -> tran;
            default -> card;
        };
        r.failRead(0, "30");
        CbexportAbendException e = assertThrows(CbexportAbendException.class, () -> program().run());
        assertEquals(message, e.getMessage());
        assertEquals(recordsBefore, exp.written.size());
        assertEquals(message, sysout.get(sysout.size() - 2));
        assertEquals("CBEXPORT: ABENDING PROGRAM", sysout.get(sysout.size() - 1));
    }

    @Test
    void readFailureAfterFirstRecordStillExportsEarlierRecords() {
        cust = new InMemoryFiles.Reader(List.of(CUSTOMER.toBytes(), CUSTOMER.toBytes())).failRead(1, "04");
        CbexportAbendException e = assertThrows(CbexportAbendException.class, () -> program().run());
        assertEquals("ERROR: Reading CUSTOMER-INPUT, Status: 04", e.getMessage());
        assertEquals(1, exp.written.size());
    }

    @Test
    void writeFailureAbendsWithExactMessage() {
        exp.failWrite(2, "21");
        CbexportAbendException e = assertThrows(CbexportAbendException.class, () -> program().run());
        assertEquals("ERROR: Writing export record, Status: 21", e.getMessage());
        assertEquals(2, exp.written.size());
        assertEquals(List.of("ERROR: Writing export record, Status: 21", "CBEXPORT: ABENDING PROGRAM"),
                sysout.subList(sysout.size() - 2, sysout.size()));
        assertFalse(sysout.contains("CBEXPORT: Cross-references exported: 000000000"));
    }
}
