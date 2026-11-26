package com.carddemo.programs.cbtrn01c;

import com.carddemo.programs.cbtrn01c.copybook.AccountRecord;
import com.carddemo.programs.cbtrn01c.copybook.CardRecord;
import com.carddemo.programs.cbtrn01c.copybook.CardXrefRecord;
import com.carddemo.programs.cbtrn01c.copybook.CustomerRecord;
import com.carddemo.programs.cbtrn01c.copybook.DalytranRecord;
import com.carddemo.programs.cbtrn01c.copybook.TranRecord;
import com.carddemo.programs.cbtrn01c.io.InMemoryIndexedFile;
import com.carddemo.programs.cbtrn01c.io.InMemorySequentialFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbtrn01cTest {

    private static final String CARD_OK = "4859452612877065";
    private static final String CARD_NO_ACCT = "1111222233334444";
    private static final String CARD_UNKNOWN = "9999888877776666";

    private final List<String> out = new ArrayList<>();
    private InMemorySequentialFile<DalytranRecord> dalytran;
    private InMemoryIndexedFile<CustomerRecord> cust;
    private InMemoryIndexedFile<CardXrefRecord> xref;
    private InMemoryIndexedFile<CardRecord> card;
    private InMemoryIndexedFile<AccountRecord> acct;
    private InMemoryIndexedFile<TranRecord> tran;

    @BeforeEach
    void setUp() {
        dalytran = new InMemorySequentialFile<>(List.of());
        cust = InMemoryIndexedFile.empty();
        xref = new InMemoryIndexedFile<>(List.of(
                new CardXrefRecord(CARD_OK, 27, 1, ""),
                new CardXrefRecord(CARD_NO_ACCT, 5, 77, "")), CardXrefRecord::cardNum);
        card = InMemoryIndexedFile.empty();
        acct = new InMemoryIndexedFile<>(List.of(account(1)), AccountRecord::acctIdDisplay);
        tran = InMemoryIndexedFile.empty();
    }

    private static AccountRecord account(long id) {
        return new AccountRecord(id, "Y", new BigDecimal("194.00"), new BigDecimal("2020.00"),
                new BigDecimal("1020.00"), "2014-11-20", "2025-05-20", "2025-05-20", BigDecimal.ZERO,
                BigDecimal.ZERO, "A000000000", "", "");
    }

    private static DalytranRecord tran(String id, String cardNum) {
        return new DalytranRecord(id, "01", 1, "POS TERM", "Purchase", new BigDecimal("50.47"), 800000000L,
                "Merchant", "City", "72112", cardNum, "2022-06-10 19:27:53.000000", "", "");
    }

    private Cbtrn01c program() {
        return program(StandardCharsets.ISO_8859_1);
    }

    private Cbtrn01c program(Charset cs) {
        return new Cbtrn01c(dalytran, cust, xref, card, acct, tran, out::add, cs);
    }

    private static List<String> xrefFound(String cardNum, String acctId, String custId) {
        return List.of("SUCCESSFUL READ OF XREF", "CARD NUMBER: " + cardNum, "ACCOUNT ID : " + acctId,
                "CUSTOMER ID: " + custId);
    }

    @Test
    void mainParaVerifiesCardAndAccountThenReprocessesLastRecordAtEof() {
        DalytranRecord t = tran("T000000000000001", CARD_OK);
        dalytran = new InMemorySequentialFile<>(List.of(t));

        assertEquals(0, program().run());

        List<String> expected = new ArrayList<>();
        expected.add("START OF EXECUTION OF PROGRAM CBTRN01C");
        expected.add(t.toFixedWidth());
        expected.addAll(xrefFound(CARD_OK, "00000000001", "000000027"));
        expected.add("SUCCESSFUL READ OF ACCOUNT FILE");
        // EOF pass: the last record is looked up again (no DISPLAY of the record)
        expected.addAll(xrefFound(CARD_OK, "00000000001", "000000027"));
        expected.add("SUCCESSFUL READ OF ACCOUNT FILE");
        expected.add("END OF EXECUTION OF PROGRAM CBTRN01C");
        assertEquals(expected, out);
        assertEquals(List.of(CARD_OK, CARD_OK), xref.keysRead());
        assertEquals(List.of("00000000001", "00000000001"), acct.keysRead());
        assertTrue(dalytran.isClosed() && cust.isClosed() && xref.isClosed() && card.isClosed()
                && acct.isClosed() && tran.isClosed());
    }

    @Test
    void lookupXrefInvalidKeySkipsTransaction() {
        DalytranRecord t = tran("T000000000000002", CARD_UNKNOWN);
        dalytran = new InMemorySequentialFile<>(List.of(t));

        program().run();

        String skip = "CARD NUMBER 9999888877776666 COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-T000000000000002";
        assertEquals(List.of(
                "START OF EXECUTION OF PROGRAM CBTRN01C",
                t.toFixedWidth(),
                "INVALID CARD NUMBER FOR XREF", skip,
                "INVALID CARD NUMBER FOR XREF", skip,
                "END OF EXECUTION OF PROGRAM CBTRN01C"), out);
        assertTrue(acct.keysRead().isEmpty());
    }

    @Test
    void readAccountInvalidKeyReportsAccountNotFound() {
        dalytran = new InMemorySequentialFile<>(List.of(tran("T3", CARD_NO_ACCT)));

        program().run();

        List<String> pass = new ArrayList<>(xrefFound(CARD_NO_ACCT, "00000000077", "000000005"));
        pass.add("INVALID ACCOUNT NUMBER FOUND");
        pass.add("ACCOUNT 00000000077 NOT FOUND");
        assertEquals(pass, out.subList(2, 2 + pass.size()));
        assertEquals(pass, out.subList(2 + pass.size(), 2 + 2 * pass.size()));
    }

    @Test
    void invalidXrefKeepsPreviousXrefIdsButNewCardNumber() {
        dalytran = new InMemorySequentialFile<>(List.of(tran("T1", CARD_OK), tran("T2", CARD_UNKNOWN),
                tran("T3", CARD_OK)));

        program().run();

        long skips = out.stream().filter(l -> l.contains("COULD NOT BE VERIFIED")).count();
        long accountReads = out.stream().filter("SUCCESSFUL READ OF ACCOUNT FILE"::equals).count();
        assertEquals(1, skips);
        assertEquals(3, accountReads); // T1, T3, and T3 again at EOF
        assertEquals(List.of(CARD_OK, CARD_UNKNOWN, CARD_OK, CARD_OK), xref.keysRead());
    }

    @Test
    void emptyDailyFileLooksUpInitialSpacesRecordOnce() {
        program().run();

        String blankCard = " ".repeat(16);
        assertEquals(List.of(
                "START OF EXECUTION OF PROGRAM CBTRN01C",
                "INVALID CARD NUMBER FOR XREF",
                "CARD NUMBER " + blankCard + " COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-" + blankCard,
                "END OF EXECUTION OF PROGRAM CBTRN01C"), out);
        assertEquals(List.of(blankCard), xref.keysRead());
    }

    @ParameterizedTest(name = "{0}")
    @CsvSource({
            "DALYTRAN, ERROR OPENING DAILY TRANSACTION FILE",
            "CUSTFILE, ERROR OPENING CUSTOMER FILE",
            "XREFFILE, ERROR OPENING CROSS REF FILE",
            "CARDFILE, ERROR OPENING CARD FILE",
            "ACCTFILE, ERROR OPENING ACCOUNT FILE",
            "TRANFILE, ERROR OPENING TRANSACTION FILE"
    })
    void openFailureAbendsWith999(String dd, String message) {
        switch (dd) {
            case "DALYTRAN" -> dalytran.withOpenStatus("35");
            case "CUSTFILE" -> cust.withOpenStatus("35");
            case "XREFFILE" -> xref.withOpenStatus("35");
            case "CARDFILE" -> card.withOpenStatus("35");
            case "ACCTFILE" -> acct.withOpenStatus("35");
            default -> tran.withOpenStatus("35");
        }

        AbendException e = assertThrows(AbendException.class, () -> program().run());

        assertEquals(999, e.abendCode());
        assertEquals(0, e.timing());
        assertEquals(List.of(message, "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM"),
                out.subList(out.size() - 3, out.size()));
        assertFalse(out.contains("END OF EXECUTION OF PROGRAM CBTRN01C"));
    }

    @Test
    void openFailureStopsBeforeLaterOpens() {
        xref.withOpenStatus("93");
        assertThrows(AbendException.class, () -> program().run());
        assertEquals(List.of("START OF EXECUTION OF PROGRAM CBTRN01C", "ERROR OPENING CROSS REF FILE",
                "FILE STATUS IS: NNNN9051", "ABENDING PROGRAM"), out);
    }

    @Test
    void dalytranReadErrorAbends() {
        dalytran = new InMemorySequentialFile<>(List.of(tran("T1", CARD_OK))).failReadAt(1, "30");

        assertThrows(AbendException.class, () -> program().run());

        assertEquals(List.of("ERROR READING DAILY TRANSACTION FILE", "FILE STATUS IS: NNNN0030",
                "ABENDING PROGRAM"), out.subList(out.size() - 3, out.size()));
        assertFalse(dalytran.isClosed());
    }

    @Test
    void dalytranNonZeroSuccessStatusIsTreatedAsError() {
        dalytran = new InMemorySequentialFile<DalytranRecord>(List.of()).failReadAt(0, "04");
        assertThrows(AbendException.class, () -> program().run());
        assertTrue(out.contains("FILE STATUS IS: NNNN0004"));
    }

    @ParameterizedTest(name = "{0}")
    @CsvSource({
            "CUSTFILE, ERROR CLOSING CUSTOMER FILE, 0042",
            "XREFFILE, ERROR CLOSING CROSS REF FILE, 0042",
            "CARDFILE, ERROR CLOSING CARD FILE, 0042",
            "ACCTFILE, ERROR CLOSING ACCOUNT FILE, 0042",
            "TRANFILE, ERROR CLOSING TRANSACTION FILE, 0042"
    })
    void closeFailureAbends(String dd, String message, String status) {
        switch (dd) {
            case "CUSTFILE" -> cust.withCloseStatus("42");
            case "XREFFILE" -> xref.withCloseStatus("42");
            case "CARDFILE" -> card.withCloseStatus("42");
            case "ACCTFILE" -> acct.withCloseStatus("42");
            default -> tran.withCloseStatus("42");
        }
        assertThrows(AbendException.class, () -> program().run());
        assertEquals(List.of(message, "FILE STATUS IS: NNNN" + status, "ABENDING PROGRAM"),
                out.subList(out.size() - 3, out.size()));
    }

    @Test
    void dalytranCloseFailureReportsCustomerFileAndCustfileStatusAsInCobol() {
        dalytran.withCloseStatus("42");
        assertThrows(AbendException.class, () -> program().run());
        assertEquals(List.of("ERROR CLOSING CUSTOMER FILE", "FILE STATUS IS: NNNN0000", "ABENDING PROGRAM"),
                out.subList(out.size() - 3, out.size()));
        assertFalse(cust.isClosed());
    }

    @Test
    void xrefReadErrorOtherThanInvalidKeyIsUnhandled() {
        dalytran = new InMemorySequentialFile<>(List.of(tran("T1", CARD_OK)));
        xref.withReadStatus("30");
        CobolRuntimeException e = assertThrows(CobolRuntimeException.class, () -> program().run());
        assertEquals("XREFFILE", e.fileName());
        assertEquals("30", e.fileStatus());
    }

    @Test
    void accountReadErrorOtherThanInvalidKeyIsUnhandled() {
        dalytran = new InMemorySequentialFile<>(List.of(tran("T1", CARD_OK)));
        acct.withReadStatus("92");
        CobolRuntimeException e = assertThrows(CobolRuntimeException.class, () -> program().run());
        assertEquals("ACCTFILE", e.fileName());
    }

    @Test
    void ebcdicStatusCharsetReproducesZosBinaryValue() {
        xref.withOpenStatus("9A");
        assertThrows(AbendException.class, () -> program(Charset.forName("IBM037")).run());
        assertTrue(out.contains("FILE STATUS IS: NNNN9193"));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "00|0000",
            "35|0035",
            "23|0023",
            "9A|9065",
            "97|9055",
            "X1|X049"
    })
    void displayIoStatusFormatsNumericAndBinaryStatuses(String status, String expected) {
        assertEquals(expected, Cbtrn01c.formatIoStatus(status, StandardCharsets.ISO_8859_1));
    }

    @Test
    void applResultConditionNames() {
        ApplResult r = new ApplResult();
        r.set(0);
        assertTrue(r.isAok());
        r.set(16);
        assertTrue(r.isEof());
        r.set(12);
        assertFalse(r.isAok() || r.isEof());
        assertEquals(12, r.value());
    }

    @Test
    void instanceRunsOnlyOnce() {
        Cbtrn01c p = program();
        assertEquals(0, p.run());
        assertThrows(IllegalStateException.class, p::run);
    }

    @Test
    void ioStatusDigitsIgnoreDefaultLocale() {
        java.util.Locale saved = java.util.Locale.getDefault();
        try {
            java.util.Locale.setDefault(java.util.Locale.forLanguageTag("ar-EG-u-nu-arab"));
            assertEquals("9065", Cbtrn01c.formatIoStatus("9A", java.nio.charset.StandardCharsets.ISO_8859_1));
        } finally {
            java.util.Locale.setDefault(saved);
        }
    }
}
