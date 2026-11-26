package com.carddemo.programs.cobtupdt;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CobtupdtTest {

    private static final String PAD50 = " ".repeat(50);

    private final List<String> sysout = new ArrayList<>();
    private final InMemoryTransactionTypeGateway table = new InMemoryTransactionTypeGateway();

    private Cobtupdt program(String... lines) {
        return new Cobtupdt(FixedWidthTrRecordReader.ofLines(List.of(lines)), table, sysout::add);
    }

    private int run(String... lines) {
        return program(lines).run();
    }

    private List<String> trimmed() {
        return sysout.stream().map(s -> s.startsWith("PROCESSING") ? s : s.stripTrailing()).toList();
    }

    private static String desc(String s) {
        return (s + PAD50).substring(0, 50);
    }

    private static String processing(String rec) {
        return "PROCESSING   " + (rec + " ".repeat(53)).substring(0, 53);
    }

    @Nested
    class OpenFiles0001 {

        @Test
        void openOkThenEmptyFileEndsWithRcZero() {
            int rc = run();
            assertEquals(0, rc);
            assertEquals(List.of("OPEN FILE OK"), sysout);
        }

        @Test
        void openFailureDisplaysNotOkAndStopsOnTheFailedRead() {
            TrRecordReader missing = FixedWidthTrRecordReader.of(java.nio.file.Path.of("does/not/exist.txt"));
            Cobtupdt p = new Cobtupdt(missing, table, sysout::add);
            CobolFileStatusException e = assertThrows(CobolFileStatusException.class, p::run);
            assertEquals("47", e.fileStatus());
            assertEquals("47", p.wsInfStatus());
            assertEquals(List.of("OPEN FILE NOT OK"), sysout);
        }
    }

    @Nested
    class ReadRecords1002 {

        @Test
        void displaysEachRecordPaddedTo53BytesAndNothingAtEnd() {
            run("*01 comment");
            assertEquals(List.of("OPEN FILE OK", processing("*01 comment"), "IGNORING COMMENTED LINE"), sysout);
            assertEquals(13 + 53, sysout.get(1).length());
        }

        @Test
        void recordsLongerThan53BytesAreTruncated() {
            String longRec = "A01" + "X".repeat(60);
            run(longRec);
            assertEquals("PROCESSING   " + longRec.substring(0, 53), sysout.get(1));
            assertEquals(Optional.of("X".repeat(50)), table.find("01"));
        }

        @Test
        void closesFileAtEnd() {
            Cobtupdt p = program("*");
            p.run();
            assertEquals("00", p.wsInfStatus());
        }
    }

    @Nested
    class TreatRecord1003 {

        @Test
        void addInsertsBlankPaddedDescription() {
            int rc = run("A08Fee");
            assertEquals(0, rc);
            assertEquals(List.of("OPEN FILE OK", processing("A08Fee"), "ADDING RECORD",
                    "RECORD INSERTED SUCCESSFULLY"), sysout);
            assertEquals(Optional.of(desc("Fee")), table.find("08"));
        }

        @Test
        void updateReplacesDescription() {
            table.seed("02", "Payment");
            int rc = run("U02Payment received");
            assertEquals(0, rc);
            assertEquals(List.of("OPEN FILE OK", processing("U02Payment received"), "UPDATING RECORD",
                    "RECORD UPDATED SUCCESSFULLY"), sysout);
            assertEquals(Optional.of(desc("Payment received")), table.find("02"));
        }

        @Test
        void deleteRemovesRow() {
            table.seed("06", "Reversal");
            int rc = run("D06");
            assertEquals(0, rc);
            assertEquals(List.of("OPEN FILE OK", processing("D06"), "DELETING RECORD",
                    "RECORD DELETED SUCCESSFULLY"), sysout);
            assertTrue(table.find("06").isEmpty());
        }

        @Test
        void commentLineIsIgnored() {
            int rc = run("*01Purchase");
            assertEquals(0, rc);
            assertEquals("IGNORING COMMENTED LINE", sysout.get(2));
            assertTrue(table.rows().isEmpty());
        }

        @ParameterizedTest
        @CsvSource({"X01Foo", "a01Foo", "u01Foo", "' 01Foo'", "101Foo"})
        void invalidTypeDisplaysErrorSetsRc4AndContinues(String rec) {
            int rc = run(rec, "A09Next");
            assertEquals(4, rc);
            assertEquals(List.of("OPEN FILE OK", processing(rec), "ERROR: TYPE NOT VALID",
                    processing("A09Next"), "ADDING RECORD", "RECORD INSERTED SUCCESSFULLY"), trimmed());
            assertEquals(Optional.of(desc("Next")), table.find("09"));
        }

        @Test
        void blankLineIsAnInvalidType() {
            int rc = run("");
            assertEquals(4, rc);
            assertEquals("ERROR: TYPE NOT VALID", trimmed().get(2));
        }

        @Test
        void abendMessageIsDisplayedAsFull80ByteField() {
            run("Z");
            assertEquals(80, sysout.get(2).length());
            assertEquals("ERROR: TYPE NOT VALID" + " ".repeat(59), sysout.get(2));
        }

        @Test
        void transactionTypeIsNotValidatedAsNumeric() {
            int rc = run("AXYNot numeric");
            assertEquals(0, rc);
            assertEquals(Optional.of(desc("Not numeric")), table.find("XY"));
        }
    }

    @Nested
    class InsertDb10031 {

        @Test
        void duplicateKeyReportsSqlcodeMinus803() {
            table.seed("01", "Purchase");
            int rc = run("A01Purchase again");
            assertEquals(4, rc);
            assertEquals(List.of("OPEN FILE OK", processing("A01Purchase again"), "ADDING RECORD",
                    "Error accessing: TRANSACTION_TYPE table. SQLCODE: -803"), trimmed());
            assertEquals(Optional.of("Purchase"), table.find("01"));
        }

        @Test
        void positiveSqlcodeOnInsertIsSilentlyIgnored() {
            table.failNextWith(100);
            int rc = run("A01Purchase");
            assertEquals(0, rc);
            assertEquals(List.of("OPEN FILE OK", processing("A01Purchase"), "ADDING RECORD"), sysout);
        }

        @Test
        void otherNegativeSqlcode() {
            table.failNextWith(-911);
            int rc = run("A01Purchase");
            assertEquals(4, rc);
            assertEquals("Error accessing: TRANSACTION_TYPE table. SQLCODE: -911", trimmed().get(3));
        }
    }

    @Nested
    class UpdateDb10032 {

        @Test
        void noRowReportsNoRecordsFound() {
            int rc = run("U05Refund");
            assertEquals(4, rc);
            assertEquals(List.of("OPEN FILE OK", processing("U05Refund"), "UPDATING RECORD",
                    "No records found."), trimmed());
        }

        @Test
        void negativeSqlcode() {
            table.failNextWith(-204);
            int rc = run("U05Refund");
            assertEquals(4, rc);
            assertEquals("Error accessing: TRANSACTION_TYPE table. SQLCODE: -204", trimmed().get(3));
        }

        @Test
        void otherPositiveSqlcodeIsSilentlyIgnored() {
            table.failNextWith(437);
            int rc = run("U05Refund");
            assertEquals(0, rc);
            assertEquals(3, sysout.size());
        }
    }

    @Nested
    class DeleteDb10033 {

        @Test
        void noRowReportsNoRecordsFound() {
            int rc = run("D05");
            assertEquals(4, rc);
            assertEquals(List.of("OPEN FILE OK", processing("D05"), "DELETING RECORD",
                    "No records found."), trimmed());
        }

        @Test
        void negativeSqlcode() {
            table.failNextWith(-532);
            int rc = run("D05");
            assertEquals(4, rc);
            assertEquals("Error accessing: TRANSACTION_TYPE table. SQLCODE: -532", trimmed().get(3));
        }
    }

    @Nested
    class Abend9999 {

        @Test
        void returnCodeStaysFourAfterLaterSuccess() {
            int rc = run("D99", "A10New", "*");
            assertEquals(4, rc);
            assertEquals(Optional.of(desc("New")), table.find("10"));
        }

        @Test
        void returnMessageIsNeverClearedSoShorterMessagesKeepTheTailOfLongerOnes() {
            table.seed("01", "Purchase");
            int rc = run("A01Dup", "U77Missing", "Q");
            assertEquals(4, rc);
            List<String> out = trimmed();
            assertEquals("Error accessing: TRANSACTION_TYPE table. SQLCODE: -803", out.get(3));
            assertEquals("No records found.TRANSACTION_TYPE table. SQLCODE: -803", out.get(6));
            assertEquals("ERROR: TYPE NOT VALIDSACTION_TYPE table. SQLCODE: -803", out.get(8));
        }
    }

    @Nested
    class EditSqlcode {

        @ParameterizedTest
        @CsvSource(delimiter = '|', value = {
                "0|'    0'",
                "100|'  100'",
                "-1|'   -1'",
                "-803|' -803'",
                "-9999|'-9999'",
                "-30081|'  -81'",
                "12345|' 2345'"})
        void picMinusMinusMinusMinus9(int sqlcode, String expected) {
            assertEquals(expected, Cobtupdt.editSqlcode(sqlcode));
        }

        @Test
        void wsVarSqlcodeAndSqlcaReflectLastStatement() {
            Cobtupdt p = program("D42");
            p.run();
            assertEquals("  100", p.wsVarSqlcode());
            assertEquals(100, p.sqlca().sqlcode());
            assertTrue(p.sqlca().isNotFound());
        }
    }

    @Test
    void stringIntoTruncatesAt80Bytes() {
        Cobtupdt p = program();
        p.stringIntoReturnMsg("Y".repeat(90));
        assertEquals("Y".repeat(80), p.wsReturnMsg());
    }

    @Test
    void mixedBatchEndToEnd() {
        table.seed("01", "Purchase").seed("02", "Payment");
        int rc = run(
                "* MAINTAIN TRANSACTION TYPES",
                "A08Fee",
                "U01Purchase - card present",
                "D02",
                "D02");
        assertEquals(4, rc);
        assertEquals(List.of(
                "OPEN FILE OK",
                processing("* MAINTAIN TRANSACTION TYPES"), "IGNORING COMMENTED LINE",
                processing("A08Fee"), "ADDING RECORD", "RECORD INSERTED SUCCESSFULLY",
                processing("U01Purchase - card present"), "UPDATING RECORD", "RECORD UPDATED SUCCESSFULLY",
                processing("D02"), "DELETING RECORD", "RECORD DELETED SUCCESSFULLY",
                processing("D02"), "DELETING RECORD", "No records found."), trimmed());
        assertEquals(List.of("01", "08"), List.copyOf(table.rows().keySet()));
    }
}
