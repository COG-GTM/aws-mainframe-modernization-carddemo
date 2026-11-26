package com.carddemo.programs.cbact02c;

import com.carddemo.programs.cbact02c.io.CardFile.ReadResult;
import com.carddemo.programs.cbact02c.io.InMemoryCardFile;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbact02cTest {

    private static final String START = "START OF EXECUTION OF PROGRAM CBACT02C";
    private static final String END = "END OF EXECUTION OF PROGRAM CBACT02C";

    private static final String REC_A = TestRecords.card("1000000000000001", 1, 123, "TEST HOLDER A", "2030-01-31", "Y");
    private static final String REC_B = TestRecords.card("2000000000000002", 22, 7, "TEST HOLDER B", "2031-12-01", "N");

    private final List<String> sysout = new ArrayList<>();

    @Test
    void mainlineDisplaysEveryRecordBetweenStartAndEndBanners() {
        Cbact02c program = new Cbact02c(new InMemoryCardFile(List.of(REC_A, REC_B)), sysout::add);

        assertEquals(0, program.run());

        assertEquals(List.of(START, REC_A, REC_B, END), sysout);
        assertEquals(150, sysout.get(1).length());
    }

    @Test
    void mainlineDisplaysInRecordKeyOrderLikeKsdsSequentialRead() {
        Cbact02c program = new Cbact02c(new InMemoryCardFile(List.of(REC_B, REC_A)), sysout::add);

        program.run();

        assertEquals(List.of(START, REC_A, REC_B, END), sysout);
    }

    @Test
    void emptyFileDisplaysOnlyBanners() {
        Cbact02c program = new Cbact02c(new InMemoryCardFile(List.of()), sysout::add);

        assertEquals(0, program.run());

        assertEquals(List.of(START, END), sysout);
    }

    @Test
    void openSuccessSetsApplAok() {
        ScriptedCardFile file = new ScriptedCardFile("00", List.of(), "00");
        Cbact02c program = new Cbact02c(file, sysout::add);

        program.cardfileOpen();

        assertTrue(program.isApplAok());
        assertEquals(0, program.applResult());
        assertTrue(sysout.isEmpty());
    }

    @Test
    void openFailureDisplaysErrorStatusAndAbends() {
        ScriptedCardFile file = new ScriptedCardFile("35", List.of(), "00");
        Cbact02c program = new Cbact02c(file, sysout::add);

        AbendException abend = assertThrows(AbendException.class, program::run);

        assertEquals(999, abend.abendCode());
        assertEquals(0, abend.timing());
        assertEquals(List.of(START, "ERROR OPENING CARDFILE", "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM"), sysout);
        assertEquals(12, program.applResult());
        assertEquals(0, file.readCalls);
        assertEquals(0, file.closeCalls);
    }

    @Test
    void getNextStatus00MovesRecordIntoCardRecord() {
        ScriptedCardFile file = new ScriptedCardFile("00", List.of(new ReadResult("00", REC_B)), "00");
        Cbact02c program = new Cbact02c(file, sysout::add);

        program.cardfileGetNext();

        assertTrue(program.isApplAok());
        assertFalse(program.endOfFile());
        assertEquals("2000000000000002", program.currentCardRecord().cardNum());
        assertEquals(22L, program.currentCardRecord().cardAcctId());
        assertEquals(7, program.currentCardRecord().cardCvvCd());
    }

    @Test
    void getNextStatus10SetsApplEofAndEndOfFile() {
        ScriptedCardFile file = new ScriptedCardFile("00", List.of(new ReadResult("10", null)), "00");
        Cbact02c program = new Cbact02c(file, sysout::add);

        program.cardfileGetNext();

        assertTrue(program.isApplEof());
        assertEquals(16, program.applResult());
        assertTrue(program.endOfFile());
        assertTrue(sysout.isEmpty());
    }

    @Test
    void endOfFileKeepsPreviousCardRecord() {
        ScriptedCardFile file = new ScriptedCardFile("00",
                List.of(new ReadResult("00", REC_A), new ReadResult("10", null)), "00");
        Cbact02c program = new Cbact02c(file, sysout::add);

        program.cardfileGetNext();
        program.cardfileGetNext();

        assertEquals(REC_A, program.currentCardRecord().toFixedWidth());
    }

    @ParameterizedTest
    @CsvSource({
        "30, FILE STATUS IS: NNNN0030",
        "04, FILE STATUS IS: NNNN0004",
        "46, FILE STATUS IS: NNNN0046",
        "92, FILE STATUS IS: NNNN9050"
    })
    void readErrorDisplaysErrorStatusAndAbends(String status, String expectedStatusLine) {
        ScriptedCardFile file = new ScriptedCardFile("00",
                List.of(new ReadResult("00", REC_A), new ReadResult(status, REC_B)), "00");
        Cbact02c program = new Cbact02c(file, sysout::add);

        AbendException abend = assertThrows(AbendException.class, program::run);

        assertEquals(999, abend.abendCode());
        assertEquals(List.of(START, REC_A, "ERROR READING CARDFILE", expectedStatusLine, "ABENDING PROGRAM"), sysout);
        assertEquals(12, program.applResult());
        assertEquals(0, file.closeCalls);
    }

    @Test
    void wrongLengthRecordFromKsdsAbendsWithStatus04() {
        Cbact02c program = new Cbact02c(new InMemoryCardFile(List.of(REC_A.substring(0, 149))), sysout::add);

        assertThrows(AbendException.class, program::run);

        assertEquals(List.of(START, "ERROR READING CARDFILE", "FILE STATUS IS: NNNN0004", "ABENDING PROGRAM"), sysout);
    }

    @Test
    void closeSuccessResetsApplResultToZero() {
        ScriptedCardFile file = new ScriptedCardFile("00", List.of(), "00");
        Cbact02c program = new Cbact02c(file, sysout::add);

        program.cardfileClose();

        assertEquals(0, program.applResult());
        assertTrue(sysout.isEmpty());
    }

    @Test
    void closeFailureDisplaysErrorStatusAndAbendsAfterRecords() {
        ScriptedCardFile file = new ScriptedCardFile("00", List.of(new ReadResult("00", REC_A)), "42");
        Cbact02c program = new Cbact02c(file, sysout::add);

        AbendException abend = assertThrows(AbendException.class, program::run);

        assertEquals(999, abend.abendCode());
        assertEquals(List.of(START, REC_A, "ERROR CLOSING CARDFILE", "FILE STATUS IS: NNNN0042", "ABENDING PROGRAM"), sysout);
        assertEquals(1, file.closeCalls);
    }

    @Test
    void abendProgramDisplaysMessageAndThrowsCode999() {
        Cbact02c program = new Cbact02c(new InMemoryCardFile(List.of()), sysout::add);

        AbendException abend = assertThrows(AbendException.class, program::abendProgram);

        assertEquals(999, abend.abendCode());
        assertEquals(List.of("ABENDING PROGRAM"), sysout);
    }

    @ParameterizedTest
    @CsvSource({
        "00, 0000",
        "35, 0035",
        "10, 0010",
        "97, 9055",
        "9A, 9065",
        "0A, 0065",
        "'3 ', 3032"
    })
    void displayIoStatusFormatsIoStatus04(String ioStatus, String expected) {
        assertEquals(expected, Cbact02c.formatIoStatus04(ioStatus));
    }

    @Test
    void displayIoStatusUsesBinaryValueOfSecondByte() {
        assertEquals("9021", Cbact02c.formatIoStatus04("9" + (char) 0x15));
        assertEquals("9255", Cbact02c.formatIoStatus04("9" + (char) 0xFF));
        assertEquals("9000", Cbact02c.formatIoStatus04("9" + (char) 0x00));
    }

    @Test
    void displayIoStatusWritesFileStatusLine() {
        ScriptedCardFile file = new ScriptedCardFile("00", List.of(new ReadResult("9" + (char) 0x15, null)), "00");
        Cbact02c program = new Cbact02c(file, sysout::add);

        assertThrows(AbendException.class, program::cardfileGetNext);

        assertEquals(List.of("ERROR READING CARDFILE", "FILE STATUS IS: NNNN9021", "ABENDING PROGRAM"), sysout);
    }
}
