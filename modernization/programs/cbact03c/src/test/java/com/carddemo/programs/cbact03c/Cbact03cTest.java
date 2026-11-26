package com.carddemo.programs.cbact03c;

import org.junit.jupiter.api.Test;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class Cbact03cTest {

    private static final String REC1 = "050002445376574000000005000000000050              ";
    private static final String REC2 = "068358619817151600000002700000000027              ";
    private static final String START = "START OF EXECUTION OF PROGRAM CBACT03C";
    private static final String END = "END OF EXECUTION OF PROGRAM CBACT03C";

    private final List<String> out = new ArrayList<>();

    @Test
    void mainLineDisplaysEachRecordTwiceThenEnds() {
        InMemoryXrefFile file = InMemoryXrefFile.ofRecords(List.of(REC1, REC2));
        new Cbact03c(file, out::add).run();
        assertEquals(List.of(START, REC1, REC1, REC2, REC2, END), out);
        assertEquals(1, file.closeCount());
    }

    @Test
    void emptyFileOnlyDisplaysBanners() {
        new Cbact03c(InMemoryXrefFile.ofRecords(List.of()), out::add).run();
        assertEquals(List.of(START, END), out);
    }

    @Test
    void getNextOnEofSetsApplEofAndEndOfFile() {
        Cbact03c p = new Cbact03c(InMemoryXrefFile.ofRecords(List.of()), out::add);
        p.xrefFileOpen();
        p.xrefFileGetNext();
        assertEquals(16, p.applResult());
        assertEquals('Y', p.endOfFile());
        assertEquals(List.of(), out);
    }

    @Test
    void getNextOnSuccessSetsApplAokAndDisplaysRecord() {
        Cbact03c p = new Cbact03c(InMemoryXrefFile.ofRecords(List.of(REC1)), out::add);
        p.xrefFileOpen();
        p.xrefFileGetNext();
        assertEquals(0, p.applResult());
        assertEquals('N', p.endOfFile());
        assertEquals(List.of(REC1), out);
    }

    @Test
    void openFailureAbendsWithFileStatus() {
        InMemoryXrefFile file = InMemoryXrefFile.ofRecords(List.of(REC1)).withOpenStatus("35");
        AbendException e = assertThrows(AbendException.class, () -> new Cbact03c(file, out::add).run());
        assertEquals(999, e.abendCode());
        assertEquals(0, e.timing());
        assertEquals(List.of(START, "ERROR OPENING XREFFILE", "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM"), out);
        assertEquals(0, file.closeCount());
    }

    @Test
    void readErrorAbendsAfterDisplayingEarlierRecords() {
        InMemoryXrefFile file = new InMemoryXrefFile(List.of(
                XrefFile.ReadResult.of(REC1), XrefFile.ReadResult.status("30")));
        AbendException e = assertThrows(AbendException.class, () -> new Cbact03c(file, out::add).run());
        assertEquals(999, e.abendCode());
        assertEquals(List.of(START, REC1, REC1, "ERROR READING XREFFILE", "FILE STATUS IS: NNNN0030",
                "ABENDING PROGRAM"), out);
        assertEquals(0, file.closeCount());
    }

    @Test
    void readStatus04IsTreatedAsErrorNotSuccess() {
        InMemoryXrefFile file = new InMemoryXrefFile(List.of(XrefFile.ReadResult.status("04")));
        assertThrows(AbendException.class, () -> new Cbact03c(file, out::add).run());
        assertEquals(List.of(START, "ERROR READING XREFFILE", "FILE STATUS IS: NNNN0004", "ABENDING PROGRAM"), out);
    }

    @Test
    void readStatus9xUsesBinaryByteValueOfSecondByte() {
        InMemoryXrefFile file = new InMemoryXrefFile(List.of(XrefFile.ReadResult.status("9#")));
        assertThrows(AbendException.class, () -> new Cbact03c(file, out::add).run());
        assertEquals("FILE STATUS IS: NNNN9035", out.get(2));
    }

    @Test
    void closeFailureAbendsAfterAllRecords() {
        InMemoryXrefFile file = InMemoryXrefFile.ofRecords(List.of(REC1)).withCloseStatus("42");
        AbendException e = assertThrows(AbendException.class, () -> new Cbact03c(file, out::add).run());
        assertEquals(999, e.abendCode());
        assertEquals(List.of(START, REC1, REC1, "ERROR CLOSING XREFFILE", "FILE STATUS IS: NNNN0042",
                "ABENDING PROGRAM"), out);
    }

    @Test
    void closeSuccessLeavesApplResultZero() {
        Cbact03c p = new Cbact03c(InMemoryXrefFile.ofRecords(List.of()), out::add);
        p.xrefFileOpen();
        p.xrefFileClose();
        assertEquals(0, p.applResult());
    }

    @Test
    void displayIoStatusNumericStatusIsZeroPrefixed() {
        assertEquals("0000", Cbact03c.formatIoStatus04("00", StandardCharsets.ISO_8859_1));
        assertEquals("0023", Cbact03c.formatIoStatus04("23", StandardCharsets.ISO_8859_1));
        assertEquals("0039", Cbact03c.formatIoStatus04("39", StandardCharsets.ISO_8859_1));
    }

    @Test
    void displayIoStatusStat1NineUsesBinaryOfStat2() {
        // ASCII runtime: '1' = 0x31 = 49, 0x7F = 127, 0xFF = 255
        assertEquals("9049", Cbact03c.formatIoStatus04("91", StandardCharsets.ISO_8859_1));
        assertEquals("9127", Cbact03c.formatIoStatus04("9\u007F", StandardCharsets.ISO_8859_1));
        assertEquals("9255", Cbact03c.formatIoStatus04("9\u00FF", StandardCharsets.ISO_8859_1));
    }

    @Test
    void displayIoStatusNonNumericStatusKeepsStat1Character() {
        assertEquals("A049", Cbact03c.formatIoStatus04("A1", StandardCharsets.ISO_8859_1));
        assertEquals("0032", Cbact03c.formatIoStatus04("0 ", StandardCharsets.ISO_8859_1));
    }

    @Test
    void displayIoStatusEbcdicRuntimeUsesEbcdicByteValue() {
        // z/OS: '1' = X'F1' = 241
        assertEquals("9241", Cbact03c.formatIoStatus04("91", Charset.forName("IBM037")));
    }

    @Test
    void displayIoStatusMessageText() {
        InMemoryXrefFile file = InMemoryXrefFile.ofRecords(List.of()).withOpenStatus("91");
        assertThrows(AbendException.class, () -> new Cbact03c(file, out::add).run());
        assertEquals("FILE STATUS IS: NNNN9049", out.get(2));
    }

    @Test
    void abendParagraphDisplaysAndThrowsU999() {
        Cbact03c p = new Cbact03c(InMemoryXrefFile.ofRecords(List.of()), out::add);
        AbendException e = assertThrows(AbendException.class, p::abendProgram);
        assertEquals("ABEND U0999", e.getMessage());
        assertEquals(List.of("ABENDING PROGRAM"), out);
    }
}
