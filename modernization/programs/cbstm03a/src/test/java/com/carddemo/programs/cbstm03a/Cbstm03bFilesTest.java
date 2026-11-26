package com.carddemo.programs.cbstm03a;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cbstm03a.copybook.M03bArea;
import java.io.IOException;
import java.util.List;
import org.junit.jupiter.api.Test;

class Cbstm03bFilesTest {

    private static String call(Cbstm03bCall files, String dd, M03bArea.Oper oper, M03bArea area) {
        area.dd(dd);
        area.oper(oper);
        area.rc("00");
        files.call(area);
        return area.rc();
    }

    @Test
    void sequentialReadReturnsKeyOrderThenEofThenReadAfterEof() {
        Cbstm03bFiles f = Cbstm03bFiles.inMemory(List.of(),
                List.of(TestData.xref("2222222222222222", 2, 2), TestData.xref("1111111111111111", 1, 1)),
                List.of(), List.of());
        M03bArea a = new M03bArea();
        assertEquals("47", call(f, "XREFFILE", M03bArea.Oper.READ, a));
        assertEquals("00", call(f, "XREFFILE", M03bArea.Oper.OPEN, a));
        assertEquals("41", call(f, "XREFFILE", M03bArea.Oper.OPEN, a));
        assertEquals("00", call(f, "XREFFILE", M03bArea.Oper.READ, a));
        assertTrue(a.fldt().startsWith("1111111111111111"));
        assertEquals("00", call(f, "XREFFILE", M03bArea.Oper.READ, a));
        assertTrue(a.fldt().startsWith("2222222222222222"));
        a.fldt("");
        assertEquals("10", call(f, "XREFFILE", M03bArea.Oper.READ, a));
        assertEquals(" ".repeat(1000), a.fldt());
        assertEquals("46", call(f, "XREFFILE", M03bArea.Oper.READ, a));
        assertEquals("00", call(f, "XREFFILE", M03bArea.Oper.CLOSE, a));
        assertEquals("42", call(f, "XREFFILE", M03bArea.Oper.CLOSE, a));
    }

    @Test
    void duplicateSequentialKeysKeepFirstRecordLikeAKsds() {
        Cbstm03bFiles f = Cbstm03bFiles.inMemory(List.of(),
                List.of(TestData.xref("1111111111111111", 1, 1), TestData.xref("1111111111111111", 2, 2)),
                List.of(), List.of());
        M03bArea a = new M03bArea();
        call(f, "XREFFILE", M03bArea.Oper.OPEN, a);
        assertEquals("00", call(f, "XREFFILE", M03bArea.Oper.READ, a));
        assertTrue(a.fldt().startsWith("1111111111111111000000001"));
        assertEquals("10", call(f, "XREFFILE", M03bArea.Oper.READ, a));
    }

    @Test
    void uncheckedReadFailureDuringOpenIsStatus35() {
        Cbstm03bFiles.Opener broken = () -> new com.carddemo.programs.cbstm03a.io.RecordReader() {
            @Override
            public java.util.Optional<String> read() {
                throw new java.io.UncheckedIOException(new IOException("Is a directory"));
            }

            @Override
            public void close() {
            }
        };
        Cbstm03bFiles f = new Cbstm03bFiles(broken, broken, broken, broken);
        M03bArea a = new M03bArea();
        assertEquals("35", call(f, "XREFFILE", M03bArea.Oper.OPEN, a));
        assertEquals("35", call(f, "ACCTFILE", M03bArea.Oper.OPEN, a));
    }

    @Test
    void keyedReadUsesKeyLengthAndReportsNotFound() {
        Cbstm03bFiles f = Cbstm03bFiles.inMemory(List.of(), List.of(), List.of(TestData.customer(7)),
                List.of(TestData.account(9, "1.00")));
        M03bArea a = new M03bArea();
        assertEquals("00", call(f, "CUSTFILE", M03bArea.Oper.OPEN, a));
        a.key("000000007");
        a.keyLn(9);
        assertEquals("00", call(f, "CUSTFILE", M03bArea.Oper.READ_K, a));
        assertTrue(a.fldt().startsWith("000000007Jane"));
        a.key("000000008");
        assertEquals("23", call(f, "CUSTFILE", M03bArea.Oper.READ_K, a));
        assertEquals("47", call(f, "ACCTFILE", M03bArea.Oper.READ_K, a));
        assertEquals("00", call(f, "ACCTFILE", M03bArea.Oper.OPEN, a));
        a.key("00000000009");
        a.keyLn(11);
        assertEquals("00", call(f, "ACCTFILE", M03bArea.Oper.READ_K, a));
        assertTrue(a.fldt().startsWith("00000000009Y"));
    }

    @Test
    void openFailureIsStatus35AndUnknownDdLeavesRcUntouched() {
        Cbstm03bFiles.Opener failing = () -> {
            throw new IOException("no such dataset");
        };
        Cbstm03bFiles f = new Cbstm03bFiles(failing, failing, failing, failing);
        M03bArea a = new M03bArea();
        assertEquals("35", call(f, "TRNXFILE", M03bArea.Oper.OPEN, a));
        assertEquals("35", call(f, "CUSTFILE", M03bArea.Oper.OPEN, a));
        a.dd("BOGUSDD");
        a.rc("XX");
        f.call(a);
        assertEquals("XX", a.rc());
    }
}
