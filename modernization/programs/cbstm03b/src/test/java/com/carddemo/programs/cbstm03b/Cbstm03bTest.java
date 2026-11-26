package com.carddemo.programs.cbstm03b;

import com.carddemo.programs.cbstm03b.copybook.FdAcctFileRec;
import com.carddemo.programs.cbstm03b.copybook.FdCustFileRec;
import com.carddemo.programs.cbstm03b.copybook.FdTrnxFileRec;
import com.carddemo.programs.cbstm03b.copybook.FdXrefFileRec;
import com.carddemo.programs.cbstm03b.copybook.LkM03bArea;
import com.carddemo.programs.cbstm03b.copybook.M03bOper;
import com.carddemo.programs.cbstm03b.io.InMemoryIndexedFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbstm03bTest {

    private static final String SENTINEL = "UNCHANGED";

    static String trnx(String card, String id, String data) {
        return new FdTrnxFileRec(card, id, data).toRecord();
    }

    static String xref(String card, String data) {
        return new FdXrefFileRec(card, data).toRecord();
    }

    static String cust(String id, String data) {
        return new FdCustFileRec(id, data).toRecord();
    }

    static String acct(String id, String data) {
        return new FdAcctFileRec(id, data).toRecord();
    }

    // Loaded out of key order on purpose: the KSDS returns ascending key order.
    static final String T_B1 = trnx("4000000000000002", "0000000000000001", "T-B1");
    static final String T_A2 = trnx("4000000000000001", "0000000000000002", "T-A2");
    static final String T_A1 = trnx("4000000000000001", "0000000000000001", "T-A1");
    static final String X_2 = xref("4000000000000002", "000000002000000000002");
    static final String X_1 = xref("4000000000000001", "000000001000000000001");
    static final String C_1 = cust("000000001", "CUSTOMER ONE");
    static final String C_2 = cust("000000002", "CUSTOMER TWO");
    static final String A_1 = acct("00000000001", "ACCOUNT ONE");
    static final String A_2 = acct("00000000002", "ACCOUNT TWO");

    InMemoryIndexedFile trnxFile;
    InMemoryIndexedFile xrefFile;
    InMemoryIndexedFile custFile;
    InMemoryIndexedFile acctFile;
    Cbstm03b pgm;

    @BeforeEach
    void setUp() {
        trnxFile = InMemoryIndexedFile.of("TRNXFILE", 350, 0, 32, List.of(T_B1, T_A2, T_A1));
        xrefFile = InMemoryIndexedFile.of("XREFFILE", 50, 0, 16, List.of(X_2, X_1));
        custFile = InMemoryIndexedFile.of("CUSTFILE", 500, 0, 9, List.of(C_1, C_2));
        acctFile = InMemoryIndexedFile.of("ACCTFILE", 300, 0, 11, List.of(A_2, A_1));
        pgm = new Cbstm03b(trnxFile, xrefFile, custFile, acctFile);
    }

    LkM03bArea area(String dd, M03bOper op) {
        return new LkM03bArea().dd(dd).set(op).rc("00").fldt(SENTINEL);
    }

    LkM03bArea call(String dd, M03bOper op) {
        LkM03bArea lk = area(dd, op);
        pgm.call(lk);
        return lk;
    }

    LkM03bArea readK(String dd, String key, int keyLn) {
        LkM03bArea lk = area(dd, M03bOper.READ_K).key(key).keyLn(keyLn);
        pgm.call(lk);
        return lk;
    }

    static String fldtOf(String record) {
        return record + " ".repeat(LkM03bArea.FLDT_LEN - record.length());
    }

    @Nested
    class Start0000 {

        @Test
        void unknownDdGoesBackWithoutTouchingTheArea() {
            LkM03bArea lk = area("STMTFILE", M03bOper.OPEN).rc("99");
            pgm.call(lk);
            assertEquals("99", lk.rc());
            assertEquals(fldtOf(SENTINEL), lk.fldt());
            assertTrue(!trnxFile.isOpen() && !xrefFile.isOpen() && !custFile.isOpen() && !acctFile.isOpen());
        }

        @Test
        void ddMatchIsExactAndCaseSensitive() {
            LkM03bArea lk = area("trnxfile", M03bOper.OPEN).rc("99");
            pgm.call(lk);
            assertEquals("99", lk.rc());
            assertTrue(!trnxFile.isOpen());
        }

        @Test
        void ddLongerThanEightCharactersIsTruncatedLikePicX8() {
            LkM03bArea lk = area("TRNXFILEX", M03bOper.OPEN);
            assertEquals("TRNXFILE", lk.dd());
            pgm.call(lk);
            assertEquals("00", lk.rc());
        }
    }

    @Nested
    class TrnxfileProc1000 {

        @Test
        void openReadsInKeyOrderThenEndOfFileThenClose() {
            assertEquals("00", call("TRNXFILE", M03bOper.OPEN).rc());

            LkM03bArea r1 = call("TRNXFILE", M03bOper.READ);
            assertEquals("00", r1.rc());
            assertEquals(fldtOf(T_A1), r1.fldt());
            assertEquals(fldtOf(T_A2), call("TRNXFILE", M03bOper.READ).fldt());
            assertEquals(fldtOf(T_B1), call("TRNXFILE", M03bOper.READ).fldt());

            LkM03bArea eof = call("TRNXFILE", M03bOper.READ);
            assertEquals("10", eof.rc());
            assertEquals(fldtOf(SENTINEL), eof.fldt(), "READ INTO is not performed at end of file");

            assertEquals("46", call("TRNXFILE", M03bOper.READ).rc());
            assertEquals("00", call("TRNXFILE", M03bOper.CLOSE).rc());
        }

        @Test
        void readBeforeOpenIsStatus47() {
            LkM03bArea lk = call("TRNXFILE", M03bOper.READ);
            assertEquals("47", lk.rc());
            assertEquals(fldtOf(SENTINEL), lk.fldt());
        }

        @Test
        void openTwiceIs41AndCloseWhenNotOpenIs42() {
            assertEquals("00", call("TRNXFILE", M03bOper.OPEN).rc());
            assertEquals("41", call("TRNXFILE", M03bOper.OPEN).rc());
            assertEquals("00", call("TRNXFILE", M03bOper.CLOSE).rc());
            assertEquals("42", call("TRNXFILE", M03bOper.CLOSE).rc());
        }

        @Test
        void reopenRestartsAtFirstKey() {
            call("TRNXFILE", M03bOper.OPEN);
            call("TRNXFILE", M03bOper.READ);
            call("TRNXFILE", M03bOper.CLOSE);
            call("TRNXFILE", M03bOper.OPEN);
            assertEquals(fldtOf(T_A1), call("TRNXFILE", M03bOper.READ).fldt());
        }

        @ParameterizedTest
        @ValueSource(chars = {'K', 'W', 'Z', 'X', ' '})
        void unsupportedOperationReturnsLastStatusWithoutIo(char op) {
            call("TRNXFILE", M03bOper.OPEN);
            call("TRNXFILE", M03bOper.READ);
            call("TRNXFILE", M03bOper.READ);
            call("TRNXFILE", M03bOper.READ);
            assertEquals("10", call("TRNXFILE", M03bOper.READ).rc());

            LkM03bArea lk = area("TRNXFILE", M03bOper.OPEN).oper(op).rc("00");
            pgm.call(lk);
            assertEquals("10", lk.rc(), "1900-EXIT moves the unchanged TRNXFILE-STATUS");
            assertEquals(fldtOf(SENTINEL), lk.fldt());
            assertTrue(trnxFile.isOpen());
        }

        @Test
        void unsupportedOperationBeforeAnyIoReturnsInitialSpacesStatus() {
            assertEquals("  ", call("TRNXFILE", M03bOper.WRITE).rc());
        }
    }

    @Nested
    class XreffileProc2000 {

        @Test
        void openReadsInKeyOrderThenEndOfFileThenClose() {
            assertEquals("00", call("XREFFILE", M03bOper.OPEN).rc());
            assertEquals(fldtOf(X_1), call("XREFFILE", M03bOper.READ).fldt());
            assertEquals(fldtOf(X_2), call("XREFFILE", M03bOper.READ).fldt());
            LkM03bArea eof = call("XREFFILE", M03bOper.READ);
            assertEquals("10", eof.rc());
            assertEquals(fldtOf(SENTINEL), eof.fldt());
            assertEquals("00", call("XREFFILE", M03bOper.CLOSE).rc());
        }

        @Test
        void errorsAndUnsupportedOperations() {
            assertEquals("47", call("XREFFILE", M03bOper.READ).rc());
            assertEquals("42", call("XREFFILE", M03bOper.CLOSE).rc());
            assertEquals("42", readK("XREFFILE", "4000000000000001", 16).rc(), "READ-K not handled for XREFFILE");
            assertEquals("42", call("XREFFILE", M03bOper.REWRITE).rc());
        }

        @Test
        void statusesAreKeptPerFile() {
            call("XREFFILE", M03bOper.OPEN);
            call("TRNXFILE", M03bOper.READ);
            assertEquals("00", call("XREFFILE", M03bOper.WRITE).rc());
            assertEquals("47", call("TRNXFILE", M03bOper.WRITE).rc());
        }
    }

    @Nested
    class CustfileProc3000 {

        @BeforeEach
        void open() {
            assertEquals("00", call("CUSTFILE", M03bOper.OPEN).rc());
        }

        @Test
        void readByKeyFound() {
            LkM03bArea lk = readK("CUSTFILE", "000000002", 9);
            assertEquals("00", lk.rc());
            assertEquals(fldtOf(C_2), lk.fldt());
        }

        @Test
        void readByKeyNotFoundIs23AndLeavesFldt() {
            LkM03bArea lk = readK("CUSTFILE", "000000009", 9);
            assertEquals("23", lk.rc());
            assertEquals(fldtOf(SENTINEL), lk.fldt());
        }

        @Test
        void keyIsReferenceModifiedThenMovedToPicX9() {
            assertEquals("00", readK("CUSTFILE", "000000001XYZ", 9).rc());
            assertEquals("00", readK("CUSTFILE", "000000001XYZ", 12).rc(), "PIC X(09) truncates on the right");
            assertEquals("23", readK("CUSTFILE", "000000001", 8).rc(), "'00000000 ' is space padded, not found");
        }

        @Test
        void sequentialReadIsNotHandledForCustfile() {
            LkM03bArea lk = call("CUSTFILE", M03bOper.READ);
            assertEquals("00", lk.rc());
            assertEquals(fldtOf(SENTINEL), lk.fldt());
        }

        @Test
        void closeThenReadIs47() {
            assertEquals("00", call("CUSTFILE", M03bOper.CLOSE).rc());
            assertEquals("47", readK("CUSTFILE", "000000001", 9).rc());
        }

        @ParameterizedTest
        @ValueSource(ints = {0, -1, 26, 9999})
        void outOfRangeKeyLengthAbends(int keyLn) {
            Cbstm03bAbend ex = assertThrows(Cbstm03bAbend.class, () -> readK("CUSTFILE", "000000001", keyLn));
            assertTrue(ex.getMessage().startsWith("EC-BOUND-REF-MOD"));
        }
    }

    @Nested
    class AcctfileProc4000 {

        @BeforeEach
        void open() {
            assertEquals("00", call("ACCTFILE", M03bOper.OPEN).rc());
        }

        @Test
        void readByKeyFound() {
            LkM03bArea lk = readK("ACCTFILE", "00000000002", 11);
            assertEquals("00", lk.rc());
            assertEquals(fldtOf(A_2), lk.fldt());
        }

        @Test
        void readByKeyNotFoundIs23AndLeavesFldt() {
            LkM03bArea lk = readK("ACCTFILE", "00000000003", 11);
            assertEquals("23", lk.rc());
            assertEquals(fldtOf(SENTINEL), lk.fldt());
        }

        @Test
        void shortKeyIsRightJustifiedAndZeroFilledIntoPic9_11() {
            LkM03bArea lk = readK("ACCTFILE", "1", 1);
            assertEquals("00", lk.rc());
            assertEquals(fldtOf(A_1), lk.fldt());
        }

        @Test
        void longKeyIsTruncatedOnTheLeftIntoPic9_11() {
            LkM03bArea lk = readK("ACCTFILE", "9900000000002", 13);
            assertEquals("00", lk.rc());
            assertEquals(fldtOf(A_2), lk.fldt());
        }

        @Test
        void sequentialReadAndWriteAreNotHandled() {
            readK("ACCTFILE", "00000000003", 11);
            assertEquals("23", call("ACCTFILE", M03bOper.READ).rc());
            assertEquals("23", call("ACCTFILE", M03bOper.WRITE).rc());
        }

        @Test
        void closeIsHandled() {
            assertEquals("00", call("ACCTFILE", M03bOper.CLOSE).rc());
            assertEquals("42", call("ACCTFILE", M03bOper.CLOSE).rc());
        }

        @Test
        void zeroKeyLengthAbends() {
            assertThrows(Cbstm03bAbend.class, () -> readK("ACCTFILE", "00000000001", 0));
        }
    }

    @Test
    void unallocatedDatasetOpensWithStatus35() throws Exception {
        Cbstm03b missing = Cbstm03b.fromDatasets(null, null, null, java.nio.file.Path.of("does-not-exist.txt"));
        for (String dd : List.of("TRNXFILE", "XREFFILE", "CUSTFILE", "ACCTFILE")) {
            LkM03bArea lk = area(dd, M03bOper.OPEN);
            missing.call(lk);
            assertEquals("35", lk.rc(), dd);
        }
        LkM03bArea read = area("TRNXFILE", M03bOper.READ);
        missing.call(read);
        assertEquals("47", read.rc());
    }
}
