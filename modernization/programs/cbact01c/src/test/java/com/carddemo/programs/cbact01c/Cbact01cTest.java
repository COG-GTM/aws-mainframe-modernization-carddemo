package com.carddemo.programs.cbact01c;

import com.carddemo.programs.cbact01c.copybook.AccountRecord;
import com.carddemo.programs.cbact01c.gateway.CobdatftDateFormatter;
import com.carddemo.programs.cbact01c.io.InMemoryRecordReader;
import com.carddemo.programs.cbact01c.io.InMemoryRecordWriter;
import com.carddemo.programs.cbact01c.record.ArrArrayRec;
import com.carddemo.programs.cbact01c.record.OutAcctRec;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbact01cTest {
    private final List<String> sysout = new ArrayList<>();
    private InMemoryRecordReader acct;
    private final InMemoryRecordWriter out = new InMemoryRecordWriter();
    private final InMemoryRecordWriter arry = new InMemoryRecordWriter();
    private final InMemoryRecordWriter vbrc = new InMemoryRecordWriter();

    private static AccountRecord account(long id, String bal, String cycDebit) {
        return AccountRecord.of(id, "Y", new BigDecimal(bal), new BigDecimal("2020.00"), new BigDecimal("1020.00"),
                "2014-11-20", "2025-05-20", "2025-05-20", new BigDecimal("0.00"), new BigDecimal(cycDebit),
                "A000000000", "GROUP0001");
    }

    private Cbact01c program(AccountRecord... records) {
        List<byte[]> bytes = new ArrayList<>();
        for (AccountRecord r : records) {
            bytes.add(r.toBytes());
        }
        acct = new InMemoryRecordReader(bytes);
        return new Cbact01c(acct, out, arry, vbrc, new CobdatftDateFormatter(), sysout::add);
    }

    private static String text(byte[] b) {
        return new String(b, StandardCharsets.ISO_8859_1);
    }

    @Test
    void mainlineProcessesOneAccountAndDisplaysInCobolOrder() {
        AccountRecord rec = account(1, "194.00", "0");
        int rc = program(rec).run();

        assertEquals(0, rc);
        assertEquals(List.of(
                "START OF EXECUTION OF PROGRAM CBACT01C",
                "ACCT-ID                 :00000000001",
                "ACCT-ACTIVE-STATUS      :Y",
                "ACCT-CURR-BAL           :00000001940{",
                "ACCT-CREDIT-LIMIT       :00000020200{",
                "ACCT-CASH-CREDIT-LIMIT  :00000010200{",
                "ACCT-OPEN-DATE          :2014-11-20",
                "ACCT-EXPIRAION-DATE     :2025-05-20",
                "ACCT-REISSUE-DATE       :2025-05-20",
                "ACCT-CURR-CYC-CREDIT    :00000000000{",
                "ACCT-CURR-CYC-DEBIT     :00000000000{",
                "ACCT-GROUP-ID           :GROUP0001 ",
                "-------------------------------------------------",
                "VBRC-REC1:00000000001Y",
                "VBRC-REC2:0000000000100000001940{00000020200{2025",
                rec.asText(),
                "END OF EXECUTION OF PROGRAM CBACT01C"), sysout);
        assertEquals(1, out.records().size());
        assertEquals(1, arry.records().size());
        assertEquals(2, vbrc.records().size());
        assertTrue(acct.isClosed());
        assertTrue(out.isClosed() && arry.isClosed() && vbrc.isClosed());
    }

    @Test
    void emptyAccountFileWritesNothingAndEndsNormally() {
        assertEquals(0, program().run());
        assertEquals(List.of("START OF EXECUTION OF PROGRAM CBACT01C", "END OF EXECUTION OF PROGRAM CBACT01C"),
                sysout);
        assertTrue(out.records().isEmpty());
    }

    @Test
    void popul1300BuildsOutAcctRecByteForByte() {
        program(account(7, "-12.34", "0")).run();
        OutAcctRec r = OutAcctRec.fromBytes(out.records().get(0));
        assertEquals(107, out.records().get(0).length);
        assertEquals("00000000007", r.text(OutAcctRec.OUT_ACCT_ID));
        assertEquals("Y", r.text(OutAcctRec.OUT_ACCT_ACTIVE_STATUS));
        assertEquals("00000000123M", r.text(OutAcctRec.OUT_ACCT_CURR_BAL));
        assertEquals("00000020200{", r.text(OutAcctRec.OUT_ACCT_CREDIT_LIMIT));
        assertEquals("00000010200{", r.text(OutAcctRec.OUT_ACCT_CASH_CREDIT_LIMIT));
        assertEquals("2014-11-20", r.text(OutAcctRec.OUT_ACCT_OPEN_DATE));
        assertEquals("2025-05-20", r.text(OutAcctRec.OUT_ACCT_EXPIRAION_DATE));
        assertEquals("00000000000{", r.text(OutAcctRec.OUT_ACCT_CURR_CYC_CREDIT));
        assertEquals("GROUP0001 ", r.text(OutAcctRec.OUT_ACCT_GROUP_ID));
    }

    @Test
    void popul1300ReformatsReissueDateThroughCobdatft() {
        program(account(1, "1", "0")).run();
        OutAcctRec r = OutAcctRec.fromBytes(out.records().get(0));
        assertEquals("20250520  ", r.text(OutAcctRec.OUT_ACCT_REISSUE_DATE));
    }

    @Test
    void popul1300ZeroCycleDebitBecomes2525Packed() {
        program(account(1, "1", "0")).run();
        OutAcctRec r = OutAcctRec.fromBytes(out.records().get(0));
        assertArrayEquals(new byte[] {0, 0, 0, 0x02, 0x52, 0x50, 0x0C}, r.raw(OutAcctRec.OUT_ACCT_CURR_CYC_DEBIT));
        assertEquals(new BigDecimal("2525.00"), r.number(OutAcctRec.OUT_ACCT_CURR_CYC_DEBIT));
    }

    @Test
    void popul1300NegativeZeroCycleDebitCountsAsZero() {
        AccountRecord rec = account(1, "1", "0");
        byte[] raw = rec.toBytes();
        raw[AccountRecord.ACCT_CURR_CYC_DEBIT.end() - 1] = '}';
        acct = new InMemoryRecordReader(List.of(raw));
        new Cbact01c(acct, out, arry, vbrc, new CobdatftDateFormatter(), sysout::add).run();
        assertEquals(new BigDecimal("2525.00"),
                OutAcctRec.fromBytes(out.records().get(0)).number(OutAcctRec.OUT_ACCT_CURR_CYC_DEBIT));
    }

    @Test
    void popul1300NonZeroCycleDebitIsNotMovedSoRecordAreaKeepsPreviousValue() {
        program(account(1, "1", "0"), account(2, "1", "99.99")).run();
        OutAcctRec second = OutAcctRec.fromBytes(out.records().get(1));
        assertEquals(new BigDecimal("2525.00"), second.number(OutAcctRec.OUT_ACCT_CURR_CYC_DEBIT));
    }

    @Test
    void popul1300NonZeroCycleDebitOnFirstRecordLeavesInitialLowValues() {
        program(account(1, "1", "99.99")).run();
        assertArrayEquals(new byte[7],
                OutAcctRec.fromBytes(out.records().get(0)).raw(OutAcctRec.OUT_ACCT_CURR_CYC_DEBIT));
    }

    @Test
    void popul1400FillsThreeOccurrencesAndInitializesTheRest() {
        program(account(5, "-0.07", "0")).run();
        byte[] bytes = arry.records().get(0);
        assertEquals(110, bytes.length);
        ArrArrayRec r = ArrArrayRec.fromBytes(bytes);
        assertEquals("00000000005", r.text(ArrArrayRec.ARR_ACCT_ID));
        assertEquals("00000000000P", r.text(ArrArrayRec.arrAcctCurrBal(1)));
        assertEquals(new BigDecimal("1005.00"), r.number(ArrArrayRec.arrAcctCurrCycDebit(1)));
        assertEquals("00000000000P", r.text(ArrArrayRec.arrAcctCurrBal(2)));
        assertEquals(new BigDecimal("1525.00"), r.number(ArrArrayRec.arrAcctCurrCycDebit(2)));
        assertEquals("00000010250}", r.text(ArrArrayRec.arrAcctCurrBal(3)));
        assertArrayEquals(new byte[] {0, 0, 0, 0x02, 0x50, 0x00, 0x0D}, r.raw(ArrArrayRec.arrAcctCurrCycDebit(3)));
        for (int i = 4; i <= 5; i++) {
            assertEquals("00000000000{", r.text(ArrArrayRec.arrAcctCurrBal(i)));
            assertArrayEquals(new byte[] {0, 0, 0, 0, 0, 0, 0x0C}, r.raw(ArrArrayRec.arrAcctCurrCycDebit(i)));
        }
        assertEquals("    ", r.text(ArrArrayRec.ARR_FILLER));
    }

    @Test
    void writes1550And1575UseRecordLengths12And39() {
        Cbact01c p = program(account(3, "194.00", "0"));
        p.run();
        assertEquals("00000000003Y", text(vbrc.records().get(0)));
        assertEquals("0000000000300000001940{00000020200{2025", text(vbrc.records().get(1)));
        assertEquals(12, vbrc.records().get(0).length);
        assertEquals(39, vbrc.records().get(1).length);
        assertEquals(39, p.wsRecdLen());
    }

    @Test
    void getNext1000StopsAtEndOfFileWithoutDisplayingAgain() {
        Cbact01c p = program(account(1, "1", "0"));
        p.run();
        assertEquals("Y", p.endOfFile());
        assertEquals(0, p.applResult(), "9000-ACCTFILE-CLOSE resets APPL-RESULT to 0");
        assertEquals(1, sysout.stream().filter(l -> l.length() == 300).count());
    }

    @Test
    void getNext1000ReadErrorAbends() {
        program(account(1, "1", "0"));
        acct.failReadWith(2, "30");
        Cbact01c p = new Cbact01c(acct, out, arry, vbrc, new CobdatftDateFormatter(), sysout::add);
        AbendException e = assertThrows(AbendException.class, p::run);
        assertEquals(999, e.abendCode());
        assertEquals(0, e.timing());
        assertEquals(List.of("ERROR READING ACCOUNT FILE", "FILE STATUS IS: NNNN0030", "ABENDING PROGRAM"),
                sysout.subList(sysout.size() - 3, sysout.size()));
        assertEquals(1, out.records().size());
    }

    @Test
    void getNext1000Status04IsAnErrorNotEof() {
        program();
        acct.failReadWith(1, "04");
        Cbact01c p = new Cbact01c(acct, out, arry, vbrc, new CobdatftDateFormatter(), sysout::add);
        assertThrows(AbendException.class, p::run);
        assertEquals(12, p.applResult());
        assertTrue(sysout.contains("FILE STATUS IS: NNNN0004"));
    }

    @Test
    void open0000FailureAbends() {
        program();
        acct.failOpenWith("35");
        Cbact01c p = new Cbact01c(acct, out, arry, vbrc, new CobdatftDateFormatter(), sysout::add);
        assertThrows(AbendException.class, p::run);
        assertEquals(List.of("START OF EXECUTION OF PROGRAM CBACT01C", "ERROR OPENING ACCTFILE",
                "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM"), sysout);
    }

    @Test
    void open2000FailureAbendsWithStatusInMessage() {
        out.failOpenWith("37");
        assertThrows(AbendException.class, program()::run);
        assertEquals(List.of("ERROR OPENING OUTFILE37", "FILE STATUS IS: NNNN0037", "ABENDING PROGRAM"),
                sysout.subList(1, 4));
    }

    @Test
    void open3000FailureAbendsWithStatusInMessage() {
        arry.failOpenWith("30");
        assertThrows(AbendException.class, program()::run);
        assertEquals(List.of("ERROR OPENING ARRAYFILE30", "FILE STATUS IS: NNNN0030", "ABENDING PROGRAM"),
                sysout.subList(1, 4));
    }

    @Test
    void open4000FailureAbendsWithStatusInMessage() {
        vbrc.failOpenWith("90");
        assertThrows(AbendException.class, program()::run);
        assertEquals(List.of("ERROR OPENING VBRC FILE90", "FILE STATUS IS: NNNN9048", "ABENDING PROGRAM"),
                sysout.subList(1, 4));
    }

    @Test
    void write1350FailureAbends() {
        out.failWriteWith(1, "34");
        assertThrows(AbendException.class, program(account(1, "1", "0"))::run);
        assertEquals(List.of("ACCOUNT FILE WRITE STATUS IS:34", "FILE STATUS IS: NNNN0034", "ABENDING PROGRAM"),
                sysout.subList(sysout.size() - 3, sysout.size()));
        assertTrue(arry.records().isEmpty());
    }

    @Test
    void write1450FailureAbends() {
        arry.failWriteWith(1, "34");
        assertThrows(AbendException.class, program(account(1, "1", "0"))::run);
        assertEquals("ACCOUNT FILE WRITE STATUS IS:34", sysout.get(sysout.size() - 3));
        assertTrue(vbrc.records().isEmpty());
    }

    @Test
    void write1550FailureAbends() {
        vbrc.failWriteWith(1, "44");
        assertThrows(AbendException.class, program(account(1, "1", "0"))::run);
        assertEquals("ACCOUNT FILE WRITE STATUS IS:44", sysout.get(sysout.size() - 3));
        assertTrue(vbrc.records().isEmpty());
    }

    @Test
    void write1575FailureAbends() {
        vbrc.failWriteWith(2, "44");
        assertThrows(AbendException.class, program(account(1, "1", "0"))::run);
        assertEquals("ACCOUNT FILE WRITE STATUS IS:44", sysout.get(sysout.size() - 3));
        assertEquals(1, vbrc.records().size());
    }

    @Test
    void writeStatus10IsTolerated() {
        out.failWriteWith(1, "10");
        assertEquals(0, program(account(1, "1", "0")).run());
    }

    @Test
    void close9000FailureAbends() {
        program();
        acct.failCloseWith("42");
        Cbact01c p = new Cbact01c(acct, out, arry, vbrc, new CobdatftDateFormatter(), sysout::add);
        assertThrows(AbendException.class, p::run);
        assertEquals(List.of("ERROR CLOSING ACCOUNT FILE", "FILE STATUS IS: NNNN0042", "ABENDING PROGRAM"),
                sysout.subList(1, 4));
    }

    @Test
    void displayIoStatus9910Formats() {
        assertEquals("0035", Cbact01c.formatIoStatus04("35"));
        assertEquals("0000", Cbact01c.formatIoStatus04("00"));
        // '9x' statuses: IO-STAT1 then binary value of the IO-STAT2 byte (ASCII '2' = 50).
        assertEquals("9050", Cbact01c.formatIoStatus04("92"));
        // non-numeric status: same treatment (ASCII 'A' = 65)
        assertEquals("0065", Cbact01c.formatIoStatus04("0A"));
        assertEquals("1032", Cbact01c.formatIoStatus04("1 "));
    }
}
