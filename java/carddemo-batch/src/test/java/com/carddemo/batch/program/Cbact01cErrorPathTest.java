package com.carddemo.batch.program;

import com.carddemo.batch.io.AbendException;
import com.carddemo.batch.io.FileStatusException;
import com.carddemo.batch.io.FixedRecordWriter;
import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.io.RecordPrefix;
import com.carddemo.batch.io.VariableRecordWriter;
import com.carddemo.batch.record.AccountRecord;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** FILE STATUS -> FileStatusException and 9999-ABEND-PROGRAM / CEE3ABD -> AbendException. */
class Cbact01cErrorPathTest {

    private static String run(Path acct, Path dir, StringBuilder displayOut) {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(buf, true, StandardCharsets.ISO_8859_1);
        try {
            new Cbact01c(acct, dir.resolve("OUTFILE"), dir.resolve("ARRYFILE"), dir.resolve("VBRCFILE"), RecordPrefix.GNUCOBOL_VARSEQ, ps).run();
            return null;
        } catch (AbendException e) {
            return e.getMessage();
        } finally {
            displayOut.append(buf.toString(StandardCharsets.ISO_8859_1));
        }
    }

    @Test
    void missingAcctfileAbendsWithStatus35(@TempDir Path tmp) {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(buf, true, StandardCharsets.ISO_8859_1);
        Cbact01c prog = new Cbact01c(tmp.resolve("missing.txt"), tmp.resolve("OUTFILE"), tmp.resolve("ARRYFILE"), tmp.resolve("VBRCFILE"), ps);
        AbendException abend = assertThrows(AbendException.class, prog::run);
        assertEquals(999, abend.abendCode());
        assertEquals(0, abend.timing());
        assertEquals("CEE3ABD: USER ABEND U999 TIMING=0", abend.getMessage());
        FileStatusException fs = assertInstanceOf(FileStatusException.class, abend.getCause());
        assertEquals("35", fs.status());
        assertEquals("ACCTFILE", fs.ddname());
        assertEquals("35", prog.acctfileStatus());
        String display = buf.toString(StandardCharsets.ISO_8859_1);
        assertEquals("START OF EXECUTION OF PROGRAM CBACT01C\nERROR OPENING ACCTFILE\nFILE STATUS IS: NNNN0035\nABENDING PROGRAM\n", display);
    }

    @Test
    void unwritableOutputDirectoryAbendsOnOutfileOpen(@TempDir Path tmp) {
        StringBuilder display = new StringBuilder();
        String abend = run(Repo.SAMPLE_ACCTDATA, tmp.resolve("no-such-dir"), display);
        assertEquals("CEE3ABD: USER ABEND U999 TIMING=0", abend);
        assertTrue(display.toString().contains("ERROR OPENING OUTFILE35\n"), display.toString());
        assertTrue(display.toString().contains("FILE STATUS IS: NNNN0035\n"), display.toString());
        assertTrue(display.toString().endsWith("ABENDING PROGRAM\n"), display.toString());
        assertFalse(display.toString().contains("ACCT-ID"), "no account was processed");
    }

    @Test
    void badRecordLengthIsAttributeMismatch39(@TempDir Path tmp) throws IOException {
        Path acct = tmp.resolve("acct.txt");
        Files.write(acct, "0000000000A short record\n".getBytes(StandardCharsets.ISO_8859_1));
        StringBuilder display = new StringBuilder();
        String abend = run(acct, tmp, display);
        assertEquals("CEE3ABD: USER ABEND U999 TIMING=0", abend);
        assertTrue(display.toString().contains("ERROR OPENING ACCTFILE\nFILE STATUS IS: NNNN0039\n"), display.toString());
    }

    @Test
    void duplicateKeysAreRejectedWithStatus22(@TempDir Path tmp) throws IOException {
        byte[] rec = new byte[AccountRecord.LENGTH];
        Arrays.fill(rec, (byte) ' ');
        System.arraycopy("00000000001".getBytes(StandardCharsets.ISO_8859_1), 0, rec, 0, 11);
        ByteArrayOutputStream two = new ByteArrayOutputStream();
        two.write(rec);
        two.write('\n');
        two.write(rec);
        two.write('\n');
        Path acct = tmp.resolve("dup.txt");
        Files.write(acct, two.toByteArray());
        KsdsFile ksds = new KsdsFile("ACCTFILE", acct, AccountRecord.LENGTH, 0, 11);
        FileStatusException e = assertThrows(FileStatusException.class, ksds::open);
        assertEquals("22", e.status());
        assertEquals("OPEN", e.operation());
    }

    @Test
    void ksdsReadsInKeyOrderNotFileOrder(@TempDir Path tmp) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        for (String id : new String[] {"00000000030", "00000000002", "00000000010"}) {
            byte[] rec = new byte[AccountRecord.LENGTH];
            Arrays.fill(rec, (byte) ' ');
            System.arraycopy(id.getBytes(StandardCharsets.ISO_8859_1), 0, rec, 0, 11);
            out.write(rec);
            out.write('\n');
        }
        Path acct = tmp.resolve("unsorted.txt");
        Files.write(acct, out.toByteArray());
        KsdsFile ksds = new KsdsFile("ACCTFILE", acct, AccountRecord.LENGTH, 0, 11);
        assertThrows(FileStatusException.class, ksds::readNext, "READ before OPEN is status 42");
        ksds.open();
        assertEquals("00000000002", key(ksds.readNext().orElseThrow()));
        assertEquals("00000000010", key(ksds.readNext().orElseThrow()));
        assertEquals("00000000030", key(ksds.readNext().orElseThrow()));
        assertTrue(ksds.readNext().isEmpty(), "end of file -> status 10");
        assertTrue(ksds.read("00000000010").isPresent(), "random READ by key");
        assertTrue(ksds.read("00000000011").isEmpty(), "random READ of a missing key -> status 23");
        ksds.close();
        assertEquals("42", assertThrows(FileStatusException.class, ksds::close).status());
    }

    private static String key(byte[] rec) {
        return new String(rec, 0, 11, StandardCharsets.ISO_8859_1);
    }

    @Test
    void writersMapIoFailuresToFileStatus(@TempDir Path tmp) {
        FixedRecordWriter fixed = new FixedRecordWriter("OUTFILE", tmp.resolve("OUTFILE"), 107);
        assertEquals("42", assertThrows(FileStatusException.class, () -> fixed.write(new byte[107])).status());
        fixed.open();
        assertEquals("39", assertThrows(FileStatusException.class, () -> fixed.write(new byte[106])).status());
        fixed.close();
        assertEquals("42", assertThrows(FileStatusException.class, fixed::close).status());

        VariableRecordWriter vb = new VariableRecordWriter("VBRCFILE", tmp.resolve("VBRCFILE"), RecordPrefix.GNUCOBOL_VARSEQ, 10, 80);
        vb.open();
        assertEquals("39", assertThrows(FileStatusException.class, () -> vb.write(new byte[80], 81)).status());
        assertEquals("39", assertThrows(FileStatusException.class, () -> vb.write(new byte[80], 9)).status());
        vb.close();
        assertEquals("35", assertThrows(FileStatusException.class,
                () -> new FixedRecordWriter("ARRYFILE", tmp.resolve("nope").resolve("ARRYFILE"), 110).open()).status());
    }

    @Test
    void ioStatusDisplayFollows9910Paragraph(@TempDir Path tmp) {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(buf, true, StandardCharsets.ISO_8859_1);
        Cbact01c prog = new Cbact01c(tmp.resolve("x"), tmp.resolve("OUTFILE"), tmp.resolve("ARRYFILE"), tmp.resolve("VBRCFILE"), ps);
        assertThrows(AbendException.class, prog::run);
        assertTrue(buf.toString(StandardCharsets.ISO_8859_1).contains("FILE STATUS IS: NNNN0035"));
    }
}
