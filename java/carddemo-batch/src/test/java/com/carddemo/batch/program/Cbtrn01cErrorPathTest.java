package com.carddemo.batch.program;

import com.carddemo.batch.io.AbendException;
import com.carddemo.batch.io.FileStatusException;
import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.io.SequentialFile;
import com.carddemo.batch.record.CardXrefRecord;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** FILE STATUS -> FileStatusException and Z-ABEND-PROGRAM / CEE3ABD -> AbendException for CBTRN01C. */
class Cbtrn01cErrorPathTest {

    private static final String START = "START OF EXECUTION OF PROGRAM CBTRN01C\n";

    private static Path empty(Path tmp) throws IOException {
        return Files.write(tmp.resolve("TRANFILE"), new byte[0]);
    }

    private static Cbtrn01c program(Path dalytran, Path cust, Path xref, Path card, Path acct, Path tran, ByteArrayOutputStream buf) {
        PrintStream ps = new PrintStream(buf, true, StandardCharsets.ISO_8859_1);
        return new Cbtrn01c(dalytran, cust, xref, card, acct, tran, ps);
    }

    private static String text(ByteArrayOutputStream buf) {
        return buf.toString(StandardCharsets.ISO_8859_1);
    }

    @Test
    void missingDalytranAbendsWithStatus35(@TempDir Path tmp) throws IOException {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        Cbtrn01c prog = program(tmp.resolve("missing"), Repo.SAMPLE_CUSTDATA, Repo.SAMPLE_CARDXREF, Repo.SAMPLE_CARDDATA,
                Repo.SAMPLE_ACCTDATA, empty(tmp), buf);
        AbendException abend = assertThrows(AbendException.class, prog::run);
        assertEquals(999, abend.abendCode());
        assertEquals(0, abend.timing());
        assertEquals("CEE3ABD: USER ABEND U999 TIMING=0", abend.getMessage());
        FileStatusException fs = assertInstanceOf(FileStatusException.class, abend.getCause());
        assertEquals("35", fs.status());
        assertEquals("DALYTRAN", fs.ddname());
        assertEquals("35", prog.dalytranStatus());
        assertEquals(START + "ERROR OPENING DAILY TRANSACTION FILE\nFILE STATUS IS: NNNN0035\nABENDING PROGRAM\n", text(buf));
        assertTrue(prog.outcomes().isEmpty());
    }

    @Test
    void eachOpenParagraphReportsItsOwnFile(@TempDir Path tmp) throws IOException {
        Path missing = tmp.resolve("missing");
        Path tran = empty(tmp);
        String[][] cases = {
            {"CUSTFILE", "ERROR OPENING CUSTOMER FILE"},
            {"XREFFILE", "ERROR OPENING CROSS REF FILE"},
            {"CARDFILE", "ERROR OPENING CARD FILE"},
            {"ACCTFILE", "ERROR OPENING ACCOUNT FILE"},
            {"TRANFILE", "ERROR OPENING TRANSACTION FILE"},
        };
        for (String[] c : cases) {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            Cbtrn01c prog = program(Repo.SAMPLE_DAILYTRAN,
                    c[0].equals("CUSTFILE") ? missing : Repo.SAMPLE_CUSTDATA,
                    c[0].equals("XREFFILE") ? missing : Repo.SAMPLE_CARDXREF,
                    c[0].equals("CARDFILE") ? missing : Repo.SAMPLE_CARDDATA,
                    c[0].equals("ACCTFILE") ? missing : Repo.SAMPLE_ACCTDATA,
                    c[0].equals("TRANFILE") ? missing : tran, buf);
            AbendException abend = assertThrows(AbendException.class, prog::run, c[0]);
            FileStatusException fs = assertInstanceOf(FileStatusException.class, abend.getCause());
            assertEquals(c[0], fs.ddname());
            assertEquals("35", fs.status(), c[0]);
            assertEquals(START + c[1] + "\nFILE STATUS IS: NNNN0035\nABENDING PROGRAM\n", text(buf), c[0]);
        }
    }

    @Test
    void emptyTranfileOpensSuccessfullyBecauseItIsNeverRead(@TempDir Path tmp) throws IOException {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        Cbtrn01c prog = program(Repo.SAMPLE_DAILYTRAN, Repo.SAMPLE_CUSTDATA, Repo.SAMPLE_CARDXREF, Repo.SAMPLE_CARDDATA,
                Repo.SAMPLE_ACCTDATA, empty(tmp), buf);
        assertEquals(0, prog.run());
        assertEquals("00", prog.tranfileStatus());
        assertEquals(300, prog.outcomes().size());
    }

    @Test
    void shortDalytranRecordIsAttributeMismatch39(@TempDir Path tmp) throws IOException {
        Path dalytran = tmp.resolve("dailytran.txt");
        Files.write(dalytran, "0000000000683580 short record\n".getBytes(StandardCharsets.ISO_8859_1));
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        Cbtrn01c prog = program(dalytran, Repo.SAMPLE_CUSTDATA, Repo.SAMPLE_CARDXREF, Repo.SAMPLE_CARDDATA,
                Repo.SAMPLE_ACCTDATA, empty(tmp), buf);
        AbendException abend = assertThrows(AbendException.class, prog::run);
        assertEquals("39", ((FileStatusException) abend.getCause()).status());
        assertEquals(START + "ERROR OPENING DAILY TRANSACTION FILE\nFILE STATUS IS: NNNN0039\nABENDING PROGRAM\n", text(buf));
    }

    @Test
    void emptyDalytranPerformsTheSinglePostEofLookupAgainstTheInitialRecordArea(@TempDir Path tmp) throws IOException {
        Path dalytran = Files.write(tmp.resolve("dailytran.txt"), new byte[0]);
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        Cbtrn01c prog = program(dalytran, Repo.SAMPLE_CUSTDATA, Repo.SAMPLE_CARDXREF, Repo.SAMPLE_CARDDATA,
                Repo.SAMPLE_ACCTDATA, empty(tmp), buf);
        assertEquals(0, prog.run());
        assertTrue(prog.outcomes().isEmpty(), "no transaction, no outcome row");
        String spaces16 = " ".repeat(16);
        assertEquals(START + "INVALID CARD NUMBER FOR XREF\nCARD NUMBER " + spaces16
                + " COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-" + spaces16 + "\nEND OF EXECUTION OF PROGRAM CBTRN01C\n",
                text(buf), "the lookups still run once with DALYTRAN-RECORD at its initial (spaces) content");
    }

    @Test
    void sequentialFileReturnsRecordsInFileOrderAndMapsStatuses(@TempDir Path tmp) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        for (String id : new String[] {"30", "02", "10"}) {
            byte[] rec = ("rec" + id + " ").getBytes(StandardCharsets.ISO_8859_1);
            out.write(rec);
            out.write('\n');
        }
        Path f = Files.write(tmp.resolve("seq.txt"), out.toByteArray());
        SequentialFile seq = new SequentialFile("DALYTRAN", f, 6);
        assertEquals("42", assertThrows(FileStatusException.class, seq::readNext).status(), "READ before OPEN");
        assertFalse(seq.isOpen());
        seq.open();
        assertEquals(3, seq.recordCount());
        assertEquals("rec30 ", new String(seq.readNext().orElseThrow(), StandardCharsets.ISO_8859_1));
        assertEquals("rec02 ", new String(seq.readNext().orElseThrow(), StandardCharsets.ISO_8859_1));
        assertEquals("rec10 ", new String(seq.readNext().orElseThrow(), StandardCharsets.ISO_8859_1));
        assertTrue(seq.readNext().isEmpty(), "end of file -> status 10");
        seq.close();
        assertEquals("42", assertThrows(FileStatusException.class, seq::close).status());
        assertEquals("35", assertThrows(FileStatusException.class,
                () -> new SequentialFile("DALYTRAN", tmp.resolve("nope"), 6).open()).status());
    }

    @Test
    void lineSequentialKsdsLoadPadsShortLinesWithSpaces(@TempDir Path tmp) throws IOException {
        Path xref = Files.write(tmp.resolve("xref.txt"),
                ("1111111111111111" + "000000001" + "00000000001" + "\n"
                        + "2222222222222222" + "000000002" + "00000000002" + "              " + "\n")
                        .getBytes(StandardCharsets.ISO_8859_1));
        KsdsFile padded = new KsdsFile("XREFFILE", xref, CardXrefRecord.LENGTH, 0, 16, KsdsFile.LoadFormat.LINE_SEQUENTIAL);
        padded.open();
        assertEquals(2, padded.recordCount());
        byte[] rec = padded.read("1111111111111111").orElseThrow();
        assertEquals(50, rec.length);
        assertEquals("1111111111111111" + "000000001" + "00000000001" + "              ",
                new String(rec, StandardCharsets.ISO_8859_1), "FILLER padded with spaces as KSDSLOAD's LINE SEQUENTIAL READ does");
        assertTrue(padded.read("3333333333333333").isEmpty(), "random READ of a missing key -> status 23");

        KsdsFile fixed = new KsdsFile("XREFFILE", xref, CardXrefRecord.LENGTH, 0, 16);
        assertEquals("39", assertThrows(FileStatusException.class, fixed::open).status(),
                "the default FIXED load still rejects short records");

        Path tooLong = Files.write(tmp.resolve("long.txt"), ("x".repeat(51) + "\n").getBytes(StandardCharsets.ISO_8859_1));
        assertEquals("39", assertThrows(FileStatusException.class,
                () -> new KsdsFile("XREFFILE", tooLong, 50, 0, 16, KsdsFile.LoadFormat.LINE_SEQUENTIAL).open()).status());
    }

    @Test
    void sampleCardxrefLinesAreShorterThanTheRecordAndStillLoadAllFiftyKeys() {
        KsdsFile xref = new KsdsFile("XREFFILE", Repo.SAMPLE_CARDXREF, CardXrefRecord.LENGTH, 0, 16, KsdsFile.LoadFormat.LINE_SEQUENTIAL);
        xref.open();
        assertEquals(50, xref.recordCount());
        CardXrefRecord r = CardXrefRecord.decode(xref.read("4859452612877065").orElseThrow());
        assertEquals(7L, r.xrefAcctId());
        assertEquals(7L, r.xrefCustId());
        assertEquals("              ", new String(r.encode(), 36, 14, StandardCharsets.ISO_8859_1), "FILLER");
    }
}
