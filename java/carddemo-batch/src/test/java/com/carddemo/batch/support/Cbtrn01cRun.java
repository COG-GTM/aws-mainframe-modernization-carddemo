package com.carddemo.batch.support;

import com.carddemo.batch.program.Cbtrn01c;
import com.carddemo.batch.program.TransactionOutcome;
import com.carddemo.batch.record.AccountRecord;
import com.carddemo.batch.record.CardXrefRecord;
import com.carddemo.batch.record.DalytranRecord;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * One execution of {@link Cbtrn01c} on a given DALYTRAN / XREFFILE pair (CUSTFILE, CARDFILE and ACCTFILE are
 * always the sample data, TRANFILE is an empty file as in the harness), writing into
 * {@code target/parity/<name>/}. The two fixtures are run once per JVM and shared by the parity tests.
 */
public final class Cbtrn01cRun {

    private static Cbtrn01cRun sample;
    private static Cbtrn01cRun synthetic;

    public final String name;
    public final Path dalytran;
    public final Path cardxref;
    public final Path dir;
    public final Path tranfile;
    public final String display;
    public final int returnCode;
    public final List<TransactionOutcome> outcomes;

    private Cbtrn01cRun(String name, Path dalytran, Path cardxref) {
        this.name = name;
        this.dalytran = dalytran;
        this.cardxref = cardxref;
        this.dir = Paths.get("target", "parity", name).toAbsolutePath();
        this.tranfile = dir.resolve("TRANFILE");
        try {
            Files.createDirectories(dir);
            Files.write(tranfile, new byte[0]);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(buf, true, StandardCharsets.ISO_8859_1);
        Cbtrn01c prog = new Cbtrn01c(dalytran, Repo.SAMPLE_CUSTDATA, cardxref, Repo.SAMPLE_CARDDATA,
                Repo.SAMPLE_ACCTDATA, tranfile, ps);
        this.returnCode = prog.run();
        ps.flush();
        this.display = buf.toString(StandardCharsets.ISO_8859_1);
        this.outcomes = prog.outcomes();
        try {
            Files.writeString(dir.resolve("display.txt"), display, StandardCharsets.ISO_8859_1);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** The 300-transaction sample data {@code app/data/ASCII/dailytran.txt} against the sample XREFFILE. */
    public static synchronized Cbtrn01cRun sample() {
        if (sample == null) {
            sample = new Cbtrn01cRun("cbtrn01c-sample", Repo.SAMPLE_DAILYTRAN, Repo.SAMPLE_CARDXREF);
        }
        return sample;
    }

    /**
     * The {@code golden-files/CBTRN01C/synthetic-rejections} fixture, rebuilt exactly as
     * {@code test-harness/cobol/run_synthetic_rejections.sh} does: the first three sample transactions, the
     * second with card 9999999999999999 (not in the xref) and the third with card 8888888888888888, which is
     * appended to the xref pointing at account 99999999999 (not in acctdata).
     */
    public static synchronized Cbtrn01cRun syntheticRejections() {
        if (synthetic == null) {
            Path dir = Paths.get("target", "parity", "cbtrn01c-synthetic-rejections").toAbsolutePath();
            try {
                Files.createDirectories(dir);
                List<byte[]> recs = readRecords(Repo.SAMPLE_DAILYTRAN, DalytranRecord.LENGTH).subList(0, 3);
                setCard(recs.get(1), "9999999999999999");
                setCard(recs.get(2), "8888888888888888");
                ByteArrayOutputStream tran = new ByteArrayOutputStream();
                for (byte[] r : recs) {
                    tran.write(r);
                    tran.write('\n');
                }
                Path dalytran = dir.resolve("dailytran.txt");
                Files.write(dalytran, tran.toByteArray());
                byte[] xref = Files.readAllBytes(Repo.SAMPLE_CARDXREF);
                ByteArrayOutputStream xrefOut = new ByteArrayOutputStream();
                xrefOut.write(xref);
                xrefOut.write(("8888888888888888" + "000000099" + "99999999999" + "\n").getBytes(StandardCharsets.ISO_8859_1));
                Path cardxref = dir.resolve("cardxref.txt");
                Files.write(cardxref, xrefOut.toByteArray());
                synthetic = new Cbtrn01cRun("cbtrn01c-synthetic-rejections", dalytran, cardxref);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
        return synthetic;
    }

    private static void setCard(byte[] rec, String cardNum) {
        System.arraycopy(cardNum.getBytes(StandardCharsets.ISO_8859_1), 0, rec,
                DalytranRecord.DALYTRAN_CARD_NUM.offset(), DalytranRecord.DALYTRAN_CARD_NUM.length());
    }

    /** The newline-delimited fixed-width records of a sample file, in file order. */
    public static List<byte[]> readRecords(Path file, int recordLength) {
        try {
            byte[] all = Files.readAllBytes(file);
            List<byte[]> out = new ArrayList<>();
            int pos = 0;
            while (pos + recordLength <= all.length) {
                out.add(Arrays.copyOfRange(all, pos, pos + recordLength));
                pos += recordLength;
                if (pos < all.length && all[pos] == '\n') {
                    pos++;
                }
            }
            return out;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** The lines of a line-sequential sample file, space-padded to the record length (KSDSLOAD semantics). */
    public static List<byte[]> readLines(Path file, int recordLength) {
        try {
            List<byte[]> out = new ArrayList<>();
            for (String line : Files.readAllLines(file, StandardCharsets.ISO_8859_1)) {
                byte[] rec = new byte[recordLength];
                Arrays.fill(rec, (byte) ' ');
                byte[] b = line.getBytes(StandardCharsets.ISO_8859_1);
                System.arraycopy(b, 0, rec, 0, b.length);
                out.add(rec);
            }
            return out;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** DALYTRAN decoded through the Java {@link DalytranRecord} layout, in the golden JSON shape. */
    public List<Map<String, Object>> inputJson() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (byte[] r : readRecords(dalytran, DalytranRecord.LENGTH)) {
            out.add(JsonRecords.toJson(r, DalytranRecord.LAYOUT));
        }
        return out;
    }

    public List<Map<String, Object>> xrefJson() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (byte[] r : readLines(cardxref, CardXrefRecord.LENGTH)) {
            out.add(JsonRecords.toJson(r, CardXrefRecord.LAYOUT));
        }
        return out;
    }

    public List<Map<String, Object>> acctJson() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (byte[] r : readRecords(Repo.SAMPLE_ACCTDATA, AccountRecord.LENGTH)) {
            out.add(JsonRecords.toJson(r, AccountRecord.LAYOUT));
        }
        return out;
    }

    /** The outcome rows in the golden JSON shape ({@code acct_id} as a records.py number string or null). */
    public List<Map<String, Object>> outcomesJson() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (TransactionOutcome o : outcomes) {
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("tran_id", o.tranId());
            m.put("card_num", o.cardNum());
            m.put("xref_found", o.xrefFound());
            m.put("acct_id", o.acctId() == null ? null : Long.toString(o.acctId()));
            m.put("acct_found", o.acctFound());
            m.put("outcome", o.outcome().name());
            out.add(m);
        }
        return out;
    }

    /** Writes the JSON files and display.txt reconcile.py needs into {@code dir} and returns it. */
    public Path writeJsonForHarness() {
        Gson gson = new GsonBuilder().setPrettyPrinting().serializeNulls().disableHtmlEscaping().create();
        try {
            Files.writeString(dir.resolve("input-dailytran.json"), gson.toJson(inputJson()), StandardCharsets.UTF_8);
            Files.writeString(dir.resolve("input-cardxref.json"), gson.toJson(xrefJson()), StandardCharsets.UTF_8);
            Files.writeString(dir.resolve("input-acctdata.json"), gson.toJson(acctJson()), StandardCharsets.UTF_8);
            Files.writeString(dir.resolve("outcomes.json"), gson.toJson(outcomesJson()), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return dir;
    }

    /** {@code count_display_lookups} of reconcile.py: XREF lookups DISPLAYed (successful or invalid). */
    public int displayLookups() {
        int n = 0;
        for (String line : display.split("\n", -1)) {
            if (line.startsWith("SUCCESSFUL READ OF XREF") || line.startsWith("INVALID CARD NUMBER FOR XREF")) {
                n++;
            }
        }
        return n;
    }
}
