package com.carddemo.batch.support;

import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.ZonedDecimal;
import com.carddemo.batch.io.RecordPrefix;
import com.carddemo.batch.program.Cbact01c;
import com.carddemo.batch.record.AccountRecord;
import com.carddemo.batch.record.ArrArrayRec;
import com.carddemo.batch.record.OutAcctRec;
import com.carddemo.batch.record.VbrcRec1;
import com.carddemo.batch.record.VbrcRec2;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
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
 * One execution of {@link Cbact01c} on a given ACCTFILE, writing into {@code target/parity/<name>/}.
 * The two fixtures are run once per JVM and shared by the parity tests.
 */
public final class Cbact01cRun {

    private static Cbact01cRun sample;
    private static Cbact01cRun synthetic;

    public final String name;
    public final Path acctfile;
    public final Path dir;
    public final Path outfile;
    public final Path arryfile;
    public final Path vbrcfile;
    public final String display;
    public final int returnCode;

    private Cbact01cRun(String name, Path acctfile) {
        this.name = name;
        this.acctfile = acctfile;
        this.dir = Paths.get("target", "parity", name).toAbsolutePath();
        this.outfile = dir.resolve("OUTFILE");
        this.arryfile = dir.resolve("ARRYFILE");
        this.vbrcfile = dir.resolve("VBRCFILE");
        try {
            Files.createDirectories(dir);
            Files.deleteIfExists(outfile);
            Files.deleteIfExists(arryfile);
            Files.deleteIfExists(vbrcfile);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(buf, true, StandardCharsets.ISO_8859_1);
        this.returnCode = new Cbact01c(acctfile, outfile, arryfile, vbrcfile, RecordPrefix.GNUCOBOL_VARSEQ, ps).run();
        ps.flush();
        this.display = buf.toString(StandardCharsets.ISO_8859_1);
        try {
            Files.writeString(dir.resolve("display.txt"), display, StandardCharsets.ISO_8859_1);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** The 50-account sample data {@code app/data/ASCII/acctdata.txt}. */
    public static synchronized Cbact01cRun sample() {
        if (sample == null) {
            sample = new Cbact01cRun("sample", Repo.SAMPLE_ACCTDATA);
        }
        return sample;
    }

    /**
     * The {@code golden-files/CBACT01C/synthetic-mixed-debit} fixture, rebuilt exactly as
     * {@code test-harness/cobol/run_synthetic_mixed_debit.sh} does: the first five sample accounts with
     * ACCT-CURR-CYC-DEBIT set to 10.00 / (0) / 120.50 / -75.25 / (0).
     */
    public static synchronized Cbact01cRun syntheticMixedDebit() {
        if (synthetic == null) {
            Path dir = Paths.get("target", "parity", "synthetic-mixed-debit").toAbsolutePath();
            try {
                Files.createDirectories(dir);
                List<byte[]> recs = readAccountRecords(Repo.SAMPLE_ACCTDATA).subList(0, 5);
                setDebit(recs.get(0), "10.00");
                setDebit(recs.get(2), "120.50");
                setDebit(recs.get(3), "-75.25");
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                for (byte[] r : recs) {
                    out.write(r);
                    out.write('\n');
                }
                Path acct = dir.resolve("acctdata.txt");
                Files.write(acct, out.toByteArray());
                synthetic = new Cbact01cRun("synthetic-mixed-debit", acct);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
        return synthetic;
    }

    private static void setDebit(byte[] rec, String value) {
        ZonedDecimal.encode(rec, AccountRecord.ACCT_CURR_CYC_DEBIT.offset(), AccountRecord.ACCT_CURR_CYC_DEBIT.length(),
                2, true, new BigDecimal(value));
    }

    /** The newline-delimited 300-byte records of an ACCTFILE, in file order. */
    public static List<byte[]> readAccountRecords(Path acctfile) {
        try {
            byte[] all = Files.readAllBytes(acctfile);
            List<byte[]> out = new ArrayList<>();
            int pos = 0;
            while (pos + AccountRecord.LENGTH <= all.length) {
                out.add(Arrays.copyOfRange(all, pos, pos + AccountRecord.LENGTH));
                pos += AccountRecord.LENGTH;
                if (pos < all.length && all[pos] == '\n') {
                    pos++;
                }
            }
            return out;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public List<Map<String, Object>> inputJson() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (byte[] r : readAccountRecords(acctfile)) {
            out.add(JsonRecords.toJson(r, AccountRecord.LAYOUT));
        }
        return out;
    }

    public byte[] bytes(Path p) {
        try {
            return Files.readAllBytes(p);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public List<byte[]> outfileRecords() {
        return split(bytes(outfile), OutAcctRec.LENGTH);
    }

    public List<byte[]> arryfileRecords() {
        return split(bytes(arryfile), ArrArrayRec.LENGTH);
    }

    /** OUTFILE decoded in the golden JSON shape (lenient COMP-3 decoding). */
    public List<Map<String, Object>> outfileJson() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (byte[] r : outfileRecords()) {
            out.add(JsonRecords.toJson(r, OutAcctRec.LAYOUT));
        }
        return out;
    }

    public List<Map<String, Object>> arryfileJson() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (byte[] r : arryfileRecords()) {
            out.add(JsonRecords.toJson(r, ArrArrayRec.LAYOUT));
        }
        return out;
    }

    /** VBRCFILE payloads (4-byte GnuCOBOL length prefix stripped), in file order. */
    public List<byte[]> vbrcfilePayloads() {
        byte[] all = bytes(vbrcfile);
        List<byte[]> out = new ArrayList<>();
        ByteBuffer bb = ByteBuffer.wrap(all);
        while (bb.remaining() >= 4) {
            int len = bb.getInt();
            if (len > bb.remaining()) {
                throw new IllegalStateException("VBRCFILE: length prefix " + len + " exceeds the remaining bytes");
            }
            byte[] payload = new byte[len];
            bb.get(payload);
            out.add(payload);
        }
        if (bb.hasRemaining()) {
            throw new IllegalStateException("VBRCFILE: " + bb.remaining() + " trailing bytes after the last record");
        }
        return out;
    }

    /** VBRCFILE in the golden JSON shape: {@code _record}, {@code _length} and the record's fields. */
    public List<Map<String, Object>> vbrcfileJson() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (byte[] p : vbrcfilePayloads()) {
            Map<String, Object> m = new LinkedHashMap<>();
            if (p.length == VbrcRec1.LENGTH) {
                m.put("_record", "VBRC-REC1");
                m.put("_length", p.length);
                m.putAll(JsonRecords.toJson(p, VbrcRec1.LAYOUT));
            } else if (p.length == VbrcRec2.LENGTH) {
                m.put("_record", "VBRC-REC2");
                m.put("_length", p.length);
                m.putAll(JsonRecords.toJson(p, VbrcRec2.LAYOUT));
            } else {
                m.put("_record", "UNKNOWN");
                m.put("_length", p.length);
                m.put("_bytes", new String(p, FixedWidth.CHARSET));
            }
            out.add(m);
        }
        return out;
    }

    /** Writes the four JSON files reconcile.py needs into {@code dir} and returns it. */
    public Path writeJsonForHarness() {
        Gson gson = new GsonBuilder().setPrettyPrinting().disableHtmlEscaping().create();
        try {
            Files.writeString(dir.resolve("input-acctdata.json"), gson.toJson(inputJson()), StandardCharsets.UTF_8);
            Files.writeString(dir.resolve("outfile.json"), gson.toJson(outfileJson()), StandardCharsets.UTF_8);
            Files.writeString(dir.resolve("arryfile.json"), gson.toJson(arryfileJson()), StandardCharsets.UTF_8);
            Files.writeString(dir.resolve("vbrcfile.json"), gson.toJson(vbrcfileJson()), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return dir;
    }

    public static List<byte[]> split(byte[] all, int recordLength) {
        if (all.length % recordLength != 0) {
            throw new IllegalStateException(all.length + " bytes is not a multiple of LRECL " + recordLength);
        }
        List<byte[]> out = new ArrayList<>(all.length / recordLength);
        for (int pos = 0; pos < all.length; pos += recordLength) {
            out.add(Arrays.copyOfRange(all, pos, pos + recordLength));
        }
        return out;
    }
}
