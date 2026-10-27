package com.carddemo.modernization.interest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.modernization.records.AccountRecord;
import com.carddemo.modernization.records.CardXrefRecord;
import com.carddemo.modernization.records.DisclosureGroupRecord;
import com.carddemo.modernization.records.TranCatBalanceRecord;
import com.carddemo.modernization.records.TransactionRecord;
import com.carddemo.modernization.records.codec.RecordCodec;
import java.io.IOException;
import java.lang.reflect.RecordComponent;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HexFormat;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Differential test of the CBACT04C port against the GnuCOBOL golden master in
 * {@code modernization/java21/golden/expected/}. Inputs are the post-posting snapshots the COBOL interest step
 * consumed ({@code CBTRN02C.*.bin}); outputs are compared byte for byte and field by field with
 * {@code SYSTRAN.bin}, {@code CBACT04C.ACCTFILE.bin} and {@code CBACT04C.sysout.txt}. No tolerance is applied.
 */
class InterestDifferentialTest {
    private static final Path GOLDEN = Path.of(System.getProperty("golden.dir", "../golden"));
    private static final Path EXPECTED = GOLDEN.resolve("expected");

    @ParameterizedTest(name = "golden set {0}")
    @ValueSource(strings = {"mixed", "volume"})
    void matchesGoldenMasterToTheCent(String set) throws Exception {
        Path dir = EXPECTED.resolve(set);
        verifyAcceptedBaseline(set);
        assertTrue(Files.readString(dir.resolve("return-codes.json")).matches("(?s).*\"CBACT04C\":\\s*0\\b.*"),
                "golden CBACT04C return code must be 0 (INT-20)");

        String metadata = Files.readString(EXPECTED.resolve("metadata.json"));
        LocalDateTime pinned = LocalDateTime.parse(jsonString(metadata, "clock_utc"),
                DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        Clock clock = Clock.fixed(pinned.toInstant(ZoneOffset.UTC), ZoneOffset.UTC);
        String parmDate = jsonString(metadata, "parm_date");

        InterestInput input = new InterestInput(
                read(dir.resolve("CBTRN02C.TCATBALF.bin"), TranCatBalanceRecord.CODEC),
                read(dir.resolve("CBTRN02C.ACCTFILE.bin"), AccountRecord.CODEC),
                read(dir.resolve("CBTRN02C.XREFFILE.bin"), CardXrefRecord.CODEC),
                read(dir.resolve("CBTRN02C.DISCGRP.bin"), DisclosureGroupRecord.CODEC));
        InterestRun run = new InterestCalculator(clock, parmDate).run(input);

        List<String> mismatches = new ArrayList<>();
        List<String> summary = new ArrayList<>();
        summary.add(compare(set, "SYSTRAN", dir.resolve("SYSTRAN.bin"), run.transactions(),
                TransactionRecord.CODEC, TransactionRecord::id, mismatches));
        summary.add(compare(set, "ACCTFILE", dir.resolve("CBACT04C.ACCTFILE.bin"), run.accounts(),
                AccountRecord.CODEC, AccountRecord::acctId, mismatches));
        summary.add(compareSysout(set, dir.resolve("CBACT04C.sysout.txt"), run.sysout(), mismatches));
        for (String name : List.of("TCATBALF", "XREFFILE", "DISCGRP", "TRANFILE")) {
            byte[] before = Files.readAllBytes(dir.resolve("CBTRN02C." + name + ".bin"));
            byte[] after = Files.readAllBytes(dir.resolve("CBACT04C." + name + ".bin"));
            if (!Arrays.equals(before, after)) mismatches.add(set + " " + name + ": COBOL changed a file the port leaves untouched");
        }
        summary.add(String.format("[%s] untouched TCATBALF/XREFFILE/DISCGRP/TRANFILE: byte-identical before/after in the golden run", set));

        summary.forEach(System.out::println);
        mismatches.stream().limit(50).forEach(m -> System.out.println("MISMATCH " + m));
        assertEquals(0, mismatches.size(), () -> mismatches.size() + " mismatches; first: " + mismatches.get(0));
    }

    private static <T extends Record> String compare(String set, String file, Path expectedPath, List<T> actual,
            RecordCodec<T> codec, Function<T, String> key, List<String> mismatches) throws IOException {
        byte[] expectedBytes = Files.readAllBytes(expectedPath);
        List<T> expected = decode(expectedBytes, expectedPath, codec);
        int matched = 0;
        int fields = 0;
        if (expected.size() != actual.size()) {
            mismatches.add(String.format("%s %s: record count expected %d actual %d", set, file, expected.size(), actual.size()));
        }
        for (int i = 0; i < Math.min(expected.size(), actual.size()); i++) {
            T e = expected.get(i);
            T a = actual.get(i);
            boolean bytesEqual = Arrays.equals(
                    Arrays.copyOfRange(expectedBytes, i * codec.length(), (i + 1) * codec.length()), codec.encode(a));
            boolean fieldsEqual = true;
            for (RecordComponent c : e.getClass().getRecordComponents()) {
                fields++;
                Object ev = value(c, e);
                Object av = value(c, a);
                if (!ev.equals(av)) {
                    fieldsEqual = false;
                    mismatches.add(String.format("%s %s key=%s field=%s expected=[%s] actual=[%s]",
                            set, file, key.apply(e), c.getName(), ev, av));
                }
            }
            if (fieldsEqual && !bytesEqual) {
                mismatches.add(String.format("%s %s key=%s: fields equal but encoded bytes differ", set, file, key.apply(e)));
            }
            if (fieldsEqual && bytesEqual) matched++;
        }
        return String.format("[%s] %s: records compared %d, matched %d, mismatched %d (fields compared %d)",
                set, file, expected.size(), matched, expected.size() - matched, fields);
    }

    private static String compareSysout(String set, Path expectedPath, List<String> actual, List<String> mismatches)
            throws IOException {
        List<String> expected = Files.readAllLines(expectedPath, StandardCharsets.US_ASCII);
        int matched = 0;
        if (expected.size() != actual.size()) {
            mismatches.add(String.format("%s SYSOUT: line count expected %d actual %d", set, expected.size(), actual.size()));
        }
        for (int i = 0; i < Math.min(expected.size(), actual.size()); i++) {
            if (expected.get(i).equals(actual.get(i))) matched++;
            else mismatches.add(String.format("%s SYSOUT line=%d expected=[%s] actual=[%s]", set, i + 1, expected.get(i), actual.get(i)));
        }
        return String.format("[%s] SYSOUT: lines compared %d, matched %d, mismatched %d",
                set, expected.size(), matched, expected.size() - matched);
    }

    private static Object value(RecordComponent component, Record record) {
        try {
            return component.getAccessor().invoke(record);
        } catch (ReflectiveOperationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private static <T> List<T> read(Path path, RecordCodec<T> codec) throws IOException {
        return decode(Files.readAllBytes(path), path, codec);
    }

    private static <T> List<T> decode(byte[] bytes, Path path, RecordCodec<T> codec) {
        assertEquals(0, bytes.length % codec.length(), path + " is not a whole number of records");
        List<T> records = new ArrayList<>();
        for (int offset = 0; offset < bytes.length; offset += codec.length()) {
            records.add(codec.decode(Arrays.copyOfRange(bytes, offset, offset + codec.length())));
        }
        return records;
    }

    /** Guards against comparing with an edited baseline: every file used must match golden/acceptance.sha256. */
    private static void verifyAcceptedBaseline(String set) throws Exception {
        String manifest = Files.readString(GOLDEN.resolve("acceptance.sha256"));
        List<String> files = new ArrayList<>(List.of("metadata.json"));
        for (String name : List.of("TCATBALF", "ACCTFILE", "XREFFILE", "DISCGRP", "TRANFILE")) {
            files.add(set + "/CBTRN02C." + name + ".bin");
            files.add(set + "/CBACT04C." + name + ".bin");
        }
        files.addAll(List.of(set + "/SYSTRAN.bin", set + "/CBACT04C.sysout.txt", set + "/return-codes.json"));
        for (String file : files) {
            String digest = HexFormat.of().formatHex(
                    MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(EXPECTED.resolve(file))));
            assertTrue(manifest.contains(digest + "  modernization/java21/golden/expected/" + file + "\n"),
                    file + " does not match golden/acceptance.sha256");
        }
    }

    private static String jsonString(String json, String name) {
        Matcher m = Pattern.compile("\"" + Pattern.quote(name) + "\"\\s*:\\s*\"([^\"]*)\"").matcher(json);
        assertTrue(m.find(), "metadata.json lacks " + name);
        return m.group(1);
    }
}
