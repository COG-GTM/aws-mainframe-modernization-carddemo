package com.carddemo.modernization.batch;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** One golden scenario under {@code golden/expected/<set>/}, staged as JCL datasets and compared byte for byte. */
record GoldenSet(Path golden, String name) {
    static final List<String> MASTERS = List.of("ACCTFILE", "DISCGRP", "TCATBALF", "TRANFILE", "XREFFILE");
    static final List<String> RUN_OUTPUTS = List.of("DALYREJS.bin", "SYSTRAN.bin", "CBTRN02C.sysout.txt",
            "CBACT04C.sysout.txt");

    static Path defaultGolden() {
        return Path.of(System.getProperty("golden.dir", "../golden"));
    }

    Path expected() {
        return golden.resolve("expected").resolve(name);
    }

    /** Copies DALYTRAN and the initial master files to {@code <datasets>/<DD>.bin}. */
    void stage(Path datasets) throws IOException {
        Files.createDirectories(datasets);
        Files.copy(expected().resolve("DALYTRAN.bin"), datasets.resolve("DALYTRAN.bin"));
        for (String dd : MASTERS) {
            Files.copy(expected().resolve("initial." + dd + ".bin"), datasets.resolve(dd + ".bin"));
        }
    }

    /** {@code --carddemo.*} options and job parameters for a run pinned to the golden clock and PARM. */
    List<String> arguments(Path datasets, Path runRoot, String runId) throws IOException {
        String metadata = Files.readString(golden.resolve("expected/metadata.json"));
        String clock = jsonString(metadata, "clock_utc").replace(' ', 'T') + "Z";
        return List.of("--carddemo.datasets-dir=" + datasets, "--carddemo.run-root=" + runRoot,
                "--carddemo.clock.fixed-instant=" + clock, "--carddemo.clock.zone=UTC",
                "runId=" + runId, "parmDate=" + jsonString(metadata, "parm_date"));
    }

    /** JCL condition code of the job: the highest step return code in {@code return-codes.json}. */
    int expectedExitCode() throws IOException {
        Matcher m = Pattern.compile("\"(CBTRN02C|CBACT04C)\"\\s*:\\s*(\\d+)")
                .matcher(Files.readString(expected().resolve("return-codes.json")));
        int max = 0;
        int steps = 0;
        while (m.find()) {
            max = Math.max(max, Integer.parseInt(m.group(2)));
            steps++;
        }
        assertEquals(2, steps, "return-codes.json must list CBTRN02C and CBACT04C");
        return max;
    }

    /**
     * Every file the job writes, mapped to its golden counterpart. CBACT04C only reads TCATBALF, TRANFILE, XREFFILE
     * and DISCGRP, so their final state is also the CBTRN02C snapshot; ACCTFILE is rewritten by both steps.
     */
    Map<String, Path> comparisons(Path datasets, Path runDir) {
        Map<String, Path> files = new LinkedHashMap<>();
        for (String output : RUN_OUTPUTS) {
            files.put(output, runDir.resolve(output));
        }
        for (String dd : MASTERS) {
            files.put("CBACT04C." + dd + ".bin", datasets.resolve(dd + ".bin"));
            if (!dd.equals("ACCTFILE")) {
                files.put("CBTRN02C." + dd + ".bin", datasets.resolve(dd + ".bin"));
            }
        }
        return files;
    }

    /** Asserts byte equality and returns one summary line per file. */
    List<String> assertIdentical(Map<String, Path> comparisons) throws IOException {
        verifyAccepted(new ArrayList<>(comparisons.keySet()));
        List<String> summary = new ArrayList<>();
        for (Map.Entry<String, Path> entry : comparisons.entrySet()) {
            byte[] expected = Files.readAllBytes(expected().resolve(entry.getKey()));
            assertTrue(Files.exists(entry.getValue()), entry.getValue() + " was not written");
            assertArrayEquals(expected, Files.readAllBytes(entry.getValue()), name + "/" + entry.getKey());
            summary.add(name + "/" + entry.getKey() + " identical (" + expected.length + " bytes)");
        }
        return summary;
    }

    /** Guards against comparing with an edited baseline: every file used must match golden/acceptance.sha256. */
    private void verifyAccepted(List<String> files) throws IOException {
        String manifest = Files.readString(golden.resolve("acceptance.sha256"));
        List<String> used = new ArrayList<>(List.of("metadata.json", name + "/DALYTRAN.bin", name + "/return-codes.json"));
        MASTERS.forEach(dd -> used.add(name + "/initial." + dd + ".bin"));
        files.forEach(file -> used.add(name + "/" + file));
        for (String file : used) {
            String digest = HexFormat.of().formatHex(sha256(Files.readAllBytes(golden.resolve("expected").resolve(file))));
            assertTrue(manifest.contains(digest + "  modernization/java21/golden/expected/" + file + "\n"),
                    file + " does not match golden/acceptance.sha256");
        }
    }

    private static byte[] sha256(byte[] bytes) {
        try {
            return MessageDigest.getInstance("SHA-256").digest(bytes);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    private static String jsonString(String json, String key) {
        Matcher m = Pattern.compile("\"" + Pattern.quote(key) + "\"\\s*:\\s*\"([^\"]*)\"").matcher(json);
        assertTrue(m.find(), "metadata.json lacks " + key);
        return m.group(1);
    }
}
