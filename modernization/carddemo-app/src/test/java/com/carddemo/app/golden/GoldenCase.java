package com.carddemo.app.golden;

import com.carddemo.common.batch.ReturnCode;
import com.carddemo.common.cobol.CodePage;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/**
 * One POSTTRAN golden case under {@code src/test/resources/golden/POSTTRAN/GC-<n>}: {@code input/} (DALYTRAN and an
 * optional custom CARDXREF) and {@code expected/}, the GnuCOBOL oracle outputs of
 * {@code .migration/tools/posttran_gnucobol_oracle.sh}. Abend cases also carry {@code expected-prefix/}, the oracle run on
 * the records before the failing one (runbook R-13).
 */
public record GoldenCase(String name, Path dir) {

    public static final Path ROOT = Path.of("src", "test", "resources", "golden", "POSTTRAN");

    /** Oracle RC of an abend: CEE3ABD ABCODE 999 as an exit status (999 mod 256). */
    public static final int ORACLE_ABEND_RC = 231;

    public static GoldenCase of(String name) {
        Path dir = ROOT.resolve(name);
        if (!Files.isDirectory(dir)) {
            throw new IllegalArgumentException("no golden case " + dir);
        }
        return new GoldenCase(name, dir);
    }

    public static List<GoldenCase> all() {
        try (var dirs = Files.list(ROOT)) {
            return dirs.filter(Files::isDirectory).map(p -> new GoldenCase(p.getFileName().toString(), p)).sorted(
                    (a, b) -> a.name().compareTo(b.name())).toList();
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    public Path dalytran() {
        Path ps = dir.resolve("input/DALYTRAN.PS");
        return Files.exists(ps) ? ps : dir.resolve("input/DALYTRAN.txt");
    }

    /** {@code .PS} inputs are EBCDIC RECFM=FB (GC-7), {@code .txt} inputs ASCII line-sequential. */
    public CodePage encoding() {
        return dalytran().getFileName().toString().endsWith(".PS") ? CodePage.CP037 : CodePage.ASCII;
    }

    public Optional<Path> customCardXref() {
        Path xref = dir.resolve("input/CARDXREF.txt");
        return Files.exists(xref) ? Optional.of(xref) : Optional.empty();
    }

    public Path expected(String file) {
        return dir.resolve("expected").resolve(file);
    }

    public int oracleReturnCode() {
        return Integer.parseInt(read(expected("RETURN-CODE")).strip());
    }

    public boolean abend() {
        return oracleReturnCode() == ORACLE_ABEND_RC;
    }

    /** RC the Java job must end with: the oracle RC, or 16 for an oracle abend. */
    public int expectedJavaReturnCode() {
        return abend() ? ReturnCode.ABEND : oracleReturnCode();
    }

    /**
     * Expected table unload for the parity check. Normal end: the oracle run on the same input. Abend (GC-4 rule):
     * the oracle run on the valid prefix, because COBOL leaves the failing record's updates applied and Java rolls
     * them back (D-009).
     */
    public Path expectedTable(String unload) {
        return abend() ? dir.resolve("expected-prefix").resolve(unload) : expected(unload);
    }

    /** DALYREJS is compared only on normal end; an abend publishes none (D-012). */
    public Optional<Path> expectedDalyrejs() {
        return abend() ? Optional.empty() : Optional.of(expected("DALYREJS"));
    }

    static String read(Path file) {
        try {
            return Files.readString(file, CodePage.ASCII.charset());
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }
}
