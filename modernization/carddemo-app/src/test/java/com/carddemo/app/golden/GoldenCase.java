package com.carddemo.app.golden;

import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.common.cobol.CobolCharsets;

import java.io.IOException;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;
import java.util.stream.Stream;

/**
 * One oracle golden case under {@code src/test/resources/golden/posttran/<name>}, built by
 * {@code .migration/tools/posttran_golden_cases.sh} (runbook R-16).
 */
public record GoldenCase(String name, Path dir, Charset dalytranCharset, String oracleNow) {

    public static final Path ROOT = Path.of("src/test/resources/golden/posttran");
    /** Default POSTTRAN masters, the same files the oracle loads (R-04). */
    public static final Path SAMPLE_DATA = Path.of("../../app/data/ASCII");

    public static List<GoldenCase> all() {
        try (Stream<Path> dirs = Files.list(ROOT)) {
            return dirs.filter(d -> Files.isRegularFile(d.resolve("case.properties"))).sorted().map(GoldenCase::load).toList();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static GoldenCase named(String name) {
        return load(ROOT.resolve(name));
    }

    private static GoldenCase load(Path dir) {
        Properties p = new Properties();
        try (Reader r = Files.newBufferedReader(dir.resolve("case.properties"))) {
            p.load(r);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return new GoldenCase(dir.getFileName().toString(), dir,
                CobolCharsets.forName(p.getProperty("dalytran.charset", "ISO-8859-1")), p.getProperty("oracle.now"));
    }

    /** The exact DALYTRAN bytes CBTRN02C read. */
    public Path dalytran() {
        return dir.resolve("input/DALYTRAN");
    }

    /** Line-sequential master the oracle loaded for {@code ksds}: the case override, else {@code app/data/ASCII}. */
    public Path master(Ksds ksds) {
        Path override = dir.resolve("input").resolve(ksds.sampleFile());
        return Files.isRegularFile(override) ? override : SAMPLE_DATA.resolve(ksds.sampleFile());
    }

    public Path expected(String file) {
        return dir.resolve("expected").resolve(file);
    }

    /** {@code expected/TRANSACT.unl}, {@code ACCTDATA.unl} or {@code TCATBALF.unl}. */
    public Path expectedUnload(Ksds ksds) {
        return expected(ksds.name() + ".unl");
    }

    public int oracleReturnCode() {
        try {
            return Integer.parseInt(Files.readString(expected("RETURN-CODE")).trim());
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public String toString() {
        return name;
    }
}
