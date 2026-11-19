package com.carddemo.app;

import com.carddemo.app.data.KsdsDataService;
import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.RecordFiles;
import org.assertj.core.api.Assertions;
import org.assertj.core.api.SoftAssertions;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.stream.Stream;


/**
 * Independent CBTRN02C parity pass (Wave 1, {@code functional/CARDDEMO/programs/CBTRN02C_parity_results.md}). Every case
 * under {@code parity/cbtrn02c/<case>} was produced by {@code .migration/tools/cbtrn02c_parity_oracle.sh}: inputs from
 * {@code cbtrn02c_parity_cases.py}, expected outputs straight from GnuCOBOL 3.1 running {@code app/cbl/CBTRN02C.cbl}.
 * The Java side is the real CLI ({@link CardDemoApplication#launch}) on PostgreSQL 16. One normalisation for both sides:
 * FILLER bytes and the TRAN-PROC-TS centiseconds (TRANSACT columns 325-326) are masked; SYSOUT drops the GnuCOBOL-only
 * {@code CEE3ABD ABCODE=} and {@code libcob:} lines and trailing blanks; RC 231 (U999) is read as 16 (D-015, D-039).
 */
@SpringBootTest
class Cbtrn02cIndependentParityTest extends PostgresContainerSupport {

    static final Path ROOT = Path.of("src/test/resources/parity/cbtrn02c");
    static final Path SAMPLE = Path.of("../../app/data/ASCII");
    static final String STALE = "STALE-PARITY-001";
    /** Verbatim Java outputs per case (RC, DALYREJS, SYSOUT, table unloads) for the results document. */
    static final Path ACTUAL = Path.of("target/parity-actual/cbtrn02c");

    @Autowired
    KsdsDataService data;

    /** Every artefact of a case is compared and reported, not only the first mismatch. */
    final SoftAssertions soft = new SoftAssertions();

    record Case(String name, Path dir, Properties p) {
        String kind() {
            return p.getProperty("kind");
        }

        Path file(String sub, String name) {
            return dir.resolve(sub).resolve(name);
        }

        @Override
        public String toString() {
            return name;
        }
    }

    static List<Case> cases() throws IOException {
        try (Stream<Path> dirs = Files.list(ROOT)) {
            return dirs.filter(d -> Files.isRegularFile(d.resolve("case.properties"))).sorted().map(d -> {
                Properties p = new Properties();
                try (Reader r = Files.newBufferedReader(d.resolve("case.properties"))) {
                    p.load(r);
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
                return new Case(d.getFileName().toString(), d, p);
            }).toList();
        }
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("cases")
    void javaPosttranMatchesGnuCobol(Case c, @TempDir Path out) throws IOException {
        seed(c);
        switch (c.kind()) {
            case "single" -> {
                Path rejs = out.resolve("DALYREJS");
                int rc = launch(c, c.file("input", "DALYTRAN"), rejs, out.resolve("SYSOUT"), "PARITY-" + c.name());
                assertRun(c, "expected", rc, rejs, out.resolve("SYSOUT"));
                assertTables(c, c.p().getProperty("tables.expected", "expected"));
            }
            case "rerun" -> {
                int rc1 = launch(c, c.file("input", "DALYTRAN"), out.resolve("R1"), out.resolve("S1"), "PARITY-RERUN-1");
                assertRun(c, "expected-run1", rc1, out.resolve("R1"), out.resolve("S1"));
                assertTables(c, "expected-run1");
                int rc2 = launch(c, c.file("input", "DALYTRAN"), out.resolve("R2"), out.resolve("S2"), "PARITY-RERUN-2");
                assertRun(c, "expected", rc2, out.resolve("R2"), out.resolve("S2"));
                assertTables(c, "expected");
            }
            case "restart" -> {
                Path dalytran = out.resolve("DALYTRAN");
                Files.copy(c.file("input", "DALYTRAN.leg1"), dalytran);
                int rc1 = launch(c, dalytran, out.resolve("DALYREJS"), out.resolve("SYSOUT.leg1"), "PARITY-RESTART");
                assertRun(c, "expected-leg1", rc1, out.resolve("DALYREJS"), out.resolve("SYSOUT.leg1"));
                Files.copy(c.file("input", "DALYTRAN"), dalytran, StandardCopyOption.REPLACE_EXISTING);
                int rc2 = launch(c, dalytran, out.resolve("DALYREJS"), out.resolve("SYSOUT"), "PARITY-RESTART");
                Path cap = Files.createDirectories(ACTUAL.resolve(c.name()).resolve("expected"));
                Files.writeString(cap.resolve("RETURN-CODE"), rc2 + "\n");
                Files.copy(out.resolve("SYSOUT"), cap.resolve("SYSOUT"), StandardCopyOption.REPLACE_EXISTING);
                Files.copy(out.resolve("DALYREJS"), cap.resolve("DALYREJS"), StandardCopyOption.REPLACE_EXISTING);
                soft.assertThat(rc2).as("%s RC", c).isEqualTo(oracleRc(c, "expected"));
                soft.assertThat(Files.readAllBytes(out.resolve("DALYREJS"))).as("%s DALYREJS", c)
                        .isEqualTo(Files.readAllBytes(c.file("expected", "DALYREJS")));
                assertTables(c, "expected");
                List<String> exp = programLines(c.file("expected", "SYSOUT"));
                List<String> act = programLines(out.resolve("SYSOUT"));
                soft.assertThat(act.subList(Math.max(0, act.size() - 3), act.size())).as("%s SYSOUT counters", c)
                        .containsExactlyElementsOf(exp.subList(exp.size() - 3, exp.size()));
            }
            case "missing" -> {
                Path rejs = out.resolve("DALYREJS");
                int rc = launch(c, out.resolve("NO-SUCH-DALYTRAN"), rejs, out.resolve("SYSOUT"), "PARITY-" + c.name());
                Path cap = Files.createDirectories(ACTUAL.resolve(c.name()).resolve("expected"));
                Files.writeString(cap.resolve("RETURN-CODE"), rc + "\n");
                Files.copy(out.resolve("SYSOUT"), cap.resolve("SYSOUT"), StandardCopyOption.REPLACE_EXISTING);
                Files.write(cap.resolve("TRANSACT.unl"), data.unload(Ksds.TRANSACT), CobolCharsets.ASCII);
                soft.assertThat(rc).as("%s RC", c).isEqualTo(oracleRc(c, "expected"));
                soft.assertThat(programLines(out.resolve("SYSOUT"))).as("%s SYSOUT", c)
                        .containsExactlyElementsOf(programLines(c.file("expected", "SYSOUT")));
                soft.assertThat(rejs).as("%s DALYREJS (never opened by the oracle)", c).doesNotExist();
                soft.assertThat(data.unload(Ksds.TRANSACT)).as("%s TRANSACT untouched (OPEN OUTPUT never reached)", c)
                        .anyMatch(r -> r.startsWith(STALE));
            }
            default -> throw new IllegalStateException(c.kind());
        }
        String divergence = c.p().getProperty("divergence.artifact");
        if (divergence == null) {
            soft.assertAll();
        } else {
            // Documented FAIL (CBTRN02C_parity_results.md, D-051): the mismatch must stay confined to that artefact,
            // and this case turns red once the implementation is changed so the divergence disappears.
            Assertions.assertThat(soft.assertionErrorsCollected()).as("%s divergence %s", c, c.p().getProperty("divergence.id"))
                    .isNotEmpty().allMatch(e -> e.getMessage().contains(divergence + "]"));
        }
    }

    /** Sample masters with the case's overrides, plus one stale TRANSACT row that a new run must clear. */
    void seed(Case c) {
        data.deleteAll();
        for (Ksds k : List.of(Ksds.ACCTDATA, Ksds.CARDXREF, Ksds.TCATBALF)) {
            Path f = c.file("input", k.sampleFile());
            data.load(k, RecordFiles.readLines(Files.isRegularFile(f) ? f : SAMPLE.resolve(k.sampleFile()),
                    k.recordLength(), CobolCharsets.ASCII));
        }
        String posted = RecordFiles.readLines(ROOT.resolve("baseline/expected/TRANSACT.unl"), Ksds.TRANSACT.recordLength(),
                CobolCharsets.ASCII).get(0);
        data.load(Ksds.TRANSACT, List.of(STALE + posted.substring(STALE.length())));
    }

    static int launch(Case c, Path dalytran, Path dalyrejs, Path sysout, String runId) {
        List<String> args = new ArrayList<>(List.of("--job=POSTTRAN", "--run-id=" + runId, "--dd=DALYTRAN=" + dalytran,
                "--dd=DALYREJS=" + dalyrejs, "--dd=SYSOUT=" + sysout,
                "--carddemo.batch.clock=" + LocalDateTime.parse(c.p().getProperty("oracle.now"),
                        DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss")),
                "--carddemo.posttran.dalytran-encoding=" + c.p().getProperty("dalytran.charset")));
        args.addAll(List.of(datasourceArgs()));
        return CardDemoApplication.launch(args.toArray(String[]::new),
                new PrintStream(new ByteArrayOutputStream(), true, StandardCharsets.UTF_8));
    }

    void assertRun(Case c, String expectedDir, int rc, Path dalyrejs, Path sysout) throws IOException {
        Path cap = Files.createDirectories(ACTUAL.resolve(c.name()).resolve(expectedDir));
        Files.writeString(cap.resolve("RETURN-CODE"), rc + "\n");
        Files.copy(sysout, cap.resolve("SYSOUT"), StandardCopyOption.REPLACE_EXISTING);
        if (Files.exists(dalyrejs)) {
            Files.copy(dalyrejs, cap.resolve("DALYREJS"), StandardCopyOption.REPLACE_EXISTING);
        }
        soft.assertThat(rc).as("%s %s RC", c, expectedDir).isEqualTo(oracleRc(c, expectedDir));
        soft.assertThat(Files.readAllBytes(dalyrejs)).as("%s %s DALYREJS", c, expectedDir)
                .isEqualTo(Files.readAllBytes(c.file(expectedDir, "DALYREJS")));
        soft.assertThat(programLines(sysout)).as("%s %s SYSOUT", c, expectedDir)
                .containsExactlyElementsOf(programLines(c.file(expectedDir, "SYSOUT")));
    }

    void assertTables(Case c, String expectedDir) throws IOException {
        Path cap = Files.createDirectories(ACTUAL.resolve(c.name()).resolve(expectedDir));
        for (Ksds k : List.of(Ksds.ACCTDATA, Ksds.TCATBALF, Ksds.TRANSACT)) {
            Files.write(cap.resolve(k.name() + ".unl"), data.unload(k), CobolCharsets.ASCII);
            List<String> exp = mask(k, RecordFiles.readLines(c.file(expectedDir, k.name() + ".unl"), k.recordLength(), CobolCharsets.ASCII));
            List<String> act = mask(k, data.unload(k));
            soft.assertThat(act).as("%s %s %s", c, expectedDir, k).containsExactlyElementsOf(exp);
        }
    }

    static int oracleRc(Case c, String expectedDir) throws IOException {
        int rc = Integer.parseInt(Files.readString(c.file(expectedDir, "RETURN-CODE")).trim());
        return rc == 231 ? 16 : rc;
    }

    /** FILLER columns from the copybooks: CVACT01Y 123-300, CVTRA01Y 29-50, CVTRA05Y 331-350 (+ TRAN-PROC-TS 325-326). */
    static List<String> mask(Ksds k, List<String> records) {
        List<String> out = new ArrayList<>();
        for (String r : records) {
            StringBuilder b = new StringBuilder(r);
            switch (k) {
                case ACCTDATA -> b.replace(122, 300, "~".repeat(178));
                case TCATBALF -> b.replace(28, 50, "~".repeat(22));
                case TRANSACT -> b.replace(324, 326, "##").replace(330, 350, "~".repeat(20));
                default -> {
                }
            }
            out.add(b.toString());
        }
        return out;
    }

    static List<String> programLines(Path sysout) throws IOException {
        return Files.readAllLines(sysout, CobolCharsets.ASCII).stream()
                .filter(l -> !l.startsWith("CEE3ABD ABCODE=") && !l.startsWith("libcob: ")).map(String::stripTrailing).toList();
    }
}
