package com.carddemo.app.golden;

import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.Copybooks;
import com.carddemo.common.record.RecordFiles;
import com.carddemo.common.record.RecordLayout;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Oracle-vs-Java comparisons of the golden master (plan §3, drift rules D-018). Wave 1 runs the POSTTRAN job per
 * {@link GoldenCase} and calls the four {@code assert*} methods; each failure names the case, the artefact and the record.
 */
public final class ParityHarness {

    /** TRAN-PROC-TS centiseconds: TRANSACT record columns 325–326 (1-based) are not pinned by COB_CURRENT_DATE (R-05). */
    static final int PROC_TS_CENTIS_FROM = Copybooks.CVTRA05Y.field("TRAN-PROC-TS").offset() + 20;
    static final int PROC_TS_CENTIS_TO = PROC_TS_CENTIS_FROM + 2;

    private ParityHarness() {
    }

    /** DALYREJS: byte for byte. */
    public static void assertDalyrejs(GoldenCase c, Path actual) {
        assertThat(read(actual)).as("%s DALYREJS bytes", c).isEqualTo(read(c.expected("DALYREJS")));
    }

    /**
     * One table: the Java unload (fixed-width records, key order) against the oracle's line-sequential {@code .unl}
     * (trailing spaces re-padded to LRECL). Every data field is compared; FILLER bytes are masked on both sides (D-036:
     * not stored, and CBTRN02C leaves TCATBAL FILLER of a new record to WORKING-STORAGE history). TRANSACT also masks
     * the TRAN-PROC-TS centiseconds. The expected file is {@link GoldenCase#expectedUnload}: for an abend case it is the
     * atomic end state (D-042).
     */
    public static void assertTable(GoldenCase c, Ksds ksds, List<String> actualRecords) {
        List<String> expected = normalize(ksds, RecordFiles.readLines(c.expectedUnload(ksds), ksds.recordLength(), CobolCharsets.ASCII));
        List<String> actual = normalize(ksds, actualRecords);
        assertThat(actual).as("%s %s record count", c, ksds).hasSameSizeAs(expected);
        for (int i = 0; i < expected.size(); i++) {
            assertThat(actual.get(i)).as("%s %s record %d", c, ksds, i + 1).isEqualTo(expected.get(i));
        }
    }

    /** SYSOUT: same lines in the same order, after dropping oracle-runtime lines that are not CBTRN02C DISPLAYs. */
    public static void assertSysout(GoldenCase c, List<String> actualLines) {
        assertThat(programLines(actualLines)).as("%s SYSOUT", c).containsExactlyElementsOf(programLines(readLines(c.expected("SYSOUT"))));
    }

    /** Exit code: 0 and 4 identical, oracle 231 (U999) ≡ Java 16 ({@link OracleReturnCode}). */
    public static void assertReturnCode(GoldenCase c, int javaRc) {
        assertThat(javaRc).as("%s RC (oracle %d)", c, c.oracleReturnCode()).isEqualTo(OracleReturnCode.toJava(c.oracleReturnCode()));
    }

    /**
     * Drops lines the GnuCOBOL harness prints but CBTRN02C does not: the CEE3ABD stub's {@code CEE3ABD ABCODE=...} and
     * libcob runtime warnings (e.g. {@code libcob: warning: implicit CLOSE of ...} after the abend).
     */
    static List<String> programLines(List<String> lines) {
        List<String> out = new ArrayList<>();
        for (String l : lines) {
            if (!l.startsWith("CEE3ABD ABCODE=") && !l.startsWith("libcob: ")) {
                out.add(l.stripTrailing());
            }
        }
        return out;
    }

    static List<String> normalize(Ksds ksds, List<String> records) {
        List<String> out = new ArrayList<>(records.size());
        for (String r : records) {
            assertThat(r).as("%s record length", ksds).hasSize(ksds.recordLength());
            StringBuilder masked = new StringBuilder(r);
            for (RecordLayout.Field f : ksds.layout().fields()) {
                if (f.kind() == RecordLayout.Kind.FILLER) {
                    masked.replace(f.offset(), f.end(), "~".repeat(f.length()));
                }
            }
            if (ksds == Ksds.TRANSACT) {
                masked.replace(PROC_TS_CENTIS_FROM, PROC_TS_CENTIS_TO, "##");
            }
            out.add(masked.toString());
        }
        return out;
    }

    private static byte[] read(Path p) {
        try {
            return Files.readAllBytes(p);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static List<String> readLines(Path p) {
        try {
            return Files.readAllLines(p, CobolCharsets.ASCII);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
