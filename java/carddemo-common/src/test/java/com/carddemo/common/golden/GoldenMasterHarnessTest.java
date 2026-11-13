package com.carddemo.common.golden;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.copybook.RecordLayout;
import com.carddemo.common.copybook.RecordTranscoder;
import com.carddemo.common.copybook.RecordView;
import com.carddemo.common.testing.ComparisonReport;
import com.carddemo.common.testing.GoldenMaster;
import com.carddemo.common.testing.KnownDifference;
import com.carddemo.common.testing.MainframeSources;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.opentest4j.AssertionFailedError;

import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/** Self-tests of the golden-master harness behaviour. */
class GoldenMasterHarnessTest {

    @TempDir
    Path dir;

    private final RecordLayout layout = MainframeSources.locate().layout("CVTRA01Y");

    private byte[] record(String amount, DataEncoding encoding) {
        RecordView v = RecordView.initialized(layout, encoding);
        v.setLong("TRANCAT-ACCT-ID", 1);
        v.setString("TRANCAT-TYPE-CD", "01");
        v.setLong("TRANCAT-CD", 1);
        v.setDecimal("TRAN-CAT-BAL", new BigDecimal(amount));
        return v.bytes();
    }

    @Test
    void comparesDecodedValuesAcrossEncodings() {
        GoldenMaster golden = new GoldenMaster(dir);
        ComparisonReport report = golden.compare("same", layout,
                List.of(record("10.00", DataEncoding.EBCDIC)), DataEncoding.EBCDIC,
                List.of(record("10.00", DataEncoding.ASCII)), DataEncoding.ASCII);
        assertThat(report.matches()).isTrue();
    }

    @Test
    void reportsFieldDifferencesAndRecordCounts() {
        GoldenMaster golden = new GoldenMaster(dir);
        ComparisonReport report = golden.compare("diff", layout,
                List.of(record("10.00", DataEncoding.EBCDIC), record("1", DataEncoding.EBCDIC)), DataEncoding.EBCDIC,
                List.of(record("-10.00", DataEncoding.EBCDIC)), DataEncoding.EBCDIC);
        assertThat(report.matches()).isFalse();
        assertThat(report.differences()).singleElement()
                .satisfies(d -> {
                    assertThat(d.field()).isEqualTo("TRAN-CAT-BAL");
                    assertThat(d.expected()).isEqualTo("10.00");
                    assertThat(d.actual()).isEqualTo("-10.00");
                });
        assertThatThrownBy(() -> golden.assertMatches(report)).isInstanceOf(AssertionFailedError.class)
                .hasMessageContaining("expected 2 records but got 1");
    }

    @Test
    void knownDifferencesMustBeDeclaredAndMustStillOccur() {
        GoldenMaster golden = new GoldenMaster(dir);
        List<byte[]> expected = List.of(record("10.00", DataEncoding.EBCDIC));
        List<byte[]> actual = List.of(record("11.00", DataEncoding.EBCDIC));
        KnownDifference accepted = new KnownDifference(1, "TRAN-CAT-BAL", "rounding fix");
        golden.assertMatches("known", layout, expected, DataEncoding.EBCDIC, actual, DataEncoding.EBCDIC, accepted);
        assertThatThrownBy(() -> golden.assertMatches("stale", layout, expected, DataEncoding.EBCDIC,
                expected, DataEncoding.EBCDIC, accepted))
                .hasMessageContaining("not observed");
    }

    @Test
    void detectsFillerDifferences() {
        GoldenMaster golden = new GoldenMaster(dir);
        byte[] a = record("1", DataEncoding.EBCDIC);
        byte[] b = a.clone();
        b[49] = 0x00;
        assertThat(golden.compare("filler", layout, List.of(a), DataEncoding.EBCDIC, List.of(b), DataEncoding.EBCDIC)
                .differences()).extracting(d -> d.field()).containsExactly("FILLER");
        byte[] ascii = RecordTranscoder.transcode(a, layout, DataEncoding.EBCDIC, DataEncoding.ASCII);
        assertThat(golden.compare("filler", layout, List.of(a), DataEncoding.EBCDIC, List.of(ascii), DataEncoding.ASCII)
                .matches()).isTrue();
    }

    @Test
    void snapshotLifecycle() throws Exception {
        GoldenMaster golden = new GoldenMaster(dir);
        assertThatThrownBy(() -> golden.assertMatchesSnapshot("report", List.of("a", "b")))
                .hasMessageContaining("did not exist and was created");
        assertThat(Files.readString(dir.resolve("report.golden"))).isEqualTo("a\nb\n");
        golden.assertMatchesSnapshot("report", List.of("a", "b"));
        assertThatThrownBy(() -> golden.assertMatchesSnapshot("report", List.of("a", "c")))
                .hasMessageContaining("at line 2");
    }
}
