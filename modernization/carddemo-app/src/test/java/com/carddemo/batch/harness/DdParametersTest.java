package com.carddemo.batch.harness;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.RecordPrefix;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;

class DdParametersTest {

    private static final Path OUT = Path.of("/batch-output");

    @Test
    void ddDefaultsToTableAndOutputsToTheOutputDirectory() {
        JobParameters none = new JobParameters();
        assertThat(DdParameters.isTable(none, "ACCTFILE")).isTrue();
        assertThat(DdParameters.output(none, "OUTFILE", OUT, "AWS.M2.X")).isEqualTo(OUT.resolve("AWS.M2.X"));
        assertThat(DdParameters.sysout(none, OUT, "readacct", 7)).isEqualTo(OUT.resolve("SYSOUT/readacct.undated.7.txt"));
        assertThat(DdParameters.encoding(none)).isEqualTo(RecordEncoding.EBCDIC);
        assertThat(DdParameters.recordPrefix(none)).isEqualTo(RecordPrefix.ZOS_RDW);
    }

    @Test
    void ddParametersNameFilesAndFormats() {
        JobParameters p = new JobParametersBuilder().addString("ACCTFILE", "/in/acct.txt")
                .addString("OUTFILE", "/out/o").addString("SYSOUT", "/out/sysout.txt")
                .addString("encoding", "ascii").addString("record-prefix", "gnucobol_varseq_0").toJobParameters();
        assertThat(DdParameters.isTable(p, "ACCTFILE")).isFalse();
        assertThat(DdParameters.path(p, "ACCTFILE")).isEqualTo(Path.of("/in/acct.txt"));
        assertThat(DdParameters.output(p, "OUTFILE", OUT, "X")).isEqualTo(Path.of("/out/o"));
        assertThat(DdParameters.sysout(p, OUT, "j", 1)).isEqualTo(Path.of("/out/sysout.txt"));
        assertThat(DdParameters.encoding(p)).isEqualTo(RecordEncoding.ASCII);
        assertThat(DdParameters.recordPrefix(p)).isEqualTo(RecordPrefix.GNUCOBOL_VARSEQ_0);
        assertThat(DdParameters.isTable(new JobParametersBuilder().addString("ACCTFILE", "TABLE").toJobParameters(),
                "ACCTFILE")).isTrue();
    }

    @Test
    void invalidFormatsAreRejected() {
        assertThatThrownBy(() -> DdParameters.encoding(
                new JobParametersBuilder().addString("encoding", "UTF-8").toJobParameters()))
                .hasMessageContaining("EBCDIC or ASCII");
        assertThatThrownBy(() -> DdParameters.recordPrefix(
                new JobParametersBuilder().addString("record-prefix", "VB").toJobParameters()))
                .hasMessageContaining("--record-prefix");
    }
}
