package com.carddemo.app.golden;

import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.common.batch.ReturnCode;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.RecordFiles;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class ParityHarnessTest {

    @Test
    void goldenCasesAreTheOnesOfThePlanWithTheOracleReturnCodes() {
        assertThat(GoldenCase.all()).extracting(GoldenCase::name)
                .containsExactly("acct-missing", "baseline", "cp037", "dup-id", "empty", "syn-all", "syn-ok");
        assertThat(GoldenCase.all()).extracting(GoldenCase::oracleReturnCode).containsExactly(4, 4, 4, 231, 0, 4, 0);
        assertThat(GoldenCase.named("cp037").dalytranCharset()).isEqualTo(CobolCharsets.EBCDIC);
        assertThat(GoldenCase.named("acct-missing").master(Ksds.ACCTDATA)).startsWith(GoldenCase.ROOT);
        assertThat(GoldenCase.named("baseline").master(Ksds.ACCTDATA)).startsWith(GoldenCase.SAMPLE_DATA);
    }

    @Test
    void oracleReturnCode231IsJava16AndZeroAndFourAreIdentical() {
        assertThat(OracleReturnCode.GNUCOBOL_U999).isEqualTo(231);
        assertThat(OracleReturnCode.toJava(0)).isEqualTo(ReturnCode.OK);
        assertThat(OracleReturnCode.toJava(4)).isEqualTo(ReturnCode.WARNING);
        assertThat(OracleReturnCode.toJava(231)).isEqualTo(ReturnCode.ABEND);
        assertThatThrownBy(() -> OracleReturnCode.toJava(16)).hasMessageContaining("no Java equivalent");
        assertThatThrownBy(() -> OracleReturnCode.toJava(12)).hasMessageContaining("no Java equivalent");

        ParityHarness.assertReturnCode(GoldenCase.named("dup-id"), 16);
        ParityHarness.assertReturnCode(GoldenCase.named("baseline"), 4);
        ParityHarness.assertReturnCode(GoldenCase.named("empty"), 0);
        assertThatThrownBy(() -> ParityHarness.assertReturnCode(GoldenCase.named("dup-id"), 231)).isInstanceOf(AssertionError.class);
        assertThatThrownBy(() -> ParityHarness.assertReturnCode(GoldenCase.named("baseline"), 0)).isInstanceOf(AssertionError.class);
    }

    @Test
    void transactComparisonMasksOnlyTheProcTsCentiseconds() {
        GoldenCase c = GoldenCase.named("syn-all");
        List<String> oracle = RecordFiles.readLines(c.expectedUnload(Ksds.TRANSACT), 350, CobolCharsets.ASCII);
        List<String> centis = new ArrayList<>(oracle);
        String r = centis.get(0);
        assertThat(r.substring(324, 326)).matches("\\d\\d");
        centis.set(0, r.substring(0, 324) + (r.charAt(324) == '9' ? "00" : "99") + r.substring(326));
        ParityHarness.assertTable(c, Ksds.TRANSACT, centis);

        for (int column : new int[] {323, 326}) {
            List<String> drift = new ArrayList<>(oracle);
            drift.set(0, r.substring(0, column) + (r.charAt(column) == '7' ? '8' : '7') + r.substring(column + 1));
            assertThatThrownBy(() -> ParityHarness.assertTable(c, Ksds.TRANSACT, drift)).isInstanceOf(AssertionError.class);
        }
        assertThatThrownBy(() -> ParityHarness.assertTable(c, Ksds.TRANSACT, oracle.subList(0, 2))).hasMessageContaining("record count");
    }

    @Test
    void tablesOtherThanTransactAreComparedExactly() {
        GoldenCase c = GoldenCase.named("baseline");
        List<String> oracle = RecordFiles.readLines(c.expectedUnload(Ksds.TCATBALF), 50, CobolCharsets.ASCII);
        ParityHarness.assertTable(c, Ksds.TCATBALF, oracle);
        List<String> drift = new ArrayList<>(oracle);
        String last = drift.get(99);
        drift.set(99, last.substring(0, 27) + (last.charAt(27) == 'A' ? 'B' : 'A') + last.substring(28));
        assertThatThrownBy(() -> ParityHarness.assertTable(c, Ksds.TCATBALF, drift)).hasMessageContaining("record 100");
        List<String> filler = new ArrayList<>(oracle);
        filler.set(99, last.substring(0, 28) + " ".repeat(22));
        ParityHarness.assertTable(c, Ksds.TCATBALF, filler);
    }

    @Test
    void sysoutIgnoresOnlyOracleRuntimeLines() {
        GoldenCase c = GoldenCase.named("dup-id");
        List<String> program = List.of("START OF EXECUTION OF PROGRAM CBTRN02C",
                "TCATBAL record not found for key : 00000000020030001.. Creating.",
                "ERROR WRITING TO TRANSACTION FILE", "FILE STATUS IS: NNNN0022", "ABENDING PROGRAM");
        ParityHarness.assertSysout(c, program);
        assertThatThrownBy(() -> ParityHarness.assertSysout(c, program.subList(0, 4))).isInstanceOf(AssertionError.class);
    }

    @Test
    void dalyrejsIsComparedByteForByte(@TempDir Path tmp) throws IOException {
        GoldenCase c = GoldenCase.named("acct-missing");
        byte[] bytes = Files.readAllBytes(c.expected("DALYREJS"));
        Path same = Files.write(tmp.resolve("same"), bytes);
        ParityHarness.assertDalyrejs(c, same);
        bytes[bytes.length - 1] = 'X';
        Path drift = Files.write(tmp.resolve("drift"), bytes);
        assertThatThrownBy(() -> ParityHarness.assertDalyrejs(c, drift)).isInstanceOf(AssertionError.class);
    }
}
