package com.carddemo.app.golden;

import com.carddemo.common.batch.ReturnCode;
import com.carddemo.common.cobol.CodePage;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class GoldenHarnessTest {

    @Test
    void casesAndTheirOracleReturnCodes() {
        assertThat(GoldenCase.all()).extracting(GoldenCase::name)
                .containsExactly("GC-1", "GC-2", "GC-3", "GC-4", "GC-5", "GC-6", "GC-7");
        assertThat(GoldenCase.all()).extracting(GoldenCase::oracleReturnCode).containsExactly(4, 4, 0, 231, 0, 4, 4);
        assertThat(GoldenCase.all()).allSatisfy(gc -> assertThat(gc.dalytran()).exists());
    }

    @Test
    void gc4UsesTheAbendRules() {
        GoldenCase gc4 = GoldenCase.of("GC-4");
        assertThat(gc4.abend()).isTrue();
        assertThat(gc4.expectedJavaReturnCode()).isEqualTo(ReturnCode.ABEND);
        assertThat(gc4.expectedDalyrejs()).isEmpty();
        assertThat(gc4.expectedTable("ACCTDATA.unl").toString()).contains("expected-prefix");
        // the oracle abend run left the failing record's account update applied; the valid prefix did not
        assertThat(GoldenCompare.compareUnloads(GoldenCompare.lines(gc4.expected("ACCTDATA.unl")),
                GoldenCompare.lines(gc4.expectedTable("ACCTDATA.unl")), 300)).isNotEmpty();
        GoldenCase gc1 = GoldenCase.of("GC-1");
        assertThat(gc1.abend()).isFalse();
        assertThat(gc1.expectedTable("ACCTDATA.unl")).isEqualTo(gc1.expected("ACCTDATA.unl"));
    }

    @Test
    void d007MaskIgnoresOnlyProcTsHundredths() {
        String record = "X".repeat(324) + "12" + "Y".repeat(24);
        String hundredths = "X".repeat(324) + "99" + "Y".repeat(24);
        String other = "X".repeat(323) + "Z12" + "Y".repeat(24);
        assertThat(GoldenCompare.compareUnloads(List.of(record), List.of(hundredths), 350, GoldenCompare.TRANSACT_MASK)).isEmpty();
        assertThat(GoldenCompare.compareUnloads(List.of(record), List.of(hundredths), 350)).hasSize(1);
        assertThat(GoldenCompare.compareUnloads(List.of(record), List.of(other), 350, GoldenCompare.TRANSACT_MASK)).hasSize(1);
        assertThat(GoldenCompare.compareUnloads(List.of(record), List.of(), 350)).hasSize(1);
    }

    @Test
    void trailingSpacesAreNotDifferences() {
        assertThat(GoldenCompare.compareUnloads(List.of("AB"), List.of("AB   "), 5)).isEmpty();
    }

    @Test
    void sysoutCountLines() {
        assertThat(GoldenCompare.countLines(GoldenCompare.lines(GoldenCase.of("GC-1").expected("SYSOUT"))))
                .containsExactly("TRANSACTIONS PROCESSED :000000300", "TRANSACTIONS REJECTED  :000000038");
        assertThat(GoldenCompare.countLines(GoldenCompare.lines(GoldenCase.of("GC-4").expected("SYSOUT")))).isEmpty();
    }

    @Test
    void gc6RejectsTheMissingAccountWithReason101() {
        GoldenCase gc6 = GoldenCase.of("GC-6");
        byte[] dalyrejs = GoldenCompare.readAllBytes(gc6.expected("DALYREJS"));
        assertThat(dalyrejs).hasSize(430);
        assertThat(CodePage.ASCII.decode(dalyrejs, 350, 28)).isEqualTo("0101ACCOUNT RECORD NOT FOUND");
        assertThat(gc6.customCardXref()).isPresent();
    }

    @Test
    void gc7OracleRejectsConvertBackToTheCp037InputRecords() throws Exception {
        GoldenCase gc7 = GoldenCase.of("GC-7");
        byte[] expected = GoldenCompare.expectedDalyrejs(gc7.expected("DALYREJS"), gc7.encoding());
        List<byte[]> input = GoldenCompare.records(Files.readAllBytes(gc7.dalytran()), 350);
        List<byte[]> rejects = GoldenCompare.records(expected, 430);
        assertThat(rejects).hasSize(38);
        assertThat(rejects).allSatisfy(r -> assertThat(input).anySatisfy(
                i -> assertThat(Arrays.copyOf(r, 350)).isEqualTo(i)));
    }
}
