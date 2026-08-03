package com.carddemo.interestcalc.parity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;

import com.carddemo.interestcalc.copybook.TransactionRecord;
import com.carddemo.interestcalc.program.Cbact04cProgram;
import com.carddemo.interestcalc.program.InterestCalculationRequest;
import com.carddemo.interestcalc.program.InterestCalculationResult;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * The golden-master differential harness: runs the migrated Java job over the same datasets the
 * real COBOL was run over and compares the TRANSACT output field by field, plus the account
 * master after the run byte for byte.
 *
 * <p>The golden files are the output of the <em>unmodified</em> {@code app/cbl/CBACT04C.cbl}
 * compiled with GnuCOBOL 3.1.2 ({@code oracle/run-cobol-oracle.sh}), so a failure here means the
 * Java and the COBOL genuinely disagree.
 */
class CobolOracleParityTest {

    static Stream<Arguments> datasets() {
        return Stream.of(
                Arguments.of("the shipped app/data/ASCII sample", "base", GoldenMaster.shippedDataset(), 50, 50),
                Arguments.of("the synthetic decimal edge cases", "edge", GoldenMaster.edgeDataset(), 5, 4));
    }

    @ParameterizedTest(name = "TRANSACT output matches the COBOL oracle for {0}")
    @MethodSource("datasets")
    @DisplayName("Every TRAN-RECORD field except the timestamps is identical to the COBOL output")
    void transactionOutputMatchesTheCobolOracle(String description, String dataset, Path datasetDirectory,
                                                int expectedTransactions, int expectedAccounts) {
        InterestCalculationResult result = run(datasetDirectory);
        List<TransactionRecord> expected = GoldenMaster.goldenTransactions(dataset);
        List<TransactionRecord> actual = result.transactions();

        assertThat(actual).as("number of TRANSACT records written").hasSize(expectedTransactions);
        assertThat(expected).as("number of TRANSACT records in the COBOL oracle output")
                .hasSize(expectedTransactions);
        assertThat(result.accountMasterAfter()).as("accounts in the master file").hasSize(expectedAccounts);

        List<org.junit.jupiter.api.function.Executable> assertions = new ArrayList<>();
        for (int i = 0; i < expected.size(); i++) {
            List<GoldenMaster.NamedField> expectedFields = GoldenMaster.comparableFields(expected.get(i));
            List<GoldenMaster.NamedField> actualFields = GoldenMaster.comparableFields(actual.get(i));
            int record = i + 1;
            for (int f = 0; f < expectedFields.size(); f++) {
                GoldenMaster.NamedField want = expectedFields.get(f);
                GoldenMaster.NamedField got = actualFields.get(f);
                assertions.add(() -> assertThat(got.value())
                        .as("record %d field %s", record, want.name())
                        .isEqualTo(want.value()));
            }
        }
        assertAll(assertions);
    }

    @ParameterizedTest(name = "ACCTFILE after the run matches the COBOL oracle for {0}")
    @MethodSource("datasets")
    @DisplayName("1050-UPDATE-ACCOUNT rewrites the account master exactly as the COBOL does")
    void accountMasterMatchesTheCobolOracle(String description, String dataset, Path datasetDirectory,
                                            int expectedTransactions, int expectedAccounts) {
        InterestCalculationResult result = run(datasetDirectory);
        assertThat(result.accountMasterAfter())
                .containsExactlyElementsOf(GoldenMaster.goldenLines(dataset + "/acctdata-after.dat"));
    }

    @ParameterizedTest(name = "timestamps are the only nondeterministic output for {0}")
    @MethodSource("datasets")
    @DisplayName("TRAN-ORIG-TS / TRAN-PROC-TS follow the DB2 format and come from the injected Clock")
    void timestampsComeFromTheInjectedClock(String description, String dataset, Path datasetDirectory,
                                            int expectedTransactions, int expectedAccounts) {
        for (TransactionRecord oracleRecord : GoldenMaster.goldenTransactions(dataset)) {
            assertThat(oracleRecord.originTimestamp())
                    .as("the COBOL oracle's TRAN-ORIG-TS is a DB2 timestamp taken from the wall clock")
                    .matches(GoldenMaster.DB2_TIMESTAMP);
            assertThat(oracleRecord.processTimestamp()).isEqualTo(oracleRecord.originTimestamp());
        }
        for (TransactionRecord javaRecord : run(datasetDirectory).transactions()) {
            assertThat(javaRecord.originTimestamp()).isEqualTo(GoldenMaster.EXPECTED_TIMESTAMP);
            assertThat(javaRecord.processTimestamp()).isEqualTo(GoldenMaster.EXPECTED_TIMESTAMP);
        }
    }

    static InterestCalculationResult run(Path datasetDirectory) {
        return new Cbact04cProgram(GoldenMaster.FIXED_CLOCK).run(new InterestCalculationRequest(
                datasetDirectory.resolve("tcatbal.txt"),
                datasetDirectory.resolve("cardxref.txt"),
                datasetDirectory.resolve("acctdata.txt"),
                datasetDirectory.resolve("discgrp.txt"),
                GoldenMaster.RUN_DATE));
    }
}
