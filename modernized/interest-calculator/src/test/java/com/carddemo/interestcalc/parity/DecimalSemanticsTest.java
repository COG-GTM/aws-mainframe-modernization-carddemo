package com.carddemo.interestcalc.parity;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.interestcalc.copybook.TransactionRecord;
import com.carddemo.interestcalc.program.InterestCalculationResult;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * The decimal edge cases of {@code 1300-COMPUTE-INTEREST}, asserted against the values the real
 * COBOL produced (see {@code src/test/resources/golden/edge/transact.dat}).
 *
 * <p>Every expected value in this class was read out of the GnuCOBOL run, not derived from the
 * Java implementation.
 */
class DecimalSemanticsTest {

    private static InterestCalculationResult edge;
    private static InterestCalculationResult base;

    @BeforeAll
    static void runJob() {
        edge = CobolOracleParityTest.run(GoldenMaster.edgeDataset());
        base = CobolOracleParityTest.run(GoldenMaster.shippedDataset());
    }

    /**
     * The money moment: the same inputs under {@code HALF_UP} give a different answer. If the
     * migration had used {@code setScale(2, HALF_UP)} - the default reflex in most Java shops -
     * these three records would each be one cent out.
     */
    static Stream<Arguments> truncationCases() {
        return Stream.of(
                //        balance        rate      COBOL      HALF_UP would give
                Arguments.of("100.48", "15.00", "1.25", "1.26"),
                Arguments.of("-100.48", "15.00", "-1.25", "-1.26"),
                Arguments.of("12345678.99", "25.00", "257201.64", "257201.65"));
    }

    @ParameterizedTest(name = "{0} at {1}% -> {2} (HALF_UP would give {3})")
    @MethodSource("truncationCases")
    @DisplayName("COMPUTE without ROUNDED truncates toward zero, and HALF_UP would differ")
    void halfUpWouldProduceADifferentAnswer(String balance, String rate, String cobolResult, String halfUpResult) {
        BigDecimal exact = new BigDecimal(balance).multiply(new BigDecimal(rate))
                .divide(new BigDecimal("1200"), 10, RoundingMode.DOWN);

        assertThat(exact.setScale(2, RoundingMode.DOWN))
                .as("what CBACT04C computes")
                .isEqualByComparingTo(cobolResult);
        assertThat(exact.setScale(2, RoundingMode.HALF_UP))
                .as("what a HALF_UP migration would compute")
                .isEqualByComparingTo(halfUpResult);
        assertThat(cobolResult).isNotEqualTo(halfUpResult);

        // ... and the value the real COBOL wrote to TRANSACT is the truncated one.
        assertThat(amountsWrittenByTheOracle()).contains(new BigDecimal(cobolResult));
    }

    @Test
    @DisplayName("A third decimal place is truncated, never rounded: 1.256 -> 1.25")
    void truncatesTheThirdDecimalPlace() {
        assertThat(edge.transactions().get(0).amount()).isEqualByComparingTo("1.25");
        assertThat(edge.transactions().get(1).amount())
                .as("2.083333... also truncates")
                .isEqualByComparingTo("2.08");
    }

    @Test
    @DisplayName("Truncation is toward zero (DOWN), not toward negative infinity (FLOOR)")
    void negativeBalanceTruncatesTowardZero() {
        TransactionRecord negative = edge.transactions().get(3);
        assertThat(negative.description()).startsWith("Int. for a/c 00000000102");
        assertThat(negative.amount())
                .as("-100.48 at 15%% is -1.256; FLOOR would give -1.26")
                .isEqualByComparingTo("-1.25");
        assertThat(new BigDecimal("-1.256").setScale(2, RoundingMode.FLOOR)).isEqualByComparingTo("-1.26");
    }

    @Test
    @DisplayName("A zero balance still writes a zero-amount transaction (the rate, not the balance, gates the write)")
    void zeroBalanceStillWritesATransaction() {
        TransactionRecord zero = edge.transactions().get(2);
        assertThat(zero.description()).startsWith("Int. for a/c 00000000101");
        assertThat(zero.amount()).isEqualByComparingTo("0.00");
        assertThat(zero.transactionId()).isEqualTo("2022071800000003");
    }

    @Test
    @DisplayName("A zero disclosure rate writes no transaction at all (IF DIS-INT-RATE NOT = 0)")
    void zeroRateWritesNoTransaction() {
        assertThat(edge.transactions())
                .as("account 103 is in the ZEROAPR group, whose rates are all 0.00")
                .noneMatch(record -> record.description().startsWith("Int. for a/c 00000000103"));
        assertThat(edge.recordsRead()).as("its two TCATBALF records are still read").isEqualTo(7);
    }

    @Test
    @DisplayName("File status '23' on DISCGRP falls back to the DEFAULT group")
    void disclosureGroupFallsBackToDefault() {
        assertThat(edge.console())
                .as("account 102 has a blank ACCT-GROUP-ID, so its lookup misses")
                .containsSequence("DISCLOSURE GROUP RECORD MISSING", "TRY WITH DEFAULT GROUP CODE");
        assertThat(edge.transactions().get(3).amount())
                .as("the DEFAULT 01/0001 rate is 15.00, so -100.48 gives -1.25")
                .isEqualByComparingTo("-1.25");

        assertThat(base.console().stream().filter("DISCLOSURE GROUP RECORD MISSING"::equals).count())
                .as("every account in app/data/ASCII/acctdata.txt has a blank group id, "
                        + "so all 50 lookups take the DEFAULT path")
                .isEqualTo(50);
    }

    @Test
    @DisplayName("A direct disclosure-group hit does not log the fallback")
    void directDisclosureGroupHitDoesNotFallBack() {
        long fallbacks = edge.console().stream().filter("DISCLOSURE GROUP RECORD MISSING"::equals).count();
        assertThat(fallbacks)
                .as("only account 102 misses; 101, 103 and 104 hit A000000000 / ZEROAPR directly")
                .isEqualTo(1);
    }

    @Test
    @DisplayName("1050-UPDATE-ACCOUNT posts the accumulated interest and clears the cycle totals")
    void accountUpdatePostsTotalInterestAndClearsCycleTotals() {
        // Account 101: 1.25 + 2.08 + 0.00 = 3.33 posted onto an opening balance of 1000.00.
        assertThat(edge.accountMasterAfter().get(0))
                .startsWith("00000000101Y00000010033C")
                .contains("00000000000{00000000000{");
    }

    @Test
    @DisplayName("The last account group is never rewritten - the COBOL ELSE branch is unreachable")
    void lastAccountGroupIsNeverRewritten() {
        // Account 104 earns 257201.64 of interest, but it is the last account in key order, so
        // 1050-UPDATE-ACCOUNT is never performed for it: its balance stays at 1000.00 and its
        // cycle totals (111.11 / 222.22) are left untouched. This is a genuine legacy defect,
        // reproduced deliberately. The COBOL oracle output agrees byte for byte.
        assertThat(edge.accountMasterAfter().get(3))
                .startsWith("00000000104Y00000010000{")
                .contains("00000001111A00000002222B");
    }

    private static List<BigDecimal> amountsWrittenByTheOracle() {
        return GoldenMaster.goldenTransactions("edge").stream().map(TransactionRecord::amount).toList();
    }
}
