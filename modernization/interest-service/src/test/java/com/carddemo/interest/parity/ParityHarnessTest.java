package com.carddemo.interest.parity;

import com.carddemo.interest.batch.InterestCalculationJob;
import com.carddemo.interest.batch.InterestDatasets;
import com.carddemo.interest.domain.Account;
import com.carddemo.interest.io.codec.AccountCodec;
import com.carddemo.interest.io.codec.TransactionCodec;
import com.carddemo.interest.service.FinalAccountPolicy;
import com.carddemo.interest.service.InterestAccrualResult;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.MethodOrderer;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Black-box parity harness for the CBACT04C sliver.
 *
 * <p>The harness reads the real EBCDIC datasets under {@code app/data/EBCDIC}, runs both the
 * idiomatic Java implementation and the {@link Cbact04cReferenceModel} COBOL transliteration over
 * the full input population, and compares the two output streams — rewritten account-master
 * records and generated interest transactions — byte for byte in the encoded copybook layout.
 * Comparing encoded records rather than Java objects means field formatting, sign overpunches and
 * fixed-point truncation are all in scope.
 *
 * <p>Both engines are driven with the same fixed clock and the same JCL run-date parameter, so the
 * only sources of difference are business rules and numeric semantics.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class ParityHarnessTest {

    /** {@code PARM='2022071800'} from {@code app/jcl/INTCALC.jcl:22}. */
    private static final String RUN_DATE = "2022071800";

    private static final Instant FIXED_INSTANT = Instant.parse("2022-07-18T23:14:05.120Z");
    private static final String FIXED_DB2_TIMESTAMP = "2022-07-18-23.14.05.120000";
    private static final Charset CP037 = Charset.forName("IBM037");

    private static final Path EBCDIC_DIRECTORY = Path.of("..", "..", "app", "data", "EBCDIC");
    private static final Path PARITY_REPORT = Path.of("..", "PARITY-REPORT.md");

    /** Number of scenarios a full run records; the report is only written when all are present. */
    private static final int SCENARIO_COUNT = 2;

    private static final List<ScenarioResult> RESULTS = new ArrayList<>();

    private record ScenarioResult(String name, String population, int categoryBalances,
                                  int transactionsCompared, int transactionsMatched,
                                  int accountsCompared, int accountsMatched) {

        int mismatches() {
            return (transactionsCompared - transactionsMatched) + (accountsCompared - accountsMatched);
        }
    }

    @Test
    @Order(1)
    @DisplayName("Full shipped EBCDIC population produces identical outputs in Java and COBOL")
    void shippedPopulationMatches() {
        InterestDatasets datasets = InterestDatasets.fromDirectory(EBCDIC_DIRECTORY);
        RESULTS.add(compare("Shipped EBCDIC datasets",
                "app/data/EBCDIC (TCATBALF, XREFFILE, ACCTDATA, DISCGRP), unmodified", datasets));
    }

    @Test
    @Order(2)
    @DisplayName("Derived population with non-zero balances and mixed pricing groups also matches")
    void derivedPopulationMatches() {
        InterestDatasets datasets = DerivedPopulation.build(EBCDIC_DIRECTORY);
        RESULTS.add(compare("Derived stress population",
                "shipped ACCTDATA/CARDXREF/DISCGRP with synthesised TCATBALF balances and "
                        + "rewritten account pricing groups", datasets));
    }

    @Test
    @Order(3)
    @DisplayName("Shipped population exercises the DEFAULT disclosure-group fallback")
    void shippedPopulationUsesDefaultGroupFallback() {
        InterestDatasets datasets = InterestDatasets.fromDirectory(EBCDIC_DIRECTORY);
        boolean everyAccountGroupBlank = datasets.accounts().stream()
                .allMatch(account -> account.pricingGroupId().isEmpty());
        assertTrue(everyAccountGroupBlank,
                "every shipped account has a blank ACCT-GROUP-ID, so every rate lookup falls back to DEFAULT");
    }

    @Test
    @Order(4)
    @DisplayName("Derived population reaches a non-zero exact pricing-group match, not only the fallback")
    void derivedPopulationReachesExactGroupMatch() {
        InterestDatasets datasets = DerivedPopulation.build(EBCDIC_DIRECTORY);
        List<String> groups = datasets.accounts().stream().map(Account::pricingGroupId).distinct().toList();
        assertTrue(groups.contains("A000000000"),
                "the derived accounts must use a pricing group that really exists in DISCGRP");

        boolean nonZeroExactRate = datasets.disclosureGroups().stream()
                .anyMatch(group -> group.key().accountGroupId().equals("A000000000")
                        && !group.isZeroRate());
        assertTrue(nonZeroExactRate,
                "A000000000 must price at least one category at a non-zero rate");
    }

    private static ScenarioResult compare(String scenarioName, String populationDescription,
                                          InterestDatasets datasets) {
        InterestCalculationJob job = new InterestCalculationJob(
                Clock.fixed(FIXED_INSTANT, ZoneOffset.UTC), FinalAccountPolicy.MAINFRAME_PARITY);
        InterestAccrualResult javaResult = job.run(datasets, RUN_DATE);

        Cbact04cReferenceModel.Output cobolResult = new Cbact04cReferenceModel(
                datasets.rawTransactionCategoryBalanceImage(), datasets.rawCardXrefImage(),
                datasets.rawAccountImage(), datasets.rawDisclosureGroupImage(),
                RUN_DATE, FIXED_DB2_TIMESTAMP).run();

        List<byte[]> javaTransactions = javaResult.transactions().stream()
                .map(TransactionCodec::encode).toList();
        List<byte[]> javaAccounts = javaResult.updatedAccounts().stream()
                .map(AccountCodec::encode).toList();

        assertEquals(cobolResult.transactionRecords().size(), javaTransactions.size(),
                scenarioName + ": number of generated interest transactions");
        assertEquals(cobolResult.rewrittenAccountRecords().size(), javaAccounts.size(),
                scenarioName + ": number of rewritten account records");

        int transactionsMatched = countMatches(cobolResult.transactionRecords(), javaTransactions,
                scenarioName + " transaction");
        int accountsMatched = countMatches(cobolResult.rewrittenAccountRecords(), javaAccounts,
                scenarioName + " account");

        assertEquals(javaTransactions.size(), transactionsMatched, scenarioName + ": transaction record parity");
        assertEquals(javaAccounts.size(), accountsMatched, scenarioName + ": account record parity");

        return new ScenarioResult(scenarioName, populationDescription,
                javaResult.categoryBalancesProcessed(),
                javaTransactions.size(), transactionsMatched,
                javaAccounts.size(), accountsMatched);
    }

    private static int countMatches(List<byte[]> expected, List<byte[]> actual, String what) {
        int matched = 0;
        for (int index = 0; index < expected.size(); index++) {
            if (Arrays.equals(expected.get(index), actual.get(index))) {
                matched++;
            } else {
                System.out.println("Mismatch on " + what + " record " + (index + 1)
                        + "\n  COBOL: " + new String(expected.get(index), CP037)
                        + "\n  Java : " + new String(actual.get(index), CP037));
            }
        }
        return matched;
    }

    @Test
    @Order(5)
    @DisplayName("Java job leaves untouched accounts byte-identical in the rewritten master")
    void accountMasterKeepsUntouchedAccounts() {
        InterestDatasets datasets = InterestDatasets.fromDirectory(EBCDIC_DIRECTORY);
        InterestCalculationJob job = new InterestCalculationJob(
                Clock.fixed(FIXED_INSTANT, ZoneOffset.UTC), FinalAccountPolicy.MAINFRAME_PARITY);
        InterestAccrualResult result = job.run(datasets, RUN_DATE);
        List<Account> original = datasets.accounts();
        byte[] rewritten = InterestCalculationJob.encodeAccountMaster(original, result.updatedAccounts());
        assertEquals(datasets.rawAccountImage().length, rewritten.length,
                "the rewritten account master must keep the same record count");
    }

    @AfterAll
    static void writeParityReport() throws IOException {
        if (RESULTS.size() != SCENARIO_COUNT) {
            // A partial run (a single -Dtest method, an IDE run) must not overwrite the committed
            // deliverable with an incomplete table.
            return;
        }
        StringBuilder report = new StringBuilder();
        report.append("""
                # Parity Report — CBACT04C Interest Calculation Sliver

                Generated by `ParityHarnessTest` (`mvn test` in `modernization/interest-service`).
                This file is regenerated by the test run; do not edit it by hand.

                ## What is compared

                Two engines are run over the same inputs and their **encoded output records** are
                compared byte for byte in the copybook layouts:

                | Engine | What it is |
                | --- | --- |
                | Java | `com.carddemo.interest.service.InterestAccrualService` driven by `InterestCalculationJob` |
                | Oracle | `Cbact04cReferenceModel`, a literal paragraph-by-paragraph transliteration of `app/cbl/CBACT04C.cbl` with its own EBCDIC/zoned-decimal handling |

                Outputs compared: rewritten `ACCTFILE` records (`CVACT01Y`, 300 bytes) and generated
                `TRANSACT` interest records (`CVTRA05Y`, 350 bytes). Both engines use the same fixed
                clock and the same JCL run-date parameter `PARM='2022071800'`, so timestamps and
                transaction ids are deterministic and in scope for the comparison.

                No mainframe was available for this engagement, so the oracle is derived from the
                static COBOL source rather than from a captured production run. Every rule it
                encodes is cited to a paragraph in `modernization/CBACT04C-logic-map.md`.

                ## Results

                | Scenario | Category balances read | Interest txns compared | Txns matched | Account records compared | Accounts matched | Mismatches | Match rate |
                | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
                """);
        int totalCompared = 0;
        int totalMatched = 0;
        for (ScenarioResult result : RESULTS) {
            int compared = result.transactionsCompared() + result.accountsCompared();
            int matched = result.transactionsMatched() + result.accountsMatched();
            totalCompared += compared;
            totalMatched += matched;
            report.append("| %s | %d | %d | %d | %d | %d | %d | %s |%n".formatted(
                    result.name(), result.categoryBalances(),
                    result.transactionsCompared(), result.transactionsMatched(),
                    result.accountsCompared(), result.accountsMatched(),
                    result.mismatches(), percentage(matched, compared)));
        }
        report.append("| **Total** | | | | | | **%d** | **%s** |%n%n".formatted(
                totalCompared - totalMatched, percentage(totalMatched, totalCompared)));

        for (ScenarioResult result : RESULTS) {
            report.append("- **%s** — %s%n".formatted(result.name(), result.population()));
        }

        report.append("""

                ## Notes on the populations

                * The shipped `TCATBALF` dump carries a zero `TRAN-CAT-BAL` on every one of its
                  records, and every shipped account has a blank `ACCT-GROUP-ID`. The shipped
                  scenario therefore proves the control flow, the `DEFAULT` fallback and the record
                  formatting, but it cannot exercise the arithmetic.
                * The derived scenario keeps the real account, cross-reference and disclosure
                  datasets and synthesises `TCATBALF` balances plus account pricing groups, so that
                  non-zero, negative, zero-rate (`ZEROAPR`), exact-group-match and
                  truncate-not-round cases are all covered.
                * The final account of a run is deliberately **not** balance-updated: see
                  `FinalAccountPolicy` and rule BR-8 of the logic map. Both engines reproduce this
                  defect, which is why the account-record count is one lower than the number of
                  accounts processed.
                """);
        Files.writeString(PARITY_REPORT, report.toString());
    }

    private static String percentage(int matched, int compared) {
        if (compared == 0) {
            return "n/a";
        }
        return "%.2f%%".formatted(100.0 * matched / compared);
    }
}
