package com.carddemo.posting.parity;

import com.carddemo.posting.batch.DailyTransactionPostingJob;
import com.carddemo.posting.batch.PostingDatasets;
import com.carddemo.posting.batch.PostingRunOutput;
import com.carddemo.posting.domain.RejectReason;
import com.carddemo.posting.io.codec.TransactionCategoryBalanceCodec;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Black-box parity harness for the CBTRN02C sliver.
 *
 * <p>The harness reads the real EBCDIC datasets under {@code app/data/EBCDIC}, runs both the
 * idiomatic Java job and the {@link Cbtrn02cReferenceModel} COBOL transliteration over the same
 * inputs, and compares all four output streams of job {@code POSTTRAN} byte for byte in their
 * encoded copybook layouts: posted transactions, rejected transactions, the rewritten account
 * master and the rewritten transaction-category-balance file.
 *
 * <p>Comparing encoded records rather than Java objects puts field formatting, EBCDIC sign
 * overpunches, filler bytes and fixed-point truncation in scope. Both engines are driven with the
 * same frozen clock, so processing timestamps are deterministic and are compared too.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class ParityHarnessTest {

    private static final Instant FIXED_INSTANT = Instant.parse("2022-07-18T23:14:05.120Z");
    private static final String FIXED_DB2_TIMESTAMP = "2022-07-18-23.14.05.120000";
    private static final Charset CP037 = Charset.forName("IBM037");

    private static final Path EBCDIC_DIRECTORY = Path.of("..", "..", "app", "data", "EBCDIC");
    private static final Path PARITY_REPORT = Path.of("..", "PARITY-REPORT-CBTRN02C.md");

    private static final List<ScenarioResult> RESULTS = new ArrayList<>();

    private record StreamResult(String name, int compared, int matched) {
        int mismatched() {
            return compared - matched;
        }
    }

    private record ScenarioResult(String name, String population, int transactionsRead,
                                  int posted, int rejected, int categoryBalancesCreated,
                                  List<StreamResult> streams) {

        int compared() {
            return streams.stream().mapToInt(StreamResult::compared).sum();
        }

        int matched() {
            return streams.stream().mapToInt(StreamResult::matched).sum();
        }
    }

    @Test
    @Order(1)
    @DisplayName("Shipped DALYTRAN population posts and rejects identically in Java and COBOL")
    void shippedPopulationMatches() {
        RESULTS.add(compare("Shipped EBCDIC datasets",
                "app/data/EBCDIC — DALYTRAN, CARDXREF, ACCTDATA and TCATBALF, unmodified",
                PostingDatasets.loadFrom(EBCDIC_DIRECTORY)));
    }

    @Test
    @Order(2)
    @DisplayName("Derived population covering all four reject reasons also matches")
    void derivedPopulationMatches() {
        RESULTS.add(compare("Derived stress population",
                "shipped datasets patched for unknown cards, a dangling account, expired accounts, "
                        + "refunds and unused transaction categories",
                DerivedPopulation.from(PostingDatasets.loadFrom(EBCDIC_DIRECTORY))));
    }

    @Test
    @Order(3)
    @DisplayName("Balances driven past their COBOL field capacity truncate identically")
    void overflowPopulationMatches() {
        PostingDatasets datasets =
                DerivedPopulation.overflowFrom(PostingDatasets.loadFrom(EBCDIC_DIRECTORY));
        ScenarioResult result = compare("Field-capacity overflow population",
                "every shipped transaction re-pointed at one card and set to the largest amount "
                        + "its field can hold, so the receiving balances overflow", datasets);
        RESULTS.add(result);

        // The 250 shipped 01/0001 transactions now carry 999,999,999.99 each: 249,999,999,997.50,
        // of which the PIC S9(09)V99 bucket keeps only the low-order nine integer digits.
        List<BigDecimal> balances = split(run(datasets).categoryBalanceFileImage(), 50).stream()
                .map(record -> TransactionCategoryBalanceCodec.decode(record).balance()).toList();
        assertTrue(balances.contains(new BigDecimal("999999997.50")),
                "the scenario must actually overflow a category balance, not merely fill it");
    }

    @Test
    @Order(4)
    @DisplayName("Every reject reason of CBTRN02C is exercised by the two populations")
    void everyRejectReasonIsCovered() {
        Map<RejectReason, Integer> counts = new EnumMap<>(RejectReason.class);
        for (PostingDatasets datasets : List.of(
                PostingDatasets.loadFrom(EBCDIC_DIRECTORY),
                DerivedPopulation.from(PostingDatasets.loadFrom(EBCDIC_DIRECTORY)))) {
            run(datasets).result().rejected().forEach(rejected ->
                    counts.merge(rejected.reason(), 1, Integer::sum));
        }
        for (RejectReason reason : RejectReason.values()) {
            assertTrue(counts.getOrDefault(reason, 0) > 0,
                    "no scenario produced reject reason " + reason.code() + " " + reason);
        }
    }

    @Test
    @Order(5)
    @DisplayName("Shipped daily transactions survive a decode/encode round trip byte for byte")
    void dailyTransactionsRoundTrip() {
        byte[] image = PostingDatasets.loadFrom(EBCDIC_DIRECTORY).dailyTransactionImage();
        for (int offset = 0; offset < image.length; offset += 350) {
            byte[] original = Arrays.copyOfRange(image, offset, offset + 350);
            byte[] roundTripped = com.carddemo.posting.io.codec.DailyTransactionCodec
                    .encode(com.carddemo.posting.io.codec.DailyTransactionCodec.decode(original));
            assertArrayEqualsAt(original, roundTripped, offset / 350);
        }
    }

    private static void assertArrayEqualsAt(byte[] expected, byte[] actual, int index) {
        assertTrue(Arrays.equals(expected, actual),
                "daily transaction " + index + " does not re-encode to its original bytes:"
                        + "\n  file: " + new String(expected, CP037)
                        + "\n  java: " + new String(actual, CP037));
    }

    private static PostingRunOutput run(PostingDatasets datasets) {
        return new DailyTransactionPostingJob(Clock.fixed(FIXED_INSTANT, ZoneOffset.UTC))
                .run(datasets);
    }

    private static ScenarioResult compare(String scenarioName, String populationDescription,
                                          PostingDatasets datasets) {
        PostingRunOutput java = run(datasets);
        Cbtrn02cReferenceModel.Output cobol = new Cbtrn02cReferenceModel(
                datasets.dailyTransactionImage(), datasets.cardXrefImage(),
                datasets.accountImage(), datasets.categoryBalanceImage(),
                FIXED_DB2_TIMESTAMP).run();

        assertEquals(cobol.transactionsRead(), java.result().transactionsRead(),
                scenarioName + ": transactions read");
        assertEquals(cobol.returnCode(), java.result().returnCode(),
                scenarioName + ": step return code");
        assertEquals(cobol.categoryBalancesCreated(), java.categoryBalancesCreated(),
                scenarioName + ": transaction category balances created");

        List<StreamResult> streams = List.of(
                compareStream(scenarioName, "TRANSACT posted transactions (CVTRA05Y, 350 bytes)",
                        cobol.transactionRecords(), split(java.transactionFileImage(), 350)),
                compareStream(scenarioName, "DALYREJS rejected transactions (430 bytes)",
                        cobol.rejectRecords(), split(java.rejectFileImage(), 430)),
                compareStream(scenarioName, "ACCTFILE account master (CVACT01Y, 300 bytes)",
                        split(cobol.accountFileImage(), 300), split(java.accountFileImage(), 300)),
                compareStream(scenarioName, "TCATBALF category balances (CVTRA01Y, 50 bytes)",
                        split(cobol.categoryBalanceFileImage(), 50),
                        split(java.categoryBalanceFileImage(), 50)));

        for (StreamResult stream : streams) {
            assertEquals(stream.compared(), stream.matched(),
                    scenarioName + ": " + stream.name() + " parity");
        }

        return new ScenarioResult(scenarioName, populationDescription,
                java.result().transactionsRead(), java.result().posted().size(),
                java.result().rejected().size(), java.categoryBalancesCreated(), streams);
    }

    private static StreamResult compareStream(String scenarioName, String streamName,
                                              List<byte[]> expected, List<byte[]> actual) {
        assertEquals(expected.size(), actual.size(),
                scenarioName + ": record count of " + streamName);
        int matched = 0;
        for (int index = 0; index < expected.size(); index++) {
            if (Arrays.equals(expected.get(index), actual.get(index))) {
                matched++;
            } else {
                System.out.println("Mismatch in " + streamName + ", record " + (index + 1)
                        + "\n  COBOL: " + new String(expected.get(index), CP037)
                        + "\n  Java : " + new String(actual.get(index), CP037));
            }
        }
        return new StreamResult(streamName, expected.size(), matched);
    }

    private static List<byte[]> split(byte[] image, int recordLength) {
        List<byte[]> records = new ArrayList<>();
        for (int offset = 0; offset < image.length; offset += recordLength) {
            records.add(Arrays.copyOfRange(image, offset, offset + recordLength));
        }
        return records;
    }

    @AfterAll
    static void writeParityReport() throws IOException {
        if (RESULTS.isEmpty()) {
            return;
        }
        StringBuilder report = new StringBuilder();
        report.append("""
                # Parity Report — CBTRN02C Daily Transaction Posting Sliver

                Generated by `ParityHarnessTest` (`mvn test` in `modernization`).
                This file is regenerated by the test run; do not edit it by hand.

                ## What is compared

                Two engines run over the same inputs and their **encoded output records** are
                compared byte for byte in the copybook layouts:

                | Engine | What it is |
                | --- | --- |
                | Java | `com.carddemo.posting.service.DailyTransactionPostingService` driven by `DailyTransactionPostingJob` |
                | Oracle | `Cbtrn02cReferenceModel`, a paragraph-by-paragraph transliteration of `app/cbl/CBTRN02C.cbl` with its own EBCDIC, sign-overpunch and scaled-integer arithmetic, sharing no code with the module under test |

                All four output streams of job `POSTTRAN` are compared: posted transactions
                (`TRANSACT`), rejected transactions (`DALYREJS`), the rewritten account master
                (`ACCTFILE`) and the rewritten category balances (`TCATBALF`). Both engines are
                driven with the same frozen clock, so the `TRAN-PROC-TS` stamped on every posted
                transaction is deterministic and in scope for the comparison.

                No mainframe was available for this engagement, so the oracle is derived from the
                static COBOL source rather than from a captured production run. Every rule it
                encodes is cited to a paragraph in `modernization/CBTRN02C-logic-map.md`.

                ## Results

                """);
        for (ScenarioResult result : RESULTS) {
            report.append("### %s%n%n".formatted(result.name()));
            report.append("%s.%n%n".formatted(result.population()));
            report.append(("Transactions read **%d**, posted **%d**, rejected **%d**, "
                    + "category balances created **%d**.%n%n").formatted(
                    result.transactionsRead(), result.posted(), result.rejected(),
                    result.categoryBalancesCreated()));
            report.append("| Output stream | Records compared | Matched | Mismatched | Match rate |%n"
                    .formatted());
            report.append("| --- | ---: | ---: | ---: | ---: |%n".formatted());
            for (StreamResult stream : result.streams()) {
                report.append("| %s | %d | %d | %d | %s |%n".formatted(
                        stream.name(), stream.compared(), stream.matched(), stream.mismatched(),
                        percentage(stream.matched(), stream.compared())));
            }
            report.append("| **Total** | **%d** | **%d** | **%d** | **%s** |%n%n".formatted(
                    result.compared(), result.matched(), result.compared() - result.matched(),
                    percentage(result.matched(), result.compared())));
        }

        int totalCompared = RESULTS.stream().mapToInt(ScenarioResult::compared).sum();
        int totalMatched = RESULTS.stream().mapToInt(ScenarioResult::matched).sum();
        report.append("""
                ## Overall

                | Records compared | Matched | Mismatched | Match rate |
                | ---: | ---: | ---: | ---: |
                """);
        report.append("| %d | %d | %d | %s |%n%n".formatted(
                totalCompared, totalMatched, totalCompared - totalMatched,
                percentage(totalMatched, totalCompared)));

        report.append("""
                ## Notes on the populations

                * The shipped `DALYTRAN` file exercises only one rejection reason (102, over
                  limit): every card in it resolves, every account exists and none has expired.
                  It also carries only positive amounts, so the cycle-to-date debit bucket is
                  never touched. It does prove the control flow, the creation of category
                  balances (its type/category `03/0001` transactions have no bucket yet) and the
                  record formatting of all four output streams.
                * The derived scenario patches the real datasets — it never invents a record — so
                  that the remaining branches are reached: unknown card (100), an account id the
                  master does not contain (101), expired accounts (103, which also demonstrates
                  that 103 overrides an already-failed 102), refunds driving the debit bucket and
                  the negative sign overpunch, transactions in a category the account has never
                  used, and repeated maximum amounts that push balances past the capacity of the
                  receiving COBOL fields so that truncation is compared, not just arithmetic.
                * Defects are reproduced, not fixed: see rules BR-4a, BR-6a and BR-10a of the
                  logic map. Parity means matching the mainframe, including where it is wrong.
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
