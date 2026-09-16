package com.carddemo.report.domain;

import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.CardXrefLayout;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionCategory;
import com.carddemo.recordio.layout.TransactionCategoryLayout;
import com.carddemo.recordio.layout.TransactionType;
import com.carddemo.recordio.layout.TransactionTypeLayout;
import com.carddemo.recordio.store.KeyedRecordStore;
import com.carddemo.recordio.store.RecordNotFoundException;
import com.carddemo.report.ReportProperties.OutOfRangePolicy;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/** CBTRN03C main loop, 1100-1120 and 1500-A/B/C (lines 159-375, 484-513). */
class TransactionReportWriterTest {

    private static final RecordEncoding ENC = RecordEncoding.EBCDIC;
    private static final String CARD_A = "4000000000000001";
    private static final String CARD_B = "4000000000000002";
    private static final ReportDateRange RANGE = new ReportDateRange("2022-01-01", "2022-07-06");

    private final ReportLookups lookups = new ReportLookups(
            KeyedRecordStore.of("CARDXREF", List.of(new CardXref(CARD_A, 1L, "00000000001"), new CardXref(CARD_B, 2L, "00000000002")),
                    CardXrefLayout.INSTANCE, ENC, CardXref::cardNumber),
            KeyedRecordStore.of("TRANTYPE", List.of(new TransactionType("01", "Purchase")), TransactionTypeLayout.INSTANCE, ENC, TransactionType::typeCode),
            KeyedRecordStore.of("TRANCATG", List.of(new TransactionCategory("01", 1, "Regular Sales Draft")),
                    TransactionCategoryLayout.INSTANCE, ENC, TransactionCategory::key));

    private static Transaction tran(String id, String card, String amount, String procDate) {
        return new Transaction(id, "01", 1, "POS TERM", "d", new BigDecimal(amount), 1L, "m", "c", "z", card,
                "2022-06-01-00.00.00.000000", procDate + "-00.00.00.000000");
    }

    private static Transaction tran(String id, String card, String amount) {
        return tran(id, card, amount, "2022-06-15");
    }

    private List<String> run(List<Transaction> input, OutOfRangePolicy policy) {
        return new TransactionReportWriter(RANGE, lookups, policy).write(input);
    }

    @Test
    void everyLineIs133CharactersAndTheDetailLineMatchesCvtra07y() {
        List<String> lines = run(List.of(tran("0000000000000001", CARD_A, "504.77")), OutOfRangePolicy.STOP_LIKE_COBOL);
        assertThat(lines).allSatisfy(l -> assertThat(l).hasSize(133));
        assertThat(lines.get(0)).startsWith("DALYREPT");
        assertThat(lines.get(0)).contains("Date Range: 2022-01-01 to 2022-07-06");
        assertThat(lines.get(4)).startsWith(
                "0000000000000001 00000000001 01-Purchase        0001-Regular Sales Draft           POS TERM               504.77  ");
    }

    @Test
    void singleRecordRunCountsTheLastAmountTwiceInPageAndGrandTotal() {
        // documented CBTRN03C behaviour (lines 203-208), recorded in open-questions.md
        List<String> lines = run(List.of(tran("T1", CARD_A, "100.00")), OutOfRangePolicy.STOP_LIKE_COBOL);
        assertThat(lines.get(5)).startsWith("Page Total").contains("+200.00").hasSize(133);
        assertThat(lines.get(7)).startsWith("Grand Total").contains("+200.00").hasSize(133);
        assertThat(lines).noneMatch(l -> l.startsWith("Account Total"));
    }

    @Test
    void cardChangeWritesAccountTotalBeforeNextDetailButNeverForTheLastCard() {
        List<String> lines = run(List.of(tran("T1", CARD_A, "10.00"), tran("T2", CARD_B, "20.00")), OutOfRangePolicy.STOP_LIKE_COBOL);
        assertThat(lines.get(4)).startsWith("T1");
        assertThat(lines.get(5)).startsWith("Account Total").contains("+10.00").hasSize(133);
        assertThat(lines.get(7)).startsWith("T2");
        assertThat(lines.get(8)).startsWith("Page Total").contains("+50.00").hasSize(133);   // 10 + 20 + 20 (last read again)
        assertThat(lines).filteredOn(l -> l.startsWith("Account Total")).hasSize(1);
    }

    @Test
    void pageBreaksEveryTwentyLinesCountingHeadersAndTotals() {
        List<Transaction> many = IntStream.rangeClosed(1, 40)
                .mapToObj(i -> tran(String.format("T%015d", i), CARD_A, "1.00")).toList();
        List<String> lines = run(many, OutOfRangePolicy.STOP_LIKE_COBOL);
        List<Integer> pageTotalIdx = new ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            if (lines.get(i).startsWith("Page Total")) {
                pageTotalIdx.add(i);
            }
        }
        // first 20 lines: 4 header + 16 details; then page total, rule, 4 headers, 14 details = 20 ...
        assertThat(pageTotalIdx.get(0)).isEqualTo(20);
        assertThat(pageTotalIdx.get(1)).isEqualTo(40);
        assertThat(lines.get(20)).contains("+16.00").hasSize(133);
        assertThat(lines.get(40)).contains("+14.00").hasSize(133);
        assertThat(lines.get(lines.size() - 1)).startsWith("Grand Total").contains("+41.00").hasSize(133); // 40 + last again
    }

    @Test
    void recordsOutsideDateParmRangeStopTheRunLikeTheCobolOrAreSkippedByPolicy() {
        List<Transaction> in = List.of(tran("T1", CARD_A, "10.00"), tran("T2", CARD_A, "20.00", "2022-12-31"), tran("T3", CARD_A, "30.00"));
        List<String> stop = run(in, OutOfRangePolicy.STOP_LIKE_COBOL);
        assertThat(stop).hasSize(5);                       // headers + T1, then nothing
        assertThat(stop).noneMatch(l -> l.startsWith("Grand Total"));

        List<String> skip = run(in, OutOfRangePolicy.SKIP_RECORD);
        assertThat(skip).anyMatch(l -> l.startsWith("T3"));
        assertThat(skip).noneMatch(l -> l.startsWith("T2"));
        // lastRead is the skipped T2 (30 not last); COBOL adds the last *read* amount, i.e. T3's 30
        assertThat(skip.get(skip.size() - 1)).startsWith("Grand Total").contains("+70.00").hasSize(133);
    }

    @Test
    void boundaryDatesAreInclusive() {
        List<String> lines = run(List.of(tran("T1", CARD_A, "1.00", "2022-01-01"), tran("T2", CARD_A, "1.00", "2022-07-06")),
                OutOfRangePolicy.STOP_LIKE_COBOL);
        assertThat(lines).anyMatch(l -> l.startsWith("T1")).anyMatch(l -> l.startsWith("T2"));
    }

    @Test
    void unknownCardTypeOrCategoryIsFatal() {
        assertThatThrownBy(() -> run(List.of(tran("T1", "4999999999999999", "1.00")), OutOfRangePolicy.STOP_LIKE_COBOL))
                .isInstanceOf(RecordNotFoundException.class).hasMessageContaining("INVALID CARD NUMBER");
        Transaction badType = new Transaction("T1", "99", 1, "s", "d", BigDecimal.ONE, 1L, "m", "c", "z", CARD_A,
                "2022-06-01-00.00.00.000000", "2022-06-15-00.00.00.000000");
        assertThatThrownBy(() -> run(List.of(badType), OutOfRangePolicy.STOP_LIKE_COBOL))
                .isInstanceOf(RecordNotFoundException.class).hasMessageContaining("INVALID TRANSACTION TYPE");
        Transaction badCat = new Transaction("T1", "01", 9, "s", "d", BigDecimal.ONE, 1L, "m", "c", "z", CARD_A,
                "2022-06-01-00.00.00.000000", "2022-06-15-00.00.00.000000");
        assertThatThrownBy(() -> run(List.of(badCat), OutOfRangePolicy.STOP_LIKE_COBOL))
                .isInstanceOf(RecordNotFoundException.class).hasMessageContaining("INVALID TRAN CATG KEY");
    }

    @Test
    void sortStepFiltersByProcessingDateAndOrdersByCardNumber() {
        List<Transaction> sorted = TransactionSelector.selectAndSort(List.of(
                tran("T1", CARD_B, "1.00"), tran("T2", CARD_A, "1.00", "2021-12-31"), tran("T3", CARD_A, "1.00")), RANGE);
        assertThat(sorted).extracting(Transaction::id).containsExactly("T3", "T1");
    }
}
