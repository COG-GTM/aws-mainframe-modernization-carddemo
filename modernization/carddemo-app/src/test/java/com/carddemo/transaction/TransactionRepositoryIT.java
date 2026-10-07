package com.carddemo.transaction;

import static com.carddemo.support.BrowseAssertions.assertFullBrowse;
import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.support.PostgresRepositoryTest;
import com.carddemo.support.Samples;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Limit;

/**
 * TRANSACT access paths, including the COTRN00C ten-row browse and the TRAN-PROC-TS AIX. TRANSACT ships empty, so
 * the posted file is built from the DALYTRAN sample the way CBTRN02C posts it (first occurrence of each id).
 */
class TransactionRepositoryIT extends PostgresRepositoryTest {

    @Autowired
    TransactionRepository transactions;

    List<TransactionRecord> posted;

    @BeforeEach
    void load() {
        Map<String, TransactionRecord> byId = new LinkedHashMap<>();
        DailyTransactionRecord.MAPPER.fromRecords(Samples.read(Dataset.DALYTRAN, RecordEncoding.EBCDIC))
                .forEach(d -> byId.putIfAbsent(d.tranId(), d.asTransaction()));
        posted = new ArrayList<>(byId.values());
        transactions.saveAll(posted.stream().map(Transaction::from).toList());
        flushAndClear();
    }

    @Test
    void readsByKey() {
        TransactionRecord any = posted.get(42);
        assertThat(transactions.findById(any.tranId()).orElseThrow().toRecord()).isEqualTo(any);
        assertThat(transactions.findById("NOSUCHTRAN")).isEmpty();
    }

    @Test
    void browsesTenTransactionsPerScreenInTranIdOrder() {
        List<String> ids = posted.stream().map(TransactionRecord::tranId).sorted().toList();
        assertThat(ids.size()).isGreaterThan(20);
        assertFullBrowse(ids, TransactionRepository.COTRN00C_SCREEN_ROWS, Transaction::getTranId,
                transactions::browseFrom, transactions::nextPage, transactions::previousPage);
        assertThat(transactions.findAllByOrderByTranIdAsc()).extracting(Transaction::getTranId)
                .containsExactlyElementsOf(ids);
    }

    @Test
    void highestTranIdIsTheReadprevFromHighValues() {
        String max = posted.stream().map(TransactionRecord::tranId).max(Comparator.naturalOrder()).orElseThrow();
        assertThat(transactions.findFirstByOrderByTranIdDesc().orElseThrow().getTranId()).isEqualTo(max);
    }

    @Test
    void procTsAixBrowsesInProcTsThenTranIdOrder() {
        List<TransactionRecord> expected = posted.stream()
                .sorted(Comparator.comparing(TransactionRecord::procTs).thenComparing(TransactionRecord::tranId))
                .toList();
        List<TransactionRecord> walked = new ArrayList<>();
        List<Transaction> page = transactions.findByProcTsGreaterThanEqualOrderByProcTsAscTranIdAsc("",
                Limit.of(10));
        while (!page.isEmpty()) {
            page.forEach(t -> walked.add(t.toRecord()));
            Transaction last = page.get(page.size() - 1);
            page = transactions.findAfterProcTs(last.getProcTs(), last.getTranId(), Limit.of(10));
        }
        assertThat(walked).containsExactlyElementsOf(expected);

        String start = expected.get(expected.size() / 2).procTs();
        assertThat(transactions.findByProcTsGreaterThanEqualOrderByProcTsAscTranIdAsc(start, Limit.of(1000)))
                .extracting(Transaction::toRecord)
                .containsExactlyElementsOf(expected.stream().filter(t -> t.procTs().compareTo(start) >= 0).toList());
    }
}
