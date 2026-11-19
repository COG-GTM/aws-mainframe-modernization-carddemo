package com.carddemo.transaction;

import com.carddemo.app.PostgresTestBase;
import com.carddemo.app.golden.GoldenCase;
import com.carddemo.app.golden.GoldenCompare;
import com.carddemo.app.golden.TableUnloader;
import com.carddemo.common.cobol.CodePage;
import com.carddemo.transaction.batch.TransactionRecordLayout;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class TransactionPersistenceTest extends PostgresTestBase {

    @Autowired
    TransactionRepository transactions;

    @BeforeEach
    void clean() {
        truncateBusinessTables();
    }

    private static Transaction fromLine(String line, CodePage codePage) {
        String padded = line + " ".repeat(TransactionRecordLayout.LENGTH - line.length());
        return TransactionRecordLayout.toEntity(TransactionRecordLayout.LAYOUT.wrap(codePage.encode(padded), codePage));
    }

    /** Every oracle TRANSACT unload survives entity -> table -> unload unchanged, so the masked compare is exact too. */
    @Test
    void oracleTransactUnloadsRoundTripThroughTheTable() {
        int compared = 0;
        for (GoldenCase gc : GoldenCase.all()) {
            for (var unload : List.of(gc.expected("TRANSACT.unl"), gc.expectedTable("TRANSACT.unl"))) {
                truncateBusinessTables();
                List<String> expected = GoldenCompare.lines(unload);
                transactions.saveAll(expected.stream().map(l -> fromLine(l, CodePage.ASCII)).toList());
                List<String> actual = TableUnloader.transactions(transactions);
                assertThat(actual).as(gc.name()).isEqualTo(expected);
                assertThat(GoldenCompare.compareUnloads(expected, actual, 350, GoldenCompare.TRANSACT_MASK)).isEmpty();
                compared += expected.size();
            }
        }
        assertThat(compared).isGreaterThan(500);
    }

    /** D-015: both layouts in either field, and text that parses in neither, come back byte for byte. */
    @Test
    void timestampTextsAreStoredByteForByte() throws Exception {
        String sample = Files.readAllLines(GoldenCase.of("GC-1").expected("TRANSACT.unl")).get(0);
        Map<String, String[]> cases = Map.of(
                "MIX0000000000001", new String[] {"2022-06-10 19:27:53.000000", "2022-07-18-10.30.45.570000"},
                "MIX0000000000002", new String[] {"2022-06-10-19.27.53.120000", "2022-07-18 10:30:45.000000"},
                "MIX0000000000003", new String[] {"2022-06-10 19:27:53       ", "                          "});
        cases.forEach((id, ts) -> transactions.save(fromLine(id + sample.substring(16, 262) + ts[0] + ts[1], CodePage.ASCII)));

        assertThat(jdbc.queryForList("SELECT tran_id, orig_ts, proc_ts FROM transaction.transaction ORDER BY tran_id"))
                .extracting(r -> r.get("orig_ts") == null, r -> r.get("proc_ts") == null)
                .containsExactly(org.assertj.core.groups.Tuple.tuple(false, false),
                        org.assertj.core.groups.Tuple.tuple(false, false), org.assertj.core.groups.Tuple.tuple(true, true));
        assertThat(transactions.findById("MIX0000000000002")).get().extracting(Transaction::getOrigTs)
                .isEqualTo(LocalDateTime.of(2022, 6, 10, 19, 27, 53, 120_000_000));
        for (String line : TableUnloader.transactions(transactions)) {
            String[] ts = cases.get(line.substring(0, 16));
            String padded = line + " ".repeat(350 - line.length());
            assertThat(padded.substring(262, 314)).isEqualTo(ts[0] + ts[1]);
        }
    }

    @Test
    void cp037RecordsRoundTrip() {
        List<String> lines = GoldenCompare.lines(GoldenCase.of("GC-7").expected("TRANSACT.unl"));
        for (String line : lines.subList(0, 20)) {
            byte[] cp037 = CodePage.CP037.encode(line + " ".repeat(350 - line.length()));
            Transaction t = fromLine(line, CodePage.CP037);
            assertThat(TransactionRecordLayout.toRecord(t, CodePage.CP037).bytes()).isEqualTo(cp037);
        }
    }
}
