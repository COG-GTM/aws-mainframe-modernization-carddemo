package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.Transaction;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;

class TransactionRepositoryIT extends RepositoryIT {

    @Autowired
    TransactionRepository transactions;

    @Test
    void readsByTranIdAndFindsTheHighestId() {
        assertThat(transactions.findTopByOrderByTranIdDesc()).isEmpty();

        transactions.save(Fixtures.transaction("0000000000683580", null));
        transactions.save(Fixtures.transaction("0000000001774260", null));
        transactions.save(Fixtures.transaction("0000000000000001", null));
        flushAndClear();

        var t = transactions.findById("0000000000683580").orElseThrow();
        assertThat(t.getAmt()).isEqualByComparingTo("50.47");
        assertThat(t.getOrigTs()).isEqualTo(LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        assertThat(t.getProcTs()).isNull();
        assertThat(transactions.findTopByOrderByTranIdDesc())
                .get().extracting(Transaction::getTranId).isEqualTo("0000000001774260");
        assertThat(transactions.findByTranIdGreaterThanEqualOrderByTranId("0000000000000002", PageRequest.ofSize(10)))
                .extracting(Transaction::getTranId)
                .containsExactly("0000000000683580", "0000000001774260");
    }

    @Test
    void readsThroughTheProcTsAlternateIndex() {
        var day = LocalDateTime.of(2022, 7, 18, 0, 0);
        transactions.save(Fixtures.transaction("T3", day.plusHours(12).plusNanos(123_456_000)));
        transactions.save(Fixtures.transaction("T1", day.plusHours(1)));
        transactions.save(Fixtures.transaction("T2", day.plusHours(1)));
        transactions.save(Fixtures.transaction("T4", day.plusDays(1)));
        transactions.save(Fixtures.transaction("T0", day.minusNanos(1_000)));
        flushAndClear();

        assertThat(transactions.findByProcTsGreaterThanEqualAndProcTsLessThanOrderByProcTsAscTranIdAsc(
                        day, day.plusDays(1)))
                .extracting(Transaction::getTranId)
                .containsExactly("T1", "T2", "T3");
        assertThat(transactions.findById("T3").orElseThrow().getProcTs().getNano()).isEqualTo(123_456_000);
    }
}
