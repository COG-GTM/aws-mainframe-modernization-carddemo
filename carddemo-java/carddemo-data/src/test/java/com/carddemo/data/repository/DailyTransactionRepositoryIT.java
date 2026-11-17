package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.DailyTransaction;
import java.math.BigDecimal;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class DailyTransactionRepositoryIT extends RepositoryIT {

    @Autowired
    DailyTransactionRepository daily;

    @Test
    void keepsInputFileOrderIncludingDuplicateIds() {
        daily.save(Fixtures.dailyTransaction("0000000001774260", new BigDecimal("-91.90")));
        daily.save(Fixtures.dailyTransaction("0000000000683580", new BigDecimal("50.47")));
        daily.save(Fixtures.dailyTransaction("0000000001774260", new BigDecimal("1.00")));
        flushAndClear();

        var rows = daily.findAllByOrderBySeqNo();
        assertThat(rows).extracting(DailyTransaction::getTranId)
                .containsExactly("0000000001774260", "0000000000683580", "0000000001774260");
        assertThat(rows).extracting(DailyTransaction::getAmt)
                .containsExactly(new BigDecimal("-91.90"), new BigDecimal("50.47"), new BigDecimal("1.00"));
        assertThat(rows.get(0).getSeqNo()).isLessThan(rows.get(1).getSeqNo());
        assertThat(rows.get(0).getProcTs()).isNull();
    }
}
