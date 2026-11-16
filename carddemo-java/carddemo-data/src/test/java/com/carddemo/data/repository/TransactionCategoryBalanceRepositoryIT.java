package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.TransactionCategoryBalance;
import com.carddemo.data.entity.TransactionCategoryBalanceId;
import java.math.BigDecimal;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class TransactionCategoryBalanceRepositoryIT extends RepositoryIT {

    @Autowired
    TransactionCategoryBalanceRepository balances;

    @Test
    void readsByCompositeKeyAndWalksInKeyOrder() {
        save(2L, "01", 1, "10.00");
        save(1L, "02", 1, "-5.25");
        save(1L, "01", 5, "7.00");
        save(1L, "01", 1, "0.00");
        flushAndClear();

        var id = new TransactionCategoryBalanceId(1L, "02", (short) 1);
        assertThat(balances.findById(id)).get()
                .extracting(TransactionCategoryBalance::getBalance).isEqualTo(new BigDecimal("-5.25"));
        assertThat(balances.findAllByOrderByIdAcctIdAscIdTypeCdAscIdCatCdAsc())
                .extracting(TransactionCategoryBalance::getId)
                .containsExactly(
                        new TransactionCategoryBalanceId(1L, "01", (short) 1),
                        new TransactionCategoryBalanceId(1L, "01", (short) 5),
                        new TransactionCategoryBalanceId(1L, "02", (short) 1),
                        new TransactionCategoryBalanceId(2L, "01", (short) 1));
        assertThat(balances.findByIdAcctIdOrderByIdTypeCdAscIdCatCdAsc(2L)).hasSize(1);

        var row = balances.findById(id).orElseThrow();
        row.setBalance(new BigDecimal("999999999.99"));
        flushAndClear();
        assertThat(balances.findById(id).orElseThrow().getBalance()).isEqualTo(new BigDecimal("999999999.99"));
    }

    private void save(long acctId, String typeCd, int catCd, String balance) {
        var b = new TransactionCategoryBalance();
        b.setId(new TransactionCategoryBalanceId(acctId, typeCd, (short) catCd));
        b.setBalance(new BigDecimal(balance));
        balances.save(b);
    }
}
