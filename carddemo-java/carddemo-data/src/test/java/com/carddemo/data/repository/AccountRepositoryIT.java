package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.time.LocalDate;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class AccountRepositoryIT extends RepositoryIT {

    @Autowired
    AccountRepository accounts;

    @Test
    void readsAndRewritesByAcctId() {
        accounts.save(Fixtures.account(1L));
        flushAndClear();

        var account = accounts.findById(1L).orElseThrow();
        assertThat(account.getCurrBal()).isEqualByComparingTo("194.00");
        assertThat(account.getOpenDate()).isEqualTo(LocalDate.of(2014, 11, 20));
        assertThat(account.getGroupId()).isEqualTo("DEFAULT");

        account.setCurrBal(new BigDecimal("-9999999999.99"));
        flushAndClear();

        assertThat(accounts.findById(1L).orElseThrow().getCurrBal()).isEqualTo(new BigDecimal("-9999999999.99"));
        assertThat(accounts.findById(2L)).isEmpty();
    }
}
