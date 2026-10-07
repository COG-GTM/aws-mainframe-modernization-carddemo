package com.carddemo.account;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.support.PostgresRepositoryTest;
import com.carddemo.support.Samples;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/** ACCTDATA access paths: read by ACCT-ID, rewrite, sequential in key order. */
class AccountRepositoryIT extends PostgresRepositoryTest {

    @Autowired
    AccountRepository accounts;

    List<AccountRecord> sample;

    @BeforeEach
    void load() {
        loadSamples(Dataset.ACCTDATA);
        sample = AccountRecord.MAPPER.fromRecords(Samples.read(Dataset.ACCTDATA, RecordEncoding.EBCDIC));
    }

    @Test
    void readsByKeyWithGeneratedDatesAndExactDecimals() {
        AccountRecord any = sample.get(4);
        Account account = accounts.findById(any.acctId()).orElseThrow();
        assertThat(account.toRecord()).isEqualTo(any);
        assertThat(account.getCurrBal().scale()).isEqualTo(2);
        assertThat(account.getOpenDateDt()).isEqualTo(LocalDate.parse(any.openDate()));
        assertThat(account.getExpirationDateDt()).isEqualTo(LocalDate.parse(any.expirationDate()));
        assertThat(account.getReissueDateDt()).isEqualTo(LocalDate.parse(any.reissueDate()));
        assertThat(account.getActiveStatus()).isIn(AccountStatus.ACTIVE, AccountStatus.INACTIVE);
        assertThat(accounts.findById(99_999_999_999L)).isEmpty();
    }

    @Test
    void readsSequentiallyInKeyOrder() {
        assertThat(accounts.findAllByOrderByAcctIdAsc()).extracting(Account::toRecord).containsExactlyElementsOf(
                sample.stream().sorted(Comparator.comparingLong(AccountRecord::acctId)).toList());
    }

    @Test
    void rewriteBumpsTheVersion() {
        AccountRecord any = sample.get(0);
        Account account = accounts.findById(any.acctId()).orElseThrow();
        AccountRecord changed = new AccountRecord(any.acctId(), AccountStatus.INACTIVE, new BigDecimal("-12.34"),
                any.creditLimit(), any.cashCreditLimit(), any.openDate(), "2099-12-31", any.reissueDate(),
                new BigDecimal("1.00"), new BigDecimal("2.00"), any.addrZip(), any.groupId());
        account.update(changed);
        accounts.saveAndFlush(account);
        entityManager.clear();
        Account reread = accounts.findById(any.acctId()).orElseThrow();
        assertThat(reread.toRecord()).isEqualTo(changed);
        assertThat(reread.getExpirationDateDt()).isEqualTo(LocalDate.of(2099, 12, 31));
        assertThat(reread.getVersion()).isEqualTo(1);
    }
}
