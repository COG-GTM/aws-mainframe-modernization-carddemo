package com.carddemo.transaction;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.support.PostgresRepositoryTest;
import com.carddemo.support.Samples;
import java.math.BigDecimal;
import java.util.Comparator;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/** TRANTYPE, TRANCATG, DISCGRP, TCATBALF and DALYTRAN access paths. */
class ReferenceDataRepositoryIT extends PostgresRepositoryTest {

    @Autowired
    TransactionTypeRepository types;

    @Autowired
    TransactionCategoryRepository categories;

    @Autowired
    DisclosureGroupRepository disclosureGroups;

    @Autowired
    TranCatBalanceRepository balances;

    @Autowired
    DailyTransactionRepository dailyTransactions;

    @BeforeEach
    void load() {
        loadSamples(Dataset.TRANTYPE, Dataset.TRANCATG, Dataset.DISCGRP, Dataset.TCATBALF, Dataset.DALYTRAN);
        jdbc.execute("set constraints all immediate");
    }

    static <D extends Record> List<D> sample(Dataset dataset, Class<D> type) {
        return dataset.mapper().fromRecords(Samples.read(dataset, RecordEncoding.EBCDIC)).stream().map(type::cast)
                .toList();
    }

    @Test
    void transactionTypesByKeyAndInKeyOrder() {
        List<TransactionTypeRecord> sample = sample(Dataset.TRANTYPE, TransactionTypeRecord.class);
        assertThat(types.findById(sample.get(0).tranTypeCd()).orElseThrow().toRecord()).isEqualTo(sample.get(0));
        assertThat(types.findAllByOrderByTranTypeCdAsc()).extracting(TransactionType::toRecord)
                .containsExactlyElementsOf(sample.stream().sorted(Comparator.comparing(
                        TransactionTypeRecord::tranTypeCd)).toList());
    }

    @Test
    void transactionCategoriesByCompositeKeyAndInKeyOrder() {
        List<TransactionCategoryRecord> sample = sample(Dataset.TRANCATG, TransactionCategoryRecord.class);
        TransactionCategoryRecord any = sample.get(5);
        assertThat(categories.findById(new TransactionCategoryId(any.tranTypeCd(), any.tranCatCd())).orElseThrow()
                .toRecord()).isEqualTo(any);
        assertThat(categories.findById(new TransactionCategoryId("99", 9999))).isEmpty();
        assertThat(categories.findAllInKeyOrder()).extracting(TransactionCategory::toRecord)
                .containsExactlyElementsOf(sample.stream().sorted(Comparator.comparing(
                        TransactionCategoryRecord::tranTypeCd).thenComparingInt(TransactionCategoryRecord::tranCatCd))
                        .toList());
    }

    @Test
    void disclosureGroupFallsBackToDefaultLikeCbact04c() {
        List<DisclosureGroupRecord> sample = sample(Dataset.DISCGRP, DisclosureGroupRecord.class);
        DisclosureGroupRecord own = sample.stream().filter(d -> d.acctGroupId().equals("A000000000")).findFirst()
                .orElseThrow();
        DisclosureGroupRecord fallback = sample.stream().filter(d -> d.acctGroupId().equals("DEFAULT")
                && d.tranTypeCd().equals(own.tranTypeCd()) && d.tranCatCd() == own.tranCatCd()).findFirst()
                .orElseThrow();

        assertThat(disclosureGroups.findWithDefault("A000000000", own.tranTypeCd(), own.tranCatCd()).orElseThrow()
                .toRecord()).isEqualTo(own);
        assertThat(disclosureGroups.findWithDefault("NOSUCHGRP", own.tranTypeCd(), own.tranCatCd()).orElseThrow()
                .toRecord()).isEqualTo(fallback);
        assertThat(disclosureGroups.findWithDefault("NOSUCHGRP", "99", 9999)).isEmpty();
        assertThat(disclosureGroups.findAllInKeyOrder()).extracting(d -> d.getId().acctGroupId())
                .isSortedAccordingTo(Comparator.naturalOrder());
    }

    @Test
    void categoryBalancesInKeyOrderAndRewrite() {
        List<TranCatBalanceRecord> sample = sample(Dataset.TCATBALF, TranCatBalanceRecord.class);
        assertThat(balances.findAllInKeyOrder()).extracting(TranCatBalance::toRecord).containsExactlyElementsOf(
                sample.stream().sorted(Comparator.comparingLong(TranCatBalanceRecord::acctId)
                        .thenComparing(TranCatBalanceRecord::tranTypeCd)
                        .thenComparingInt(TranCatBalanceRecord::tranCatCd)).toList());

        TranCatBalanceRecord any = sample.get(3);
        TranCatBalanceId id = new TranCatBalanceId(any.acctId(), any.tranTypeCd(), any.tranCatCd());
        TranCatBalance balance = balances.findById(id).orElseThrow();
        TranCatBalanceRecord changed = new TranCatBalanceRecord(any.acctId(), any.tranTypeCd(), any.tranCatCd(),
                any.balance().add(new BigDecimal("10.05")));
        balance.update(changed);
        balances.saveAndFlush(balance);
        entityManager.clear();
        assertThat(balances.findById(id).orElseThrow().toRecord()).isEqualTo(changed);
    }

    @Test
    void dailyTransactionsReadBackInFileOrderAndMayRepeatAnId() {
        List<DailyTransactionRecord> sample = sample(Dataset.DALYTRAN, DailyTransactionRecord.class);
        List<DailyTransaction> rows = dailyTransactions.findAllByOrderByRecordSeqAsc();
        assertThat(rows).extracting(DailyTransaction::toRecord).containsExactlyElementsOf(sample);
        assertThat(rows).extracting(DailyTransaction::getRecordSeq).first().isEqualTo(1);

        DailyTransactionRecord repeated = sample.get(7);
        dailyTransactions.save(DailyTransaction.from(sample.size() + 1, repeated));
        flushAndClear();
        List<DailyTransaction> same = dailyTransactions.findByTranIdOrderByRecordSeqAsc(repeated.tranId());
        assertThat(same).extracting(DailyTransaction::getRecordSeq).containsExactly(8, sample.size() + 1);
        assertThat(same).extracting(DailyTransaction::toRecord).containsOnly(repeated);
    }
}
