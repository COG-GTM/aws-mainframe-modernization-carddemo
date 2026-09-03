package com.carddemo.interest.batch;

import com.carddemo.interest.InterestProperties;
import com.carddemo.interest.domain.InterestPostingRun;
import com.carddemo.interest.domain.InterestRateLookup;
import com.carddemo.interest.domain.InterestTransactionFactory;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.AccountLayout;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.CardXrefLayout;
import com.carddemo.recordio.layout.DisclosureGroup;
import com.carddemo.recordio.layout.DisclosureGroupLayout;
import com.carddemo.recordio.layout.TransactionCategoryBalance;
import com.carddemo.recordio.layout.TransactionCategoryBalanceLayout;
import com.carddemo.recordio.layout.TransactionLayout;
import com.carddemo.recordio.store.FixedWidthFile;
import com.carddemo.recordio.store.KeyedRecordStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

import java.time.Clock;

/**
 * Job {@code monthlyInterestJob} = INTCALC STEP15. Implemented as a single tasklet rather than a
 * chunk step because the unit of work is an account (control break over the ordered category
 * balances), not a record, and the account update at end of file depends on all its records.
 */
@Configuration
public class InterestJobConfiguration {

    public static final String JOB_NAME = "monthlyInterestJob";
    private static final Logger LOG = LoggerFactory.getLogger(InterestJobConfiguration.class);

    @Bean
    public Clock interestClock() {
        return Clock.systemDefaultZone();
    }

    @Bean
    public Tasklet interestTasklet(InterestProperties p, Clock interestClock) {
        return (StepContribution contribution, ChunkContext chunkContext) -> {
            RecordEncoding enc = RecordEncoding.of(p.encoding());
            KeyedRecordStore<TransactionCategoryBalance> balances = KeyedRecordStore.load("TCATBALF",
                    p.categoryBalances(), TransactionCategoryBalanceLayout.INSTANCE, enc, TransactionCategoryBalance::key);
            KeyedRecordStore<CardXref> xref = KeyedRecordStore.load("XREFFILE", p.cardXref(),
                    CardXrefLayout.INSTANCE, enc, CardXref::cardNumber);
            KeyedRecordStore<Account> accounts = KeyedRecordStore.load("ACCTFILE", p.accountMaster(),
                    AccountLayout.INSTANCE, enc, Account::accountId);
            KeyedRecordStore<DisclosureGroup> groups = KeyedRecordStore.load("DISCGRP", p.disclosureGroups(),
                    DisclosureGroupLayout.INSTANCE, enc, DisclosureGroup::key);

            InterestPostingRun run = new InterestPostingRun(accounts, xref, new InterestRateLookup(groups),
                    new InterestTransactionFactory(p.runDate(), interestClock));
            balances.readAll().forEach(run::accept);
            run.finish();

            accounts.save(p.accountMaster());
            FixedWidthFile.write(p.systemTransactions(), run.systemTransactions().stream()
                    .map(t -> TransactionLayout.INSTANCE.encode(t, enc)).toList());
            LOG.info("category balances read: {}, interest transactions written: {}", run.recordCount(),
                    run.systemTransactions().size());
            contribution.incrementReadCount();
            return RepeatStatus.FINISHED;
        };
    }

    @Bean
    public Step computeInterestStep(JobRepository jobRepository, PlatformTransactionManager tm, Tasklet interestTasklet) {
        return new StepBuilder("computeInterest", jobRepository).tasklet(interestTasklet, tm).build();
    }

    @Bean
    public Job monthlyInterestJob(JobRepository jobRepository, Step computeInterestStep) {
        return new JobBuilder(JOB_NAME, jobRepository).start(computeInterestStep).build();
    }
}
