package com.carddemo.batch.jobs.intcalc;

import com.carddemo.batch.io.CardDemoData;
import com.carddemo.batch.io.FixedWidthItemIO;
import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.CardXrefRecord;
import com.carddemo.batch.model.DisclosureGroupRecord;
import com.carddemo.batch.model.TranCatBalRecord;
import com.carddemo.batch.model.TransactionRecord;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.file.FlatFileItemReader;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.FileSystemResource;
import org.springframework.transaction.PlatformTransactionManager;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;

/**
 * {@code intcalcJob}: Spring Batch port of {@code CBACT04C} / JCL {@code INTCALC}.
 *
 * <p>Job parameters: {@code parmDate} (the JCL PARM, default {@code 2022071800}), {@code inputDir} (folder
 * with tcatbal.txt, cardxref.txt, acctdata.txt, discgrp.txt; default {@code app/data/ASCII}) and
 * {@code outputDir} (receives {@code transact.dat} and the rewritten {@code acctdata.txt}; default
 * {@code target/intcalc}). Timestamps come from the application's {@link Clock} bean, else the system clock.
 */
@Configuration
public class IntcalcJobConfig {

    public static final String JOB_NAME = "intcalcJob";
    public static final String DEFAULT_PARM_DATE = "2022071800";
    public static final String TRANSACT_FILE = "transact.dat";

    @Bean
    public Job intcalcJob(JobRepository jobRepository, Step intcalcStep) {
        return new JobBuilder(JOB_NAME, jobRepository).preventRestart().start(intcalcStep).build();
    }

    @Bean
    public Step intcalcStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                            FlatFileItemReader<TranCatBalRecord> intcalcTcatBalReader,
                            IntcalcItemProcessor intcalcProcessor,
                            FlatFileItemWriter<TransactionRecord> intcalcTransactionWriter) {
        return new StepBuilder("intcalcStep", jobRepository)
                .<TranCatBalRecord, TransactionRecord>chunk(100, transactionManager)
                .reader(intcalcTcatBalReader)
                .processor(intcalcProcessor)
                .writer(intcalcTransactionWriter)
                .listener(intcalcProcessor)
                .build();
    }

    @Bean
    @StepScope
    public FlatFileItemReader<TranCatBalRecord> intcalcTcatBalReader(
            @Value("#{jobParameters['inputDir']}") String inputDir) {
        return FixedWidthItemIO.reader("intcalcTcatBalReader",
                new FileSystemResource(input(inputDir).resolve(CardDemoData.TRAN_CAT_BALANCES)),
                TranCatBalRecord.CODEC);
    }

    @Bean
    @StepScope
    public IntcalcItemProcessor intcalcProcessor(@Value("#{jobParameters['inputDir']}") String inputDir,
                                                 @Value("#{jobParameters['outputDir']}") String outputDir,
                                                 @Value("#{jobParameters['parmDate']}") String parmDate,
                                                 ObjectProvider<Clock> clock) {
        Path in = input(inputDir);
        KsdsFile<Long, AccountRecord> accounts =
                KsdsFile.load(in.resolve(CardDemoData.ACCOUNTS), AccountRecord.CODEC, AccountRecord::acctId);
        KsdsFile<String, CardXrefRecord> xrefs =
                KsdsFile.load(in.resolve(CardDemoData.CARD_XREF), CardXrefRecord.CODEC, CardXrefRecord::xrefCardNum);
        KsdsFile<DisclosureGroupRecord.DisGroupKey, DisclosureGroupRecord> groups = KsdsFile.load(
                in.resolve(CardDemoData.DISCLOSURE_GROUPS), DisclosureGroupRecord.CODEC,
                DisclosureGroupRecord::disGroupKey);
        InterestCalculator calculator = new InterestCalculator(accounts, xrefs, groups,
                parmDate == null ? DEFAULT_PARM_DATE : parmDate, clock.getIfUnique(Clock::systemDefaultZone));
        return new IntcalcItemProcessor(calculator, accounts, output(outputDir).resolve(CardDemoData.ACCOUNTS));
    }

    @Bean
    @StepScope
    public FlatFileItemWriter<TransactionRecord> intcalcTransactionWriter(
            @Value("#{jobParameters['outputDir']}") String outputDir) {
        return FixedWidthItemIO.writer("intcalcTransactionWriter",
                new FileSystemResource(output(outputDir).resolve(TRANSACT_FILE)), TransactionRecord.CODEC);
    }

    private static Path input(String inputDir) {
        return inputDir == null ? CardDemoData.asciiDir() : Path.of(inputDir);
    }

    private static Path output(String outputDir) {
        Path out = Path.of(outputDir == null ? "target/intcalc" : outputDir);
        try {
            return Files.createDirectories(out);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
