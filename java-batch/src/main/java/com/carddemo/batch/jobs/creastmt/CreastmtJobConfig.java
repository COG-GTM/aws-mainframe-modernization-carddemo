package com.carddemo.batch.jobs.creastmt;

import com.carddemo.batch.model.CardXrefRecord;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobScope;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.support.ListItemReader;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

import java.nio.file.Path;

/**
 * CREASTMT.JCL as a Spring Batch job.
 * <ul>
 *   <li>{@code trxflSortStep} = STEP010 SORT (TRANSACT -> TRXFL);</li>
 *   <li>{@code createStatementStep} = STEP040 CBSTM03A/CBSTM03B: one item per XREFFILE
 *       record, producing that account's STMTFILE and HTMLFILE records.</li>
 * </ul>
 * Job parameters (file paths): {@code transactFile}, {@code trxflFile}, {@code xrefFile},
 * {@code custFile}, {@code acctFile}, {@code stmtFile}, {@code htmlFile}.
 */
@Configuration
public class CreastmtJobConfig {

    @Bean
    public Job creastmtJob(JobRepository jobRepository, Step trxflSortStep, Step createStatementStep) {
        return new JobBuilder("creastmtJob", jobRepository)
                .start(trxflSortStep)
                .next(createStatementStep)
                .build();
    }

    @Bean
    @JobScope
    public Step trxflSortStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                              @Value("#{jobParameters['transactFile']}") String transactFile,
                              @Value("#{jobParameters['trxflFile']}") String trxflFile) {
        return new StepBuilder("trxflSortStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    TrxflSorter.sort(Path.of(transactFile), Path.of(trxflFile));
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    @Bean
    public Step createStatementStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                    ListItemReader<CardXrefRecord> creastmtXrefReader,
                                    ItemProcessor<CardXrefRecord, Statement> creastmtStatementProcessor,
                                    StatementFileWriter creastmtStatementWriter) {
        return new StepBuilder("createStatementStep", jobRepository)
                .<CardXrefRecord, Statement>chunk(10, transactionManager)
                .reader(creastmtXrefReader)
                .processor(creastmtStatementProcessor)
                .writer(creastmtStatementWriter)
                .build();
    }

    @Bean
    @StepScope
    public StatementFileRepository creastmtFiles(@Value("#{jobParameters['trxflFile']}") String trxflFile,
                                                 @Value("#{jobParameters['xrefFile']}") String xrefFile,
                                                 @Value("#{jobParameters['custFile']}") String custFile,
                                                 @Value("#{jobParameters['acctFile']}") String acctFile) {
        return new StatementFileRepository(Path.of(trxflFile), Path.of(xrefFile), Path.of(custFile),
                Path.of(acctFile));
    }

    /** 1000-XREFFILE-GET-NEXT: XREFFILE in key sequence. */
    @Bean
    @StepScope
    public ListItemReader<CardXrefRecord> creastmtXrefReader(StatementFileRepository creastmtFiles) {
        return new ListItemReader<>(creastmtFiles.xrefs());
    }

    /** 2000-CUSTFILE-GET, 3000-ACCTFILE-GET, 4000-TRNXFILE-GET and 5000-CREATE-STATEMENT. */
    @Bean
    @StepScope
    public ItemProcessor<CardXrefRecord, Statement> creastmtStatementProcessor(StatementFileRepository creastmtFiles) {
        CardTransactionTable table = new CardTransactionTable(creastmtFiles.transactions());
        return xref -> StatementFormatter.format(
                creastmtFiles.customer(xref.xrefCustId()),
                creastmtFiles.account(xref.xrefAcctId()),
                table.forCard(xref.xrefCardNum()));
    }

    @Bean
    @StepScope
    public StatementFileWriter creastmtStatementWriter(@Value("#{jobParameters['stmtFile']}") String stmtFile,
                                                       @Value("#{jobParameters['htmlFile']}") String htmlFile) {
        return new StatementFileWriter(Path.of(stmtFile), Path.of(htmlFile));
    }
}
