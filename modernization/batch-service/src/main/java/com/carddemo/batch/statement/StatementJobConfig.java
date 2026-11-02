package com.carddemo.batch.statement;

import com.carddemo.batch.JclJob;
import com.carddemo.batch.source.KsdsInput;
import com.carddemo.batch.source.SourceTables;
import com.carddemo.batch.support.BatchFiles;
import com.carddemo.common.batch.BatchJobParameters;
import com.carddemo.common.batch.CondTasklet;
import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.batch.RecordSort;
import com.carddemo.common.batch.ReturnCodes;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import com.carddemo.common.record.layout.TrnxRecordLayout;
import java.util.List;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * CREASTMT. STEP010: {@code SORT FIELDS=(263,16,CH,A,1,16,CH,A)} {@code OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)}
 * of TRANSACT ({@code --dd=TRANSACT} or {@code transaction.transactions}) into TRXFL.SEQ. DELDEF01, STEP020
 * (REPRO into the TRXFL KSDS) and STEP030 (delete last run's reports) are data-definition only: the sorted
 * file is read in key order and every run has its own generation. STEP040 ({@code COND=(0,NE)}): CBSTM03A
 * with CBSTM03B, writing STMTFILE (80) and HTMLFILE (100).
 */
@Configuration
public class StatementJobConfig {

    public static final String CREASTMT = "CREASTMT";
    static final String TRXFL = "TRXFL.SEQ";

    @Bean
    public JclJob creastmt(JobRepository repository, PlatformTransactionManager tx, SourceTables tables) {
        var sort = new StepBuilder("STEP010", repository).tasklet((contribution, chunk) -> {
            JobParameters params = contribution.getStepExecution().getJobParameters();
            CodePage cp = BatchJobParameters.codePage(params);
            KsdsInput.Dataset transact = KsdsInput.read(params, "TRANSACT", TranRecordLayout.LAYOUT, 0, 16,
                    "transaction.transactions", () -> tables.transactions(cp));
            List<CobolRecord> sorted = RecordSort.fields(RecordSort.Key.ascending(263, 16), RecordSort.Key.ascending(1, 16))
                    .outrec(new RecordSort.OutrecField(1, 263, 16), new RecordSort.OutrecField(17, 1, 262),
                            new RecordSort.OutrecField(279, 279, 50))
                    .sort(List.of(transact.records()), TrnxRecordLayout.LAYOUT);
            var context = contribution.getStepExecution().getExecutionContext();
            context.putString("TRANSACT", transact.source());
            context.putInt(TRXFL, DdFiles.write(BatchJobParameters.runDirectory(params), TRXFL, sorted,
                    BatchJobParameters.format(params)));
            ReturnCodes.set(contribution, 0);
            return RepeatStatus.FINISHED;
        }, tx).build();
        var statements = new StepBuilder("STEP040", repository).tasklet(CondTasklet.of("(0,NE)", (contribution, chunk) -> {
            JobParameters params = contribution.getStepExecution().getJobParameters();
            CodePage cp = BatchJobParameters.codePage(params);
            List<CobolRecord> trnx = KsdsInput.keyOrder("TRNXFILE",
                    DdFiles.read(BatchFiles.stepOutput(params, TRXFL), TrnxRecordLayout.LAYOUT, cp), 0, 32);
            var xref = KsdsInput.read(params, "XREFFILE", CardXrefRecordLayout.LAYOUT, 0, 16, "card.card_xref",
                    () -> tables.crossReferences(cp));
            var cust = KsdsInput.read(params, "CUSTFILE", CustomerRecordLayout.LAYOUT, 0, 9, "customer.customers",
                    () -> tables.customers(cp));
            var acct = KsdsInput.read(params, "ACCTFILE", AccountRecordLayout.LAYOUT, 0, 11, "account.accounts",
                    () -> tables.accounts(cp));
            StatementWriter.Output output = StatementWriter.run(
                    new StatementFileService(trnx, xref.records(), cust.records(), acct.records()));
            var context = contribution.getStepExecution().getExecutionContext();
            context.putString("XREFFILE", xref.source());
            context.putString("CUSTFILE", cust.source());
            context.putString("ACCTFILE", acct.source());
            context.putInt("statements", output.statements());
            context.putInt("statementTransactions", output.transactions());
            context.putInt("STMTFILE", BatchFiles.writeLines(params, "STMTFILE", output.statementLines(), 80));
            context.putInt("HTMLFILE", BatchFiles.writeLines(params, "HTMLFILE", output.htmlLines(), 100));
            ReturnCodes.set(contribution, 0);
            return RepeatStatus.FINISHED;
        }), tx).build();
        Job job = new JobBuilder("createStatementsJob", repository).start(sort).next(statements).build();
        return new JclJob(CREASTMT, job);
    }
}
