package com.carddemo.batch.reader;

import com.carddemo.batch.JclJob;
import com.carddemo.batch.source.KsdsInput;
import com.carddemo.batch.source.SourceTables;
import com.carddemo.batch.support.BatchFiles;
import com.carddemo.common.batch.BatchJobParameters;
import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.batch.ReturnCodes;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
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
 * The read-and-print utilities: READACCT (CBACT01C), READCARD (CBACT02C), READXREF (CBACT03C) and
 * READCUST (CBCUS01C), one step STEP05 each. The input KSDS is {@code --dd=<DD>=<file>} or the table.
 * The DISPLAY output goes to {@code SYSOUT} in the run's generation directory.
 */
@Configuration
public class ReaderJobsConfig {

    @Bean
    public JclJob readAcct(JobRepository repository, PlatformTransactionManager tx, SourceTables tables) {
        Job job = new JobBuilder("readAccountsJob", repository)
                .start(new StepBuilder("STEP05", repository).tasklet((contribution, chunk) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    CodePage codePage = BatchJobParameters.codePage(params);
                    KsdsInput.Dataset accounts = KsdsInput.read(params, "ACCTFILE", AccountRecordLayout.LAYOUT, 0, 11,
                            "account.accounts", () -> tables.accounts(codePage));
                    AccountFileDump.Output output = AccountFileDump.run(accounts.records(), codePage);
                    var generation = BatchJobParameters.runDirectory(params);
                    var format = BatchJobParameters.format(params);
                    var context = contribution.getStepExecution().getExecutionContext();
                    context.putString("ACCTFILE", accounts.source());
                    context.putInt("OUTFILE", DdFiles.write(generation, "OUTFILE", output.outRecords(), format));
                    context.putInt("ARRYFILE", DdFiles.write(generation, "ARRYFILE", output.arrayRecords(), format));
                    context.putInt("VBRCFILE", BatchFiles.writeVariable(params, "VBRCFILE", output.variableRecords()));
                    BatchFiles.writeSysout(params, output.sysout());
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, tx).build())
                .build();
        return new JclJob("READACCT", job);
    }

    @Bean
    public JclJob readCard(JobRepository repository, PlatformTransactionManager tx, SourceTables tables) {
        return dumpJob(repository, tx, "READCARD", "readCardsJob", "CBACT02C", 1, "CARDFILE", CardRecordLayout.LAYOUT, 16,
                "card.cards", tables::cards);
    }

    @Bean
    public JclJob readXref(JobRepository repository, PlatformTransactionManager tx, SourceTables tables) {
        return dumpJob(repository, tx, "READXREF", "readCrossReferencesJob", "CBACT03C", 2, "XREFFILE",
                CardXrefRecordLayout.LAYOUT, 16, "card.card_xref", tables::crossReferences);
    }

    @Bean
    public JclJob readCust(JobRepository repository, PlatformTransactionManager tx, SourceTables tables) {
        return dumpJob(repository, tx, "READCUST", "readCustomersJob", "CBCUS01C", 2, "CUSTFILE",
                CustomerRecordLayout.LAYOUT, 9, "customer.customers", tables::customers);
    }

    /**
     * CBACT02C / CBACT03C / CBCUS01C: {@code DISPLAY} each record of the KSDS in key order between the
     * START / END OF EXECUTION lines. The records are shown as stored (a group DISPLAY). CBACT03C and
     * CBCUS01C show each record twice: once in the read paragraph on status '00' and again in the main
     * loop (CBACT02C has the first DISPLAY commented out).
     */
    static List<String> dump(String program, int displaysPerRecord, List<CobolRecord> records) {
        List<String> lines = new ArrayList<>();
        lines.add("START OF EXECUTION OF PROGRAM " + program);
        for (CobolRecord record : records) {
            byte[] bytes = record.toBytes();
            String text = record.codePage().decodeText(bytes, 0, bytes.length);
            for (int i = 0; i < displaysPerRecord; i++) {
                lines.add(text);
            }
        }
        lines.add("END OF EXECUTION OF PROGRAM " + program);
        return lines;
    }

    private static JclJob dumpJob(JobRepository repository, PlatformTransactionManager tx, String member, String jobName,
                                  String program, int displaysPerRecord, String dd, RecordLayout layout, int keyLength, String table,
                                  Function<CodePage, List<CobolRecord>> rows) {
        Job job = new JobBuilder(jobName, repository)
                .start(new StepBuilder("STEP05", repository).tasklet((contribution, chunk) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    CodePage codePage = BatchJobParameters.codePage(params);
                    KsdsInput.Dataset input = KsdsInput.read(params, dd, layout, 0, keyLength, table,
                            () -> rows.apply(codePage));
                    BatchFiles.writeSysout(params, dump(program, displaysPerRecord, input.records()));
                    var context = contribution.getStepExecution().getExecutionContext();
                    context.putString(dd, input.source());
                    context.putInt("recordsDisplayed", input.records().size());
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, tx).build())
                .build();
        return new JclJob(member, job);
    }
}
