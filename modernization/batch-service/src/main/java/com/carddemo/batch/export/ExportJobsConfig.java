package com.carddemo.batch.export;

import com.carddemo.batch.JclJob;
import com.carddemo.batch.source.KsdsInput;
import com.carddemo.batch.source.SourceTables;
import com.carddemo.batch.support.BatchFiles;
import com.carddemo.common.batch.BatchJobParameters;
import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.batch.JobRunDirectory;
import com.carddemo.common.batch.ReturnCodes;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import com.carddemo.common.record.layout.ExportRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.nio.file.Path;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
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
 * CBEXPORT (STEP02; STEP01's IDCAMS define is the generation directory) and CBIMPORT (STEP01).
 * CBEXPORT reads CUSTFILE, ACCTFILE, XREFFILE, TRANSACT and CARDFILE ({@code --dd} files or the
 * tables) and writes EXPFILE; CBIMPORT reads EXPFILE ({@code --dd}, else the latest CBEXPORT
 * generation, as the shared {@code EXPORT.DATA} data set) and writes CUSTOUT, ACCTOUT, XREFOUT,
 * TRNXOUT, CARDOUT and ERROUT. Both write their DISPLAYs to SYSOUT. Neither writes a table.
 */
@Configuration
public class ExportJobsConfig {

    public static final String CBEXPORT = "CBEXPORT";
    public static final String CBIMPORT = "CBIMPORT";
    static final String EXPFILE = "EXPFILE";
    /** EXPORT-SEQUENCE-NUM: the EXPFILE KSDS key (offset 27, 4 bytes binary). */
    static final int KEY_OFFSET = ExportRecordLayout.EXPORT_SEQUENCE_NUM.offset();
    static final int KEY_LENGTH = ExportRecordLayout.EXPORT_SEQUENCE_NUM.length();

    @Bean
    public JclJob cbexport(JobRepository repository, PlatformTransactionManager tx, SourceTables tables, Clock clock) {
        Job job = new JobBuilder("branchExportJob", repository)
                .start(new StepBuilder("STEP02", repository).tasklet((contribution, chunk) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    CodePage cp = BatchJobParameters.codePage(params);
                    var customers = KsdsInput.read(params, "CUSTFILE", CustomerRecordLayout.LAYOUT, 0, 9,
                            "customer.customers", () -> tables.customers(cp));
                    var accounts = KsdsInput.read(params, "ACCTFILE", AccountRecordLayout.LAYOUT, 0, 11,
                            "account.accounts", () -> tables.accounts(cp));
                    var xrefs = KsdsInput.read(params, "XREFFILE", CardXrefRecordLayout.LAYOUT, 0, 16,
                            "card.card_xref", () -> tables.crossReferences(cp));
                    var transactions = KsdsInput.read(params, "TRANSACT", TranRecordLayout.LAYOUT, 0, 16,
                            "transaction.transactions", () -> tables.transactions(cp));
                    var cards = KsdsInput.read(params, "CARDFILE", CardRecordLayout.LAYOUT, 0, 16,
                            "card.cards", () -> tables.cards(cp));
                    BranchExport.Output output = BranchExport.run(new BranchExport.Inputs(customers.records(),
                            accounts.records(), xrefs.records(), transactions.records(), cards.records()),
                            LocalDateTime.now(clock), cp);
                    var context = contribution.getStepExecution().getExecutionContext();
                    for (var entry : List.of(List.of("CUSTFILE", customers), List.of("ACCTFILE", accounts),
                            List.of("XREFFILE", xrefs), List.of("TRANSACT", transactions), List.of("CARDFILE", cards))) {
                        context.putString((String) entry.get(0), ((KsdsInput.Dataset) entry.get(1)).source());
                    }
                    context.putInt(EXPFILE, DdFiles.write(BatchJobParameters.runDirectory(params), EXPFILE,
                            output.records(), BatchJobParameters.format(params)));
                    BatchFiles.writeSysout(params, output.sysout());
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, tx).build())
                .build();
        return new JclJob(CBEXPORT, job);
    }

    @Bean
    public JclJob cbimport(JobRepository repository, PlatformTransactionManager tx, Clock clock) {
        Job job = new JobBuilder("branchImportJob", repository)
                .start(new StepBuilder("STEP01", repository).tasklet((contribution, chunk) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    CodePage cp = BatchJobParameters.codePage(params);
                    JobRunDirectory generation = BatchJobParameters.runDirectory(params);
                    Path outputRoot = generation.path().getParent().getParent();
                    Path expfile = BatchJobParameters.dd(params, EXPFILE)
                            .or(() -> JobRunDirectory.latestGeneration(outputRoot, CBEXPORT, EXPFILE))
                            .orElseThrow(() -> new IllegalStateException(
                                    "EXPFILE not found: pass --dd=EXPFILE=<file> or run CBEXPORT first"));
                    List<CobolRecord> export = KsdsInput.keyOrder(EXPFILE,
                            DdFiles.read(expfile, ExportRecordLayout.LAYOUT, cp), KEY_OFFSET, KEY_LENGTH);
                    BranchImport.Output output = BranchImport.run(export, ZonedDateTime.now(clock), cp);
                    var format = BatchJobParameters.format(params);
                    var context = contribution.getStepExecution().getExecutionContext();
                    context.putString(EXPFILE, expfile.toString());
                    context.putInt(BranchImport.CUSTOUT, DdFiles.write(generation, BranchImport.CUSTOUT, output.customers(), format));
                    context.putInt(BranchImport.ACCTOUT, DdFiles.write(generation, BranchImport.ACCTOUT, output.accounts(), format));
                    context.putInt(BranchImport.XREFOUT, DdFiles.write(generation, BranchImport.XREFOUT, output.xrefs(), format));
                    context.putInt(BranchImport.TRNXOUT, DdFiles.write(generation, BranchImport.TRNXOUT, output.transactions(), format));
                    context.putInt(BranchImport.CARDOUT, DdFiles.write(generation, BranchImport.CARDOUT, output.cards(), format));
                    context.putInt(BranchImport.ERROUT, BatchFiles.writeLines(params, BranchImport.ERROUT, output.errors(),
                            BranchImport.ERROR_RECORD_LENGTH));
                    BatchFiles.writeSysout(params, output.sysout());
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, tx).build())
                .build();
        return new JclJob(CBIMPORT, job);
    }
}
