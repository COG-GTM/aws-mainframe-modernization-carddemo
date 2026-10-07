package com.carddemo.batch.print;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.batch.BatchOutputProperties;
import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.FixedFileSink;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.harness.VariableFileSink;
import com.carddemo.card.Card;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardRepository;
import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.customer.Customer;
import com.carddemo.customer.CustomerRecord;
import com.carddemo.customer.CustomerRepository;
import java.nio.file.Path;
import java.util.function.Function;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * The print jobs {@code READACCT} ({@code CBACT01C}), {@code READCARD} ({@code CBACT02C}), {@code READXREF}
 * ({@code CBACT03C}) and {@code READCUST} ({@code CBCUS01C}): one step {@code STEP05} each, as in app/jcl. The input
 * KSDS DD ({@code ACCTFILE}, {@code CARDFILE}, {@code XREFFILE}, {@code CUSTFILE}) defaults to its PostgreSQL table;
 * {@code --<DD>=<path>} reads an unload file instead. READACCT's {@code PREDEL} (IEFBR14 delete of the three
 * outputs) is the truncate-on-open of the output files.
 */
@Configuration(proxyBeanMethods = false)
public class PrintJobsConfiguration {

    public static final String READACCT = "readacct";
    public static final String READCARD = "readcard";
    public static final String READXREF = "readxref";
    public static final String READCUST = "readcust";
    public static final String STEP = "STEP05";
    public static final String SYSOUT_CONTEXT_KEY = "SYSOUT";

    static final int PAGE_SIZE = 500;

    @FunctionalInterface
    interface ProgramStep {
        ProgramCounts run(JobParameters parameters, Sysout sysout, RecordEncoding encoding);
    }

    @Bean
    Job readacctJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    AccountRepository accounts, BatchOutputProperties output) {
        return job(READACCT, jobRepository, transactionManager, output, (parameters, sysout, encoding) -> {
            KsdsInput acctFile = input(parameters, Cbact01c.ACCTFILE, encoding, AccountRecord.MAPPER, -1L,
                    accounts::findByAcctIdGreaterThanOrderByAcctIdAsc, Account::getAcctId, Account::toRecord);
            Path dir = output.outputDir();
            return new Cbact01c(acctFile,
                    new FixedFileSink(Cbact01c.OUTFILE, DdParameters.output(parameters, Cbact01c.OUTFILE, dir,
                            "AWS.M2.CARDDEMO.ACCTDATA.PSCOMP")),
                    new FixedFileSink(Cbact01c.ARRYFILE, DdParameters.output(parameters, Cbact01c.ARRYFILE, dir,
                            "AWS.M2.CARDDEMO.ACCTDATA.ARRYPS")),
                    new VariableFileSink(Cbact01c.VBRCFILE, DdParameters.output(parameters, Cbact01c.VBRCFILE, dir,
                            "AWS.M2.CARDDEMO.ACCTDATA.VBPS"), DdParameters.recordPrefix(parameters),
                            Cbact01c.VBRC_MIN, Cbact01c.VBRC_MAX),
                    sysout, encoding).run();
        });
    }

    @Bean
    Job readcardJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    CardRepository cards, BatchOutputProperties output) {
        return job(READCARD, jobRepository, transactionManager, output, (parameters, sysout, encoding) ->
                new RecordPrintProgram(RecordPrintProgram.Program.CBACT02C,
                        input(parameters, "CARDFILE", encoding, CardRecord.MAPPER, "",
                                cards::findByCardNumGreaterThanOrderByCardNumAsc, Card::getCardNum, Card::toRecord),
                        sysout).run());
    }

    @Bean
    Job readxrefJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    CardXrefRepository xrefs, BatchOutputProperties output) {
        return job(READXREF, jobRepository, transactionManager, output, (parameters, sysout, encoding) ->
                new RecordPrintProgram(RecordPrintProgram.Program.CBACT03C,
                        input(parameters, "XREFFILE", encoding, CardXrefRecord.MAPPER, "",
                                xrefs::findByCardNumGreaterThanOrderByCardNumAsc, CardXref::getCardNum,
                                CardXref::toRecord),
                        sysout).run());
    }

    @Bean
    Job readcustJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    CustomerRepository customers, BatchOutputProperties output) {
        return job(READCUST, jobRepository, transactionManager, output, (parameters, sysout, encoding) ->
                new RecordPrintProgram(RecordPrintProgram.Program.CBCUS01C,
                        input(parameters, "CUSTFILE", encoding, CustomerRecord.MAPPER, -1,
                                customers::findByCustIdGreaterThanOrderByCustIdAsc, Customer::getCustId,
                                Customer::toRecord),
                        sysout).run());
    }

    private static <E, K, D extends Record> KsdsInput input(JobParameters parameters, String ddname, RecordEncoding encoding,
                                             CopybookRecordMapper<D> mapper, K lowValues,
                                             java.util.function.BiFunction<K, org.springframework.data.domain.Limit,
                                                     java.util.List<E>> after,
                                             Function<E, K> key, Function<E, D> toData) {
        if (!DdParameters.isTable(parameters, ddname)) {
            return KsdsInput.file(ddname, DdParameters.path(parameters, ddname), mapper.layout(), encoding);
        }
        return KsdsInput.table(ddname, lowValues, after, key, e -> mapper.toRecord(toData.apply(e), encoding),
                PAGE_SIZE);
    }

    private static Job job(String name, JobRepository jobRepository, PlatformTransactionManager transactionManager,
                           BatchOutputProperties output, ProgramStep program) {
        return new JobBuilder(name, jobRepository)
                .start(new StepBuilder(STEP, jobRepository).tasklet((contribution, chunk) -> {
                    StepExecution step = chunk.getStepContext().getStepExecution();
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    Path sysoutPath = DdParameters.sysout(parameters, output.outputDir(), name, step.getJobExecutionId());
                    step.getExecutionContext().putString(SYSOUT_CONTEXT_KEY,
                            sysoutPath.toAbsolutePath().normalize().toString());
                    try (Sysout sysout = Sysout.open(sysoutPath)) {
                        ProgramCounts counts = program.run(parameters, sysout, encoding);
                        for (long i = 0; i < counts.read(); i++) {
                            contribution.incrementReadCount();
                        }
                        contribution.incrementWriteCount(counts.written());
                    }
                    return RepeatStatus.FINISHED;
                }, transactionManager).build())
                .build();
    }
}
