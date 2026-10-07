package com.carddemo.batch.intcalc;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.batch.BaselineRunProperties;
import com.carddemo.batch.BatchOutputFile;
import com.carddemo.batch.BatchOutputProperties;
import com.carddemo.batch.DatedOutputFiles;
import com.carddemo.batch.harness.BatchCommandLine;
import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.BufferedSink;
import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.FixedFileSink;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.RecordSink;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.transaction.DisclosureGroup;
import com.carddemo.transaction.DisclosureGroupId;
import com.carddemo.transaction.DisclosureGroupRecord;
import com.carddemo.transaction.DisclosureGroupRepository;
import com.carddemo.transaction.TranCatBalance;
import com.carddemo.transaction.TranCatBalanceId;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TranCatBalanceRepository;
import java.nio.file.Path;
import java.time.Clock;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
 * INTCALC ({@code app/jcl/INTCALC.jcl}) as a harness job stream (ADR-0015): {@code STEP15} runs {@code cbact04c}
 * (CBACT04C) with the JCL {@code PARM} as job parameter {@code PARM} (default: {@code carddemo.baseline.intcalc-parm-date},
 * pinned to {@code 2022071800} by the {@code golden} profile, else the run date as {@code yyyyMMdd00}). Every KSDS DD
 * is a table unless {@code --<DD>=<path>} names an unload file (ACCTFILE is updated in place, FILLER kept). TRANSACT
 * defaults to the dated generation {@code AWS.M2.CARDDEMO.SYSTRAN(+1)} (GDG base {@code SYSTRAN}, ADR-0012),
 * catalogued only when the step ends without an abend ({@code DISP=(NEW,CATLG,DELETE)}). In table mode the step is
 * one database transaction: an abend rolls back every account rewrite, so nothing is applied twice by a new run.
 * Restarts are refused as for CBTRN02C (a file-mode ACCTFILE keeps the rewrites made before an abend).
 */
@Configuration(proxyBeanMethods = false)
public class IntcalcJobConfiguration {

    private static final Logger log = LoggerFactory.getLogger(IntcalcJobConfiguration.class);

    public static final String INTCALC = "intcalc";
    public static final String CBACT04C_JOB = "cbact04c";
    public static final String STEP15 = "STEP15";
    public static final String PARM = "PARM";
    public static final String SYSTRAN_GDG = "SYSTRAN";
    public static final String SYSOUT_CONTEXT_KEY = "SYSOUT";
    public static final String SYSTRAN_CONTEXT_KEY = "SYSTRAN";

    static final int PAGE_SIZE = 500;
    private static final DateTimeFormatter PARM_FROM_RUN_DATE = DateTimeFormatter.ofPattern("yyyyMMdd'00'");

    @Bean
    JobStream intcalcStream() {
        return new JobStream() {
            @Override
            public String name() {
                return INTCALC;
            }

            @Override
            public JobChain chain(BatchJobLauncher launcher, JobParameters parameters) {
                return new JobChain(launcher).step(STEP15, CBACT04C_JOB, JobStream.forStep(parameters, STEP15), null);
            }
        };
    }

    @Bean
    Job cbact04cJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    BatchOutputProperties output, DatedOutputFiles datedOutputFiles, Clock clock,
                    BaselineRunProperties baseline, TranCatBalanceRepository balances, CardXrefRepository xrefs,
                    DisclosureGroupRepository groups, AccountRepository accounts) {
        return new JobBuilder(CBACT04C_JOB, jobRepository).preventRestart()
                .start(new StepBuilder(STEP15, jobRepository).tasklet((contribution, chunk) -> {
                    StepExecution step = chunk.getStepContext().getStepExecution();
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    Path sysoutPath = DdParameters.sysout(parameters, output.outputDir(), CBACT04C_JOB,
                            step.getJobExecutionId());
                    step.getExecutionContext().putString(SYSOUT_CONTEXT_KEY,
                            sysoutPath.toAbsolutePath().normalize().toString());
                    boolean generation = DdParameters.isTable(parameters, Cbact04c.TRANSACT);
                    BufferedSink buffered = new BufferedSink(Cbact04c.TRANSACT);
                    RecordSink systran = generation ? buffered
                            : new FixedFileSink(Cbact04c.TRANSACT, DdParameters.path(parameters, Cbact04c.TRANSACT));
                    String parm = parm(parameters, baseline);
                    Cbact04c.Result result;
                    try (Sysout sysout = Sysout.open(sysoutPath)) {
                        result = new Cbact04c(tcatbalf(parameters, encoding, balances),
                                xref(parameters, encoding, xrefs), discgrp(parameters, encoding, groups),
                                account(parameters, encoding, accounts), systran, parm, encoding, sysout,
                                clock).run();
                    }
                    String systranPath;
                    if (generation) {
                        BatchOutputFile file = datedOutputFiles.write(SYSTRAN_GDG,
                                parameters.getLocalDate(BatchCommandLine.RUN_DATE), step.getJobExecutionId(),
                                buffered.records());
                        systranPath = file.getFilePath();
                    } else {
                        systranPath = DdParameters.path(parameters, Cbact04c.TRANSACT).toAbsolutePath().toString();
                    }
                    step.getExecutionContext().putString(SYSTRAN_CONTEXT_KEY, systranPath);
                    log.info("{}: PARM={} {} TCATBALF records, {} interest transactions, {} accounts updated"
                            + " -> SYSTRAN {}", CBACT04C_JOB, parm, result.read(), result.written(),
                            result.accountsUpdated(), systranPath);
                    for (long i = 0; i < result.read(); i++) {
                        contribution.incrementReadCount();
                    }
                    contribution.incrementWriteCount(result.written());
                    ReturnCode.set(step, result.returnCode());
                    return RepeatStatus.FINISHED;
                }, transactionManager).build())
                .build();
    }

    /** {@code PARM=}: the job parameter, else the pinned baseline value, else the run date as {@code yyyyMMdd00}. */
    static String parm(JobParameters parameters, BaselineRunProperties baseline) {
        String parm = parameters.getString(PARM);
        if (parm != null && !parm.isBlank()) {
            return parm;
        }
        if (baseline != null && baseline.intcalcParmDate() != null && !baseline.intcalcParmDate().isBlank()) {
            return baseline.intcalcParmDate();
        }
        LocalDate runDate = parameters.getLocalDate(BatchCommandLine.RUN_DATE);
        if (runDate == null) {
            throw new IllegalArgumentException(CBACT04C_JOB + " needs --PARM=<yyyyMMddnn> or --run-date");
        }
        return PARM_FROM_RUN_DATE.format(runDate);
    }

    static String tcatKey(TranCatBalanceId key) {
        return String.format(Locale.ROOT, "%011d%-2s%04d", key.acctId(), key.tranTypeCd(), key.tranCatCd());
    }

    static String discgrpKey(DisclosureGroupId key) {
        return String.format(Locale.ROOT, "%-10s%-2s%04d", key.acctGroupId(), key.tranTypeCd(), key.tranCatCd());
    }

    private static KsdsInput tcatbalf(JobParameters parameters, RecordEncoding encoding,
                                      TranCatBalanceRepository balances) {
        String dd = Cbact04c.TCATBALF;
        if (!DdParameters.isTable(parameters, dd)) {
            return KsdsInput.file(dd, DdParameters.path(parameters, dd), TranCatBalanceRecord.MAPPER.layout(),
                    encoding);
        }
        return KsdsInput.table(dd, new TranCatBalanceId(-1, "", -1), balances::findAfter, TranCatBalance::getId,
                b -> TranCatBalanceRecord.MAPPER.toRecord(b.toRecord(), encoding), PAGE_SIZE);
    }

    private static KeyedDataset<Long, CardXrefRecord> xref(JobParameters parameters, RecordEncoding encoding,
                                                           CardXrefRepository xrefs) {
        String dd = Cbact04c.XREFFILE;
        if (!DdParameters.isTable(parameters, dd)) {
            return new XrefByAccount(dd, DdParameters.path(parameters, dd), encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.INPUT,
                k -> xrefs.findFirstByAcctIdOrderByCardNumAsc(k).map(CardXref::toRecord),
                k -> xrefs.findFirstByAcctIdOrderByCardNumAsc(k).isPresent(), r -> {
                    throw new UnsupportedOperationException(dd + " is opened INPUT");
                }, r -> false, () -> {
                }, CardXrefRecord::acctId);
    }

    private static KeyedDataset<DisclosureGroupId, DisclosureGroupRecord> discgrp(
            JobParameters parameters, RecordEncoding encoding, DisclosureGroupRepository groups) {
        String dd = Cbact04c.DISCGRP;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.INPUT,
                    DisclosureGroupRecord.MAPPER,
                    r -> new DisclosureGroupId(r.acctGroupId(), r.tranTypeCd(), r.tranCatCd()),
                    IntcalcJobConfiguration::discgrpKey, encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.INPUT,
                k -> groups.findById(k).map(DisclosureGroup::toRecord), groups::existsById, r -> {
                    throw new UnsupportedOperationException(dd + " is opened INPUT");
                }, r -> false, () -> {
                }, r -> new DisclosureGroupId(r.acctGroupId(), r.tranTypeCd(), r.tranCatCd()));
    }

    private static KeyedDataset<Long, AccountRecord> account(JobParameters parameters, RecordEncoding encoding,
                                                             AccountRepository accounts) {
        String dd = Cbact04c.ACCTFILE;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.I_O,
                    AccountRecord.MAPPER, AccountRecord::acctId, k -> String.format(Locale.ROOT, "%011d", k),
                    encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.I_O, k -> accounts.findById(k).map(Account::toRecord),
                accounts::existsById, r -> accounts.save(Account.from(r)),
                r -> accounts.findById(r.acctId()).map(a -> {
                    a.update(r);
                    return true;
                }).orElse(false), () -> {
                }, AccountRecord::acctId);
    }
}
