package com.carddemo.batch.scheduler;

import com.carddemo.batch.harness.BatchCommandLine;
import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.JclCond;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobOutcome;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.load.UnloadJobConfiguration;
import com.carddemo.batch.scheduler.NightlyCycle.Member;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import javax.sql.DataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.job.builder.SimpleJobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.AbstractStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * {@code nightly-cycle}: the Spring Batch flow job that replaces the Control-M / CA-7 schedule (decision
 * {@code d-scheduler}, docs/modernization/06-scheduling.md). One step per {@link NightlyCycle#MEMBERS member}, in the
 * baseline run order; each step launches the member's harness job or {@code JobStream} through
 * {@link BatchJobLauncher}, so every child job lands in {@code batch_run} as if it had been run from the CLI, and the
 * step itself carries the member's RC (MAXCC of its steps) and the summed read/write/skip/filter counts of the child
 * jobs. A member runs only when each predecessor ran and ended with RC &lt;= 4 without an abend
 * ({@code COND=(4,LT,<predecessor>)}, the scheduler's "ended OK" condition); otherwise its step ends
 * {@code BYPASSED}. The job's RC is the highest member RC (JCL MAXCC). Every member step completes, so independent
 * branches still run after a failure, and the job cannot be restarted: re-run individual streams instead.
 *
 * <p>Parameters: everything given to the cycle reaches every member, except {@code <MEMBER>.<name>=} which only
 * reaches that member, as {@code <name>} (e.g. {@code --POSTTRAN.STEP15.SYSOUT=...}, {@code --READACCT.SYSOUT=...}).
 * {@code --AFTER-IMAGES=<dir>} unloads the KSDS datasets a member updated to {@code <dir>/<MEMBER>/<DATASET>.ksds}
 * after it ran (the table, or the file named by {@code --AFTER-IMAGES.<DATASET>=<path>}), like the baseline runner's
 * after-image unloads; a requested after-image that cannot be written raises the member RC to at least 8.
 *
 * <p>Only one cycle runs at a time across every JVM sharing the database (cron, CLI): {@link CycleLock} holds a
 * PostgreSQL advisory lock from job start to job end; a cycle that cannot take it fails before any member runs.
 */
@Configuration(proxyBeanMethods = false)
public class NightlyCycleJobConfiguration {

    public static final String AFTER_IMAGES = "AFTER-IMAGES";
    /** Identifying parameter of every child job: the cycle member that launched it. */
    public static final String MEMBER = "cycle.member";
    /** Non-identifying parameter of every child job: the job execution id of the cycle. */
    public static final String CYCLE_EXECUTION = "cycle.execution-id";
    public static final String BYPASSED = "BYPASSED";
    public static final String ABEND = "ABEND";

    static final String RC_KEY = "nightly-cycle.rc.";
    static final String ABEND_KEY = "nightly-cycle.abend.";

    private static final Logger log = LoggerFactory.getLogger(NightlyCycleJobConfiguration.class);

    @Bean
    Job nightlyCycleJob(JobRepository jobRepository, ObjectProvider<BatchJobLauncher> launcher,
                        ObjectProvider<JobStream> streams, DataSource dataSource) {
        SimpleJobBuilder job = null;
        for (Member member : NightlyCycle.MEMBERS) {
            MemberStep step = new MemberStep(member, launcher, streams);
            step.setJobRepository(jobRepository);
            job = job == null ? new JobBuilder(NightlyCycle.NAME, jobRepository).preventRestart().start(step)
                    : job.next(step);
        }
        return job.listener(new CycleLock(dataSource)).build();
    }

    /** The parameters a member's job or stream receives (see the class comment). */
    static JobParameters memberParameters(JobParameters cycle, Member member, Long cycleExecutionId) {
        JobParametersBuilder builder = new JobParametersBuilder();
        cycle.getParameters().forEach((name, value) -> {
            if (qualifier(name).isEmpty() && !isCycleOnly(name)) {
                builder.addJobParameter(name, value);
            }
        });
        cycle.getParameters().forEach((name, value) -> {
            Optional<String> qualifier = qualifier(name);
            if (qualifier.isPresent() && qualifier.get().equalsIgnoreCase(member.name())) {
                builder.addJobParameter(name.substring(qualifier.get().length() + 1), value);
            }
        });
        builder.addString(MEMBER, member.name());
        if (cycleExecutionId != null) {
            builder.addLong(CYCLE_EXECUTION, cycleExecutionId, false);
        }
        return builder.toJobParameters();
    }

    private static Optional<String> qualifier(String name) {
        int dot = name.indexOf('.');
        return dot > 0 ? NightlyCycle.member(name.substring(0, dot)).map(m -> name.substring(0, dot))
                : Optional.empty();
    }

    private static boolean isCycleOnly(String name) {
        return name.equalsIgnoreCase(AFTER_IMAGES) || name.toUpperCase().startsWith(AFTER_IMAGES + ".");
    }

    static final class MemberStep extends AbstractStep {

        private final Member member;
        private final ObjectProvider<BatchJobLauncher> launcher;
        private final ObjectProvider<JobStream> streams;

        MemberStep(Member member, ObjectProvider<BatchJobLauncher> launcher, ObjectProvider<JobStream> streams) {
            super(member.name());
            this.member = member;
            this.launcher = launcher;
            this.streams = streams;
        }

        @Override
        protected void doExecute(StepExecution stepExecution) {
            JobExecution cycle = stepExecution.getJobExecution();
            ExecutionContext context = cycle.getExecutionContext();
            Optional<String> reason = bypassReason(context);
            if (reason.isPresent()) {
                log.info("{} {} {}: bypassed ({})", NightlyCycle.NAME, member.name(), member.implementation(),
                        reason.get());
                stepExecution.setExitStatus(new ExitStatus(BYPASSED, reason.get()));
                return;
            }
            JobParameters parameters = memberParameters(cycle.getJobParameters(), member, cycle.getId());
            BatchJobLauncher jobs = launcher.getObject();
            Optional<JobStream> stream = streams.orderedStream()
                    .filter(s -> s.name().equalsIgnoreCase(member.implementation())).findFirst();
            List<JobOutcome> outcomes = new ArrayList<>();
            ReturnCode rc;
            boolean abended;
            if (stream.isPresent()) {
                JobChain.Result result = stream.get().chain(jobs, parameters).run();
                for (JobChain.StepResult step : result.steps()) {
                    if (step.bypassed()) {
                        log.info("{} {} {} {}: bypassed (COND={})", NightlyCycle.NAME, member.name(),
                                step.step().stepName(), step.step().jobName(), step.step().cond());
                    } else {
                        outcomes.add(step.outcome());
                        log.info("{} {} {} {}: {}{}", NightlyCycle.NAME, member.name(), step.step().stepName(),
                                step.step().jobName(), step.outcome().returnCode().label(),
                                step.outcome().abended() ? " ABEND" : "");
                    }
                }
                rc = result.maxReturnCode();
                abended = result.abended();
            } else {
                JobOutcome outcome = jobs.run(member.implementation(), parameters);
                outcomes.add(outcome);
                rc = outcome.returnCode();
                abended = outcome.abended();
            }
            count(stepExecution, outcomes);
            if (!afterImages(cycle, jobs) && rc.code() < ReturnCode.ERROR.code()) {
                rc = ReturnCode.ERROR;
            }
            ReturnCode.set(stepExecution, rc);
            context.putInt(RC_KEY + member.name(), rc.code());
            context.put(ABEND_KEY + member.name(), abended);
            stepExecution.setExitStatus(new ExitStatus(abended ? ABEND : ExitStatus.COMPLETED.getExitCode(),
                    rc.label()));
            log.info("{} {} {}: {}{}", NightlyCycle.NAME, member.name(), member.implementation(), rc.label(),
                    abended ? " ABEND" : "");
        }

        private Optional<String> bypassReason(ExecutionContext context) {
            Map<String, ReturnCode> previous = new LinkedHashMap<>();
            boolean abended = false;
            for (String predecessor : member.predecessors()) {
                if (!context.containsKey(RC_KEY + predecessor)) {
                    return Optional.of(predecessor + " did not run");
                }
                previous.put(predecessor, ReturnCode.of(context.getInt(RC_KEY + predecessor)));
                abended |= Boolean.TRUE.equals(context.get(ABEND_KEY + predecessor));
            }
            if (member.predecessors().isEmpty()) {
                return Optional.empty();
            }
            return JclCond.parse(member.cond()).bypass(previous, abended)
                    ? Optional.of("COND=" + member.cond() + ", predecessor RCs " + previous
                            + (abended ? " (abend)" : ""))
                    : Optional.empty();
        }

        private static void count(StepExecution target, List<JobOutcome> outcomes) {
            long read = 0;
            long write = 0;
            long skip = 0;
            long filter = 0;
            for (JobOutcome outcome : outcomes) {
                if (outcome.execution() == null) {
                    continue;
                }
                for (StepExecution step : outcome.execution().getStepExecutions()) {
                    read += step.getReadCount();
                    write += step.getWriteCount();
                    skip += step.getSkipCount();
                    filter += step.getFilterCount();
                }
            }
            target.setReadCount(read);
            target.setWriteCount(write);
            target.setProcessSkipCount(skip);
            target.setFilterCount(filter);
        }

        /** Writes the requested after-images; false when one could not be written. */
        private boolean afterImages(JobExecution cycle, BatchJobLauncher jobs) {
            JobParameters all = cycle.getJobParameters();
            String dir = all.getString(AFTER_IMAGES);
            if (dir == null || member.afterImages().isEmpty()) {
                return true;
            }
            boolean written = true;
            Path target = Path.of(dir).resolve(member.name());
            for (String dataset : member.afterImages()) {
                Path out = target.resolve(dataset + ".ksds");
                String source = all.getString(AFTER_IMAGES + "." + dataset);
                try {
                    Files.createDirectories(target);
                    if (source != null && !DdParameters.TABLE.equalsIgnoreCase(source)) {
                        Files.copy(Path.of(source), out, StandardCopyOption.REPLACE_EXISTING);
                        continue;
                    }
                } catch (IOException e) {
                    log.error("{} {}: after-image of {} not written: {}", NightlyCycle.NAME, member.name(), dataset,
                            e.toString());
                    written = false;
                    continue;
                }
                JobParametersBuilder unload = new JobParametersBuilder()
                        .addString(UnloadJobConfiguration.DATASET, dataset)
                        .addString(UnloadJobConfiguration.OUTFILE, out.toString())
                        .addString(MEMBER, member.name())
                        .addLong(CYCLE_EXECUTION, cycle.getId(), false);
                copy(all, unload, BatchCommandLine.RUN_DATE, BatchCommandLine.RUN_ID, DdParameters.ENCODING);
                JobOutcome outcome = jobs.run(UnloadJobConfiguration.UNLOAD, unload.toJobParameters());
                if (outcome.returnCode() != ReturnCode.OK) {
                    log.error("{} {}: after-image unload of {} ended {}", NightlyCycle.NAME, member.name(), dataset,
                            outcome.returnCode().label());
                    written = false;
                }
            }
            return written;
        }

        private static void copy(JobParameters from, JobParametersBuilder to, String... names) {
            for (String name : names) {
                if (from.getParameters().containsKey(name)) {
                    to.addJobParameter(name, from.getParameters().get(name));
                }
            }
        }
    }

    /**
     * Cycle-wide mutual exclusion: a session-level PostgreSQL advisory lock taken on a dedicated connection in
     * {@code beforeJob} and released in {@code afterJob}. If another cycle holds it the job fails before any member
     * runs. Other databases (none in use) are not locked.
     */
    static final class CycleLock implements JobExecutionListener {

        /** Advisory lock key ("NCYC"). */
        static final long KEY = 0x4E435943L;

        private final DataSource dataSource;
        private final Map<Long, Connection> held = new ConcurrentHashMap<>();

        CycleLock(DataSource dataSource) {
            this.dataSource = dataSource;
        }

        @Override
        public void beforeJob(JobExecution execution) {
            try {
                Connection connection = dataSource.getConnection();
                if (!"PostgreSQL".equals(connection.getMetaData().getDatabaseProductName())) {
                    connection.close();
                    return;
                }
                if (!advisory(connection, "select pg_try_advisory_lock(?)")) {
                    connection.close();
                    throw new IllegalStateException(NightlyCycle.NAME + " not started: another "
                            + NightlyCycle.NAME + " execution holds the cycle lock");
                }
                held.put(execution.getId(), connection);
            } catch (SQLException e) {
                throw new IllegalStateException(NightlyCycle.NAME + " cycle lock not acquired", e);
            }
        }

        @Override
        public void afterJob(JobExecution execution) {
            Connection connection = held.remove(execution.getId());
            if (connection == null) {
                return;
            }
            try (connection) {
                advisory(connection, "select pg_advisory_unlock(?)");
            } catch (SQLException e) {
                log.error("{}: cycle lock release failed (released when the connection closes): {}",
                        NightlyCycle.NAME, e.toString());
            }
        }

        private static boolean advisory(Connection connection, String sql) throws SQLException {
            try (PreparedStatement statement = connection.prepareStatement(sql)) {
                statement.setLong(1, KEY);
                try (ResultSet result = statement.executeQuery()) {
                    return result.next() && result.getBoolean(1);
                }
            }
        }
    }
}
