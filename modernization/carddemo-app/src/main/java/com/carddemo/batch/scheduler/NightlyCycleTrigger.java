package com.carddemo.batch.scheduler;

import com.carddemo.batch.harness.BatchCommandLine;
import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.JobOutcome;
import java.time.Clock;
import java.time.LocalDate;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.scheduling.annotation.Scheduled;

/**
 * The in-app replacement of the Control-M / CA-7 daily order: launches {@code nightly-cycle} on the cron in
 * {@code carddemo.batch.scheduler.nightly-cycle.cron} with the business date of the injected clock as
 * {@code run-date}. Skips a fire while another {@code nightly-cycle} execution is still running (e.g. a manual CLI
 * launch against the same database). Registered only by {@link NightlyCycleScheduling}.
 */
public class NightlyCycleTrigger {

    private static final Logger log = LoggerFactory.getLogger(NightlyCycleTrigger.class);

    private final BatchJobLauncher launcher;
    private final JobExplorer explorer;
    private final Clock clock;

    public NightlyCycleTrigger(BatchJobLauncher launcher, JobExplorer explorer, Clock clock) {
        this.launcher = launcher;
        this.explorer = explorer;
        this.clock = clock;
    }

    @Scheduled(cron = "${carddemo.batch.scheduler.nightly-cycle.cron}", zone = "${carddemo.clock.zone:UTC}")
    public void fire() {
        launch(LocalDate.now(clock));
    }

    /** Launches the cycle for {@code runDate}; empty when a cycle is already running. */
    public Optional<JobOutcome> launch(LocalDate runDate) {
        if (!explorer.findRunningJobExecutions(NightlyCycle.NAME).isEmpty()) {
            log.warn("{} for {} not started: a {} execution is still running", NightlyCycle.NAME, runDate,
                    NightlyCycle.NAME);
            return Optional.empty();
        }
        log.info("cron: launching {} for run-date {}", NightlyCycle.NAME, runDate);
        JobOutcome outcome = launcher.run(NightlyCycle.NAME, new JobParametersBuilder()
                .addLocalDate(BatchCommandLine.RUN_DATE, runDate)
                .addLong(BatchCommandLine.RUN_ID, clock.millis() ^ System.nanoTime())
                .toJobParameters());
        log.info("cron: {} for {} ended {}{}", NightlyCycle.NAME, runDate, outcome.returnCode().label(),
                outcome.abended() ? " ABEND" : "");
        return Optional.of(outcome);
    }
}
