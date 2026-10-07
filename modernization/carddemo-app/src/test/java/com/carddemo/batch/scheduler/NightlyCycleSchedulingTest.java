package com.carddemo.batch.scheduler;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.JobOutcome;
import com.carddemo.batch.harness.ReturnCode;
import java.io.IOException;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.boot.env.YamlPropertySourceLoader;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.boot.test.context.runner.WebApplicationContextRunner;
import org.springframework.core.env.PropertySource;
import org.springframework.core.io.ClassPathResource;
import org.springframework.scheduling.support.CronExpression;

/** The in-app cron: registered only in the web app with the flag on, off in test/golden, launches for today. */
class NightlyCycleSchedulingTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2022-07-06T22:00:00Z"), ZoneOffset.UTC);

    private final BatchJobLauncher launcher = mock(BatchJobLauncher.class);
    private final JobExplorer explorer = mock(JobExplorer.class);

    private WebApplicationContextRunner web() {
        return new WebApplicationContextRunner().withUserConfiguration(NightlyCycleScheduling.class)
                .withBean(BatchJobLauncher.class, () -> launcher).withBean(JobExplorer.class, () -> explorer)
                .withBean(Clock.class, () -> CLOCK)
                .withPropertyValues("carddemo.batch.scheduler.nightly-cycle.cron=0 0 22 * * *");
    }

    @Test
    void triggerOnlyInTheWebAppWithTheFlagOn() {
        web().withPropertyValues("carddemo.batch.scheduler.enabled=true")
                .run(c -> assertThat(c).hasSingleBean(NightlyCycleTrigger.class));
        web().withPropertyValues("carddemo.batch.scheduler.enabled=false")
                .run(c -> assertThat(c).doesNotHaveBean(NightlyCycleTrigger.class));
        web().run(c -> assertThat(c).doesNotHaveBean(NightlyCycleTrigger.class));
        new ApplicationContextRunner().withUserConfiguration(NightlyCycleScheduling.class)
                .withPropertyValues("carddemo.batch.scheduler.enabled=true")
                .run(c -> assertThat(c).doesNotHaveBean(NightlyCycleTrigger.class));
    }

    @Test
    void cronIsConfiguredAndDisabledUnderTestAndGolden() throws IOException {
        assertThat(property("application.yml", "carddemo.batch.scheduler.enabled"))
                .isEqualTo("${CARDDEMO_SCHEDULER_ENABLED:true}");
        String cron = property("application.yml", "carddemo.batch.scheduler.nightly-cycle.cron");
        assertThat(cron).isEqualTo("${CARDDEMO_NIGHTLY_CYCLE_CRON:0 0 22 * * *}");
        assertThat(CronExpression.isValidExpression(cron.substring(cron.indexOf(':') + 1, cron.length() - 1)))
                .isTrue();
        assertThat(property("application-test.yml", "carddemo.batch.scheduler.enabled")).isEqualTo("false");
        assertThat(property("application-golden.yml", "carddemo.batch.scheduler.enabled")).isEqualTo("false");
    }

    @Test
    void fireLaunchesTheCycleForTheClockDate() {
        when(explorer.findRunningJobExecutions(NightlyCycle.NAME)).thenReturn(Set.of());
        when(launcher.run(eq(NightlyCycle.NAME), any())).thenReturn(
                new JobOutcome(NightlyCycle.NAME, null, BatchStatus.COMPLETED, ReturnCode.WARNING, false, null));
        new NightlyCycleTrigger(launcher, explorer, CLOCK).fire();
        ArgumentCaptor<JobParameters> parameters = ArgumentCaptor.forClass(JobParameters.class);
        verify(launcher).run(eq(NightlyCycle.NAME), parameters.capture());
        assertThat(parameters.getValue().getLocalDate("run-date")).isEqualTo(LocalDate.of(2022, 7, 6));
    }

    @Test
    void fireIsSkippedWhileACycleIsRunning() {
        when(explorer.findRunningJobExecutions(NightlyCycle.NAME)).thenReturn(Set.of(new JobExecution(1L)));
        assertThat(new NightlyCycleTrigger(launcher, explorer, CLOCK).launch(LocalDate.of(2022, 7, 6))).isEmpty();
        verify(launcher, never()).run(any(), any());
    }

    private static String property(String file, String name) throws IOException {
        for (PropertySource<?> source : new YamlPropertySourceLoader().load(file, new ClassPathResource(file))) {
            Object value = source.getProperty(name);
            if (value != null) {
                return value.toString();
            }
        }
        return null;
    }
}
