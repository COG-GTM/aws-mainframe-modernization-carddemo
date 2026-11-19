package com.carddemo.app;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.JclExitCodeMapper;
import com.carddemo.common.batch.JclJobLauncher;
import com.carddemo.common.batch.JclSteps;
import com.carddemo.common.batch.ReturnCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.item.support.AbstractItemCountingItemStreamItemReader;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.transaction.PlatformTransactionManager;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;

/** The launch seam on a real chunk-1 job: restart resumes after the last commit; RC 4 / 16 / 12 mapping. */
@SpringBootTest
class JclLaunchSeamRestartTest extends PostgresContainerSupport {

    static final List<Integer> WRITTEN = new CopyOnWriteArrayList<>();
    static final List<Boolean> RESTART_FLAGS = new CopyOnWriteArrayList<>();
    static final AtomicInteger FAIL_ON = new AtomicInteger();
    static final AtomicBoolean RAISE_RC4 = new AtomicBoolean();

    @Autowired
    JclJobLauncher launcher;

    @BeforeEach
    void reset() {
        WRITTEN.clear();
        RESTART_FLAGS.clear();
        FAIL_ON.set(0);
        RAISE_RC4.set(false);
    }

    @Test
    void failedRunIsRestartedFromLastCommitWithSameRunId() {
        FAIL_ON.set(3);
        assertThat(launcher.run(BatchArguments.parse("--job=SEAMTEST", "--run-id=R-RESTART"))).isEqualTo(ReturnCode.ABEND);
        assertThat(WRITTEN).containsExactly(1, 2);

        FAIL_ON.set(0);
        assertThat(launcher.run(BatchArguments.parse("--job=SEAMTEST", "--run-id=R-RESTART"))).isEqualTo(ReturnCode.OK);
        assertThat(WRITTEN).containsExactly(1, 2, 3, 4, 5);
        assertThat(RESTART_FLAGS).containsExactly(false, true);
    }

    @Test
    void newRunIdIsANewInstanceAndRaisedRcIsReturned() {
        RAISE_RC4.set(true);
        assertThat(launcher.run(BatchArguments.parse("--job=seamtest"))).isEqualTo(ReturnCode.WARNING);
        assertThat(WRITTEN).containsExactly(1, 2, 3, 4, 5);
        assertThat(RESTART_FLAGS).containsExactly(false);
    }

    @TestConfiguration
    static class SeamTestJob {

        @Bean
        Job seamTestJob(JobRepository jobRepository, PlatformTransactionManager tx, JobExplorer explorer) {
            AbstractItemCountingItemStreamItemReader<Integer> reader = new AbstractItemCountingItemStreamItemReader<>() {
                @Override
                protected Integer doRead() {
                    return getCurrentItemCount() <= 5 ? getCurrentItemCount() : null;
                }

                @Override
                protected void doOpen() {
                }

                @Override
                protected void doClose() {
                }
            };
            reader.setName("seamTestReader");
            return JclSteps.job("SEAMTEST", jobRepository)
                    .listener(new JobExecutionListener() {
                        @Override
                        public void beforeJob(JobExecution execution) {
                            RESTART_FLAGS.add(JclSteps.isRestart(execution, explorer));
                        }

                        @Override
                        public void afterJob(JobExecution execution) {
                            if (RAISE_RC4.get()) {
                                JclExitCodeMapper.raise(execution, ReturnCode.WARNING);
                            }
                        }
                    })
                    .start(JclSteps.<Integer, Integer>chunkStep("STEP01", jobRepository, tx)
                            .reader(reader)
                            .processor(item -> {
                                if (item == FAIL_ON.get()) {
                                    throw new IllegalStateException("injected failure on item " + item);
                                }
                                return item;
                            })
                            .writer(chunk -> WRITTEN.addAll(chunk.getItems()))
                            .build())
                    .build();
        }
    }
}
