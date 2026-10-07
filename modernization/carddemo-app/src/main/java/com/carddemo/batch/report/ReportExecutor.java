package com.carddemo.batch.report;

import org.springframework.beans.factory.DisposableBean;
import org.springframework.core.task.TaskRejectedException;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

/**
 * The internal reader of the web app: one worker thread running submitted report streams in submission order (like
 * a single JES initiator, so two TRANREPT runs never interleave). Deliberately not an {@code Executor} bean, so it
 * does not replace Spring Boot's {@code applicationTaskExecutor}.
 */
public class ReportExecutor implements DisposableBean {

    private final ThreadPoolTaskExecutor pool = new ThreadPoolTaskExecutor();

    public ReportExecutor(int queueCapacity) {
        pool.setCorePoolSize(1);
        pool.setMaxPoolSize(1);
        pool.setQueueCapacity(queueCapacity);
        pool.setThreadNamePrefix("report-");
        pool.setWaitForTasksToCompleteOnShutdown(true);
        pool.setAwaitTerminationSeconds(60);
        pool.initialize();
    }

    /** @throws TaskRejectedException when {@code carddemo.reports.async.queue-capacity} requests are waiting */
    public void execute(Runnable task) {
        pool.execute(task);
    }

    @Override
    public void destroy() {
        pool.shutdown();
    }
}
