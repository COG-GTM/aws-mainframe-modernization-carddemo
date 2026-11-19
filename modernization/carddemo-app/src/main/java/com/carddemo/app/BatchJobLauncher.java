package com.carddemo.app;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchUsageException;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.TreeMap;

/**
 * Resolves {@code --job=<NAME>} to the {@link CardDemoJob} bean of that name. Each job owns how it maps its
 * outcome to a condition code; the launcher only rejects unknown jobs (RC 12). Wave 1 adds POSTTRAN.
 */
@Component
public class BatchJobLauncher {

    /** Runs one registered job and returns its condition code. */
    public interface CardDemoJob {
        int run(BatchArguments arguments) throws Exception;
    }

    private final Map<String, CardDemoJob> jobs;

    public BatchJobLauncher(ListableBeanFactory beans) {
        this.jobs = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
        this.jobs.putAll(beans.getBeansOfType(CardDemoJob.class));
    }

    public int run(BatchArguments arguments) throws Exception {
        CardDemoJob job = jobs.get(arguments.job());
        if (job == null) {
            throw new BatchUsageException("Unknown job " + arguments.job() + "; registered: " + jobs.keySet());
        }
        return job.run(arguments);
    }
}
