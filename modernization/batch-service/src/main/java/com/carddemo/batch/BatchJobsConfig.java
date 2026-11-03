package com.carddemo.batch;

import com.carddemo.batch.tranrept.TranreptJobConfig;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchJobRegistry;
import java.nio.file.Path;
import java.time.Clock;
import java.util.LinkedHashMap;
import java.util.Map;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/** The jobs batch-service launches, keyed by JCL member: every {@link JclJob} bean of the job packages. */
@Configuration
public class BatchJobsConfig {

    @Bean
    public BatchJobRegistry batchJobRegistry(ObjectProvider<JclJob> jobs) {
        Map<String, Job> byMember = new LinkedHashMap<>();
        jobs.orderedStream().forEach(jcl -> {
            if (byMember.put(jcl.member(), jcl.job()) != null) {
                throw new IllegalStateException("Two jobs registered for " + jcl.member());
            }
        });
        return new BatchJobRegistry(byMember);
    }

    @Bean
    public BatchJobLauncher batchJobLauncher(JobLauncher jobLauncher, BatchJobRegistry registry,
                                             @Value("${carddemo.batch.output-dir:out}") Path outputDir,
                                             Clock clock) {
        return new BatchJobLauncher(jobLauncher, registry, outputDir, clock);
    }

    @Bean
    JclJob tranreptJclJob(@Qualifier(TranreptJobConfig.JOB_NAME) Job transactionReportJob) {
        return new JclJob(TranreptJobConfig.TRANREPT, transactionReportJob);
    }
}
