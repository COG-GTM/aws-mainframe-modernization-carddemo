package com.carddemo.batch.harness;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.job.AbstractJob;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Registers a listener on every job bean so each run, whoever launches it (CLI, scheduler, a test), lands in
 * {@code batch_run} with its RC, and the RC is appended to the job's exit description ({@code RC=0004}).
 */
@Configuration(proxyBeanMethods = false)
public class BatchRunRecording {

    private static final Logger log = LoggerFactory.getLogger(BatchRunRecording.class);

    @Bean
    static BeanPostProcessor batchRunListenerRegistrar(ObjectProvider<BatchRunLog> runLog) {
        JobExecutionListener listener = new JobExecutionListener() {
            @Override
            public void afterJob(JobExecution job) {
                ReturnCode rc = runLog.getObject().record(job);
                job.setExitStatus(job.getExitStatus().addExitDescription(rc.label()));
                log.info("{} ended {} {}", job.getJobInstance().getJobName(), job.getStatus(), rc.label());
            }
        };
        return new BeanPostProcessor() {
            @Override
            public Object postProcessAfterInitialization(Object bean, String beanName) {
                if (bean instanceof AbstractJob job) {
                    job.registerJobExecutionListener(listener);
                }
                return bean;
            }
        };
    }

}
