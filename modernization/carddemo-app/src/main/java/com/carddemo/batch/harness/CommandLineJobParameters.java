package com.carddemo.batch.harness;

import org.springframework.batch.core.JobParameters;

/**
 * Turns the raw CLI parameters of one job into the parameters it expects, for jobs whose identity is derived (e.g.
 * {@code initial-load} adds the SHA-256 of its source files). Throw {@link IllegalArgumentException} for invalid
 * input; the launch then fails with RC 16.
 */
public interface CommandLineJobParameters {

    String jobName();

    JobParameters adapt(JobParameters raw);
}
