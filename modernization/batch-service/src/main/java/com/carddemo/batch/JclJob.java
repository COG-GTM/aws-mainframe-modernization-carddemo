package com.carddemo.batch;

import org.springframework.batch.core.Job;

/** A job batch-service launches, under its JCL member name; every bean of this type is registered. */
public record JclJob(String member, Job job) {
}
