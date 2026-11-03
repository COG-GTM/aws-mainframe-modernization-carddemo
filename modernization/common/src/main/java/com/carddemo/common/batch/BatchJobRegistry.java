package com.carddemo.common.batch;

import org.springframework.batch.core.Job;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/** The jobs a service can launch, keyed by JCL member name (POSTTRAN, INTCALC, ...). */
public final class BatchJobRegistry {

    private final Map<String, Job> jobs;

    public BatchJobRegistry(Map<String, Job> jobs) {
        Map<String, Job> copy = new LinkedHashMap<>();
        jobs.forEach((name, job) -> copy.put(name.toUpperCase(Locale.ROOT), job));
        this.jobs = Collections.unmodifiableMap(copy);
    }

    public static BatchJobRegistry empty() {
        return new BatchJobRegistry(Map.of());
    }

    public Job job(String jclName) {
        Job job = jobs.get(jclName.toUpperCase(Locale.ROOT));
        if (job == null) {
            throw new UnknownJobException(jclName, jobs.keySet());
        }
        return job;
    }

    public Set<String> names() {
        return jobs.keySet();
    }

    public static final class UnknownJobException extends IllegalArgumentException {
        public UnknownJobException(String name, Set<String> known) {
            super("Unknown job '" + name + "'; this service runs " + (known.isEmpty() ? "no jobs yet" : known));
        }
    }
}
