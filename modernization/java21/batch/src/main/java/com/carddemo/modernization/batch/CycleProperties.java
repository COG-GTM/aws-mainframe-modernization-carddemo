package com.carddemo.modernization.batch;

import java.nio.file.Path;
import org.springframework.boot.context.properties.ConfigurationProperties;

/** DD names from POSTTRAN.jcl and INTCALC.jcl mapped to file resources; see the module README. */
@ConfigurationProperties("carddemo")
public record CycleProperties(String encoding, Path runRoot, ClockSettings clock, Posting posting, Interest interest) {

    /** {@code fixedInstant} pins the clock (ISO-8601 instant); blank means the system clock. */
    public record ClockSettings(String fixedInstant, String zone) {}

    /** {@code dalyrejs} and {@code sysout} are file names inside the run directory. */
    public record Posting(int chunkSize, Path tranfile, Path dalytran, Path xreffile, Path acctfile, Path tcatbalf,
            String dalyrejs, String sysout) {}

    /** {@code transact} and {@code sysout} are file names inside the run directory. */
    public record Interest(Path tcatbalf, Path xreffile, Path acctfile, Path discgrp, String transact, String sysout) {}
}
