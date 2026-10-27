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
            String dalyrejs, String sysout) {
        public Posting {
            if (chunkSize < 1) {
                throw new IllegalArgumentException("carddemo.posting.chunk-size must be at least 1");
            }
            requireFileName("carddemo.posting.dalyrejs", dalyrejs);
            requireFileName("carddemo.posting.sysout", sysout);
        }
    }

    /** {@code transact} and {@code sysout} are file names inside the run directory. */
    public record Interest(Path tcatbalf, Path xreffile, Path acctfile, Path discgrp, String transact, String sysout) {
        public Interest {
            requireFileName("carddemo.interest.transact", transact);
            requireFileName("carddemo.interest.sysout", sysout);
        }
    }

    /** Run outputs are bare file names, so they cannot resolve outside {@code <run-root>/<runId>/}. */
    static void requireFileName(String property, String name) {
        if (name == null || name.isBlank() || name.equals(".") || name.equals("..")
                || !Path.of(name).getFileName().toString().equals(name) || Path.of(name).isAbsolute()) {
            throw new IllegalArgumentException(property + " must be a plain file name, got " + name);
        }
    }
}
