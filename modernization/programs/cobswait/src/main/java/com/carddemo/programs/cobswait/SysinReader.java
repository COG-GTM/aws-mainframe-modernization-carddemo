package com.carddemo.programs.cobswait;

import java.util.Optional;

/** Source for {@code ACCEPT ... FROM SYSIN}: one SYSIN record per call. */
@FunctionalInterface
public interface SysinReader {

    /** Next SYSIN record, or empty at end of file. */
    Optional<String> acceptRecord();
}
