package com.carddemo.common.online;

import java.util.Optional;

/** {@code EXEC CICS INQUIRE PROGRAM}: which programs can be the target of an {@code XCTL} (ADR-0008). */
public interface InstalledPrograms {

    /** The transaction id of an installed program, empty when the program is not installed. */
    Optional<String> tranIdOf(String programId);

    default boolean isInstalled(String programId) {
        return tranIdOf(programId).isPresent();
    }
}
