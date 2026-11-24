package com.carddemo.transaction.cobol;

import org.springframework.dao.DataAccessException;

import java.util.function.Supplier;

/**
 * Executes a keyed-file operation against the JPA store, translating unexpected persistence failures
 * into the COBOL "display IO status + abend" path (file status 30 - permanent I/O error).
 */
public final class VsamIo {

    private VsamIo() {
    }

    public static <T> T perform(String ddName, String errorMessage, Supplier<T> operation) {
        try {
            return operation.get();
        } catch (DataAccessException e) {
            throw new CobolFileException(ddName, FileStatus.PERMANENT_IO_ERROR, errorMessage, e);
        }
    }

    public static void perform(String ddName, String errorMessage, Runnable operation) {
        perform(ddName, errorMessage, () -> {
            operation.run();
            return null;
        });
    }
}
