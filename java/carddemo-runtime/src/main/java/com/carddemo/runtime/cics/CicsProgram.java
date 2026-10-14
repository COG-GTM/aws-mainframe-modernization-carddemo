package com.carddemo.runtime.cics;

import com.carddemo.runtime.cobol.RecordLayout;
import java.util.Optional;

/**
 * A migrated CICS program. {@link #execute} runs one task; it ends by calling one of the non-returning
 * {@link CicsContext} control commands (RETURN, RETURN TRANSID, XCTL, ABEND) or by returning normally
 * (equivalent to GOBACK / plain RETURN).
 *
 * <p>Implementations must be stateless: all WORKING-STORAGE is created per task inside {@link #execute}.
 */
public interface CicsProgram {

    /** The 8-character CICS program name, e.g. {@code COSGN00C}. */
    String programName();

    /** The transaction that starts this program (CSD TRANSACTION -> PROGRAM), if any. */
    Optional<String> transactionId();

    /** Layout used to decode the commarea this program passes on (for diagnostics/JSON); optional. */
    default Optional<RecordLayout> commareaLayout() {
        return Optional.empty();
    }

    void execute(CicsContext ctx);
}
