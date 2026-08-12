package com.carddemo.interest.rules;

import java.time.Clock;
import java.time.LocalDateTime;

/**
 * Produces the 26-character Db2 timestamps stamped onto generated transactions.
 *
 * <p>Business rule BR-7b. COBOL paragraph {@code Z-GET-DB2-FORMAT-TIMESTAMP}
 * ({@code app/cbl/CBACT04C.cbl:613-626}) reformats {@code FUNCTION CURRENT-DATE} into
 * {@code YYYY-MM-DD-HH.MM.SS.hh0000}: the COBOL intrinsic only supplies hundredths of a second, so
 * the four low-order digits of the Db2 microsecond field are hard-coded zeros
 * ({@code app/cbl/CBACT04C.cbl:622}).
 *
 * <p>The clock is injected so that batch runs are reproducible and the parity harness can compare
 * generated records byte for byte.
 */
public final class Db2TimestampFormatter {

    private final Clock clock;

    public Db2TimestampFormatter(Clock clock) {
        this.clock = clock;
    }

    public String now() {
        LocalDateTime now = LocalDateTime.now(clock);
        int hundredths = now.getNano() / 10_000_000;
        return "%04d-%02d-%02d-%02d.%02d.%02d.%02d0000".formatted(
                now.getYear(), now.getMonthValue(), now.getDayOfMonth(),
                now.getHour(), now.getMinute(), now.getSecond(), hundredths);
    }
}
