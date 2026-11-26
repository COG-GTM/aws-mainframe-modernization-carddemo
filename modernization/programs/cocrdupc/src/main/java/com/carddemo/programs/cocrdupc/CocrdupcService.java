package com.carddemo.programs.cocrdupc;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.function.Supplier;

/** Entry point for transaction CCUP (program COCRDUPC). Each call is one pseudo-conversational task. */
public final class CocrdupcService {

    private final CardFileGateway cardFile;
    private final Supplier<LocalDateTime> clock;

    public CocrdupcService(CardFileGateway cardFile) {
        this(cardFile, LocalDateTime::now);
    }

    public CocrdupcService(CardFileGateway cardFile, Supplier<LocalDateTime> clock) {
        this.cardFile = Objects.requireNonNull(cardFile);
        this.clock = Objects.requireNonNull(clock);
    }

    /** @throws AbendException when the program ends via ABEND-ROUTINE (ABCODE 9999). */
    public CocrdupcResponse process(CocrdupcRequest request) {
        return new CocrdupcProgram(cardFile, clock).run(request);
    }
}
