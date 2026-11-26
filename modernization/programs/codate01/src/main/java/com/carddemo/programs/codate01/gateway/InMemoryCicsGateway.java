package com.carddemo.programs.codate01.gateway;

import com.carddemo.programs.codate01.CicsTime;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.function.Supplier;

/** In-memory CICS: fixed RETRIEVE data, a pluggable clock and counters for SYNCPOINT / RETURN. */
public final class InMemoryCicsGateway implements CicsGateway {

    private final RetrieveResult retrieveResult;
    private final Supplier<LocalDateTime> clock;
    private int retrieveCount;
    private int askTimeCount;
    private int syncpointCount;
    private int returnCount;

    public InMemoryCicsGateway(RetrieveResult retrieveResult, Supplier<LocalDateTime> clock) {
        this.retrieveResult = retrieveResult;
        this.clock = clock;
    }

    public InMemoryCicsGateway(RetrieveResult retrieveResult, Clock clock) {
        this(retrieveResult, () -> LocalDateTime.now(clock));
    }

    @Override
    public RetrieveResult retrieve() {
        retrieveCount++;
        return retrieveResult;
    }

    @Override
    public long askTime() {
        askTimeCount++;
        return CicsTime.absTime(clock.get());
    }

    @Override
    public void syncpoint() {
        syncpointCount++;
    }

    @Override
    public void returnToCics() {
        returnCount++;
    }

    public int retrieveCount() { return retrieveCount; }
    public int askTimeCount() { return askTimeCount; }
    public int syncpointCount() { return syncpointCount; }
    public int returnCount() { return returnCount; }
}
