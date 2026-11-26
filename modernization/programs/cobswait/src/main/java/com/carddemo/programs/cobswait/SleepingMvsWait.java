package com.carddemo.programs.cobswait;

import com.carddemo.programs.cobswait.copybook.MvswaitTime;

/**
 * Java equivalent of MVSWAIT.asm: start an interval timer for the delay (ASMWAIT BINLBL), then
 * {@code SR 15,15} so the return code is always 0.
 */
public final class SleepingMvsWait implements MvsWait {

    /** Pluggable sleep so tests do not block. */
    @FunctionalInterface
    public interface Sleeper {
        void sleep(long millis) throws InterruptedException;
    }

    private final Sleeper sleeper;

    public SleepingMvsWait() {
        this(Thread::sleep);
    }

    public SleepingMvsWait(Sleeper sleeper) {
        this.sleeper = sleeper;
    }

    @Override
    public int call(MvswaitTime time) {
        try {
            sleeper.sleep(time.millis());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        // SR 15,15  ZERO R15 BEFORE RETURN
        return 0;
    }
}
