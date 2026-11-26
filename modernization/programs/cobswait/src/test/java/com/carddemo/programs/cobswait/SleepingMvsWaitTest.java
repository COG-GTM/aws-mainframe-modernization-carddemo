package com.carddemo.programs.cobswait;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cobswait.copybook.MvswaitTime;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class SleepingMvsWaitTest {

    @Test
    void sleepsCentisecondsTimesTenMillisAndReturnsR15Zero() {
        List<Long> slept = new ArrayList<>();
        int rc = new SleepingMvsWait(slept::add).call(new MvswaitTime(3600));
        assertEquals(List.of(36_000L), slept);
        assertEquals(0, rc);
    }

    @Test
    void interruptReturnsZeroAndKeepsInterruptFlag() {
        int rc = new SleepingMvsWait(ms -> { throw new InterruptedException(); })
                .call(new MvswaitTime(1));
        assertEquals(0, rc);
        assertTrue(Thread.interrupted());
    }

    @Test
    void realSleepWaitsAtLeastRequestedTime() {
        long start = System.nanoTime();
        assertEquals(0, new SleepingMvsWait().call(new MvswaitTime(5)));
        assertTrue(System.nanoTime() - start >= 50_000_000L);
    }
}
