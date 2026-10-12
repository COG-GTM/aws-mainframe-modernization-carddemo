package com.carddemo.zerotrust.posting;

import com.carddemo.posting.domain.PostingOutcome;

import java.math.BigDecimal;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Running posted count and posted-amount sum for the job-level JOB_COMPLETED audit entry,
 * accumulated by the secured processor alongside the posttran control totals.
 */
public final class PostedTotals {

    private final AtomicLong postedCount = new AtomicLong();
    private final AtomicReference<BigDecimal> postedAmount = new AtomicReference<>(BigDecimal.ZERO);

    public void reset() {
        postedCount.set(0);
        postedAmount.set(BigDecimal.ZERO);
    }

    public void record(PostingOutcome outcome) {
        if (outcome instanceof PostingOutcome.Posted p) {
            postedCount.incrementAndGet();
            postedAmount.accumulateAndGet(p.transaction().amount(), BigDecimal::add);
        }
    }

    public long postedCount() {
        return postedCount.get();
    }

    public BigDecimal postedAmount() {
        return postedAmount.get();
    }
}
