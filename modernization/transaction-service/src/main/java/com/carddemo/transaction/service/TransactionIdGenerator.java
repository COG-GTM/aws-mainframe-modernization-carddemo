package com.carddemo.transaction.service;

import java.time.Instant;
import java.util.concurrent.atomic.AtomicLong;
import org.springframework.stereotype.Component;

/**
 * The COBOL programs derived TRAN-ID from a counter held in the file; here it is a 16 character
 * value built from the epoch millisecond and a per instance sequence, keeping the legacy width.
 */
@Component
public class TransactionIdGenerator {

    private final AtomicLong sequence = new AtomicLong();

    public String next() {
        long millis = Instant.now().toEpochMilli();
        long tail = sequence.incrementAndGet() % 1000;
        return "%013d%03d".formatted(millis, tail);
    }
}
