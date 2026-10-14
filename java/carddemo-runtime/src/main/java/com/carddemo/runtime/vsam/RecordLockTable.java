package com.carddemo.runtime.vsam;

import java.util.HashMap;
import java.util.Map;

/**
 * Region-wide record locks for READ UPDATE on recoverable files: a record held by one task cannot be read for
 * update by another until the holder's SYNCPOINT (or task end). A waiter gives up after {@link #timeoutMillis()}
 * (CICS would suspend until DTIMOUT); the caller reports RECORDBUSY.
 */
public final class RecordLockTable {

    private final Map<String, Object> owners = new HashMap<>();
    private volatile long timeoutMillis = 5000;

    public long timeoutMillis() {
        return timeoutMillis;
    }

    public RecordLockTable timeoutMillis(long millis) {
        this.timeoutMillis = millis;
        return this;
    }

    /** Acquires (or re-enters) the lock on {@code dataset}/{@code key} for {@code owner}; false on timeout. */
    public synchronized boolean acquire(String dataset, String key, Object owner) {
        String id = dataset.trim() + '\u0000' + key;
        long deadline = System.currentTimeMillis() + timeoutMillis;
        while (owners.containsKey(id) && owners.get(id) != owner) {
            long wait = deadline - System.currentTimeMillis();
            if (wait <= 0) {
                return false;
            }
            try {
                wait(wait);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return false;
            }
        }
        owners.put(id, owner);
        return true;
    }

    /** Releases every lock held by {@code owner} (SYNCPOINT, SYNCPOINT ROLLBACK, task end). */
    public synchronized void releaseAll(Object owner) {
        if (owners.values().removeIf(o -> o == owner)) {
            notifyAll();
        }
    }

    public synchronized int size() {
        return owners.size();
    }
}
