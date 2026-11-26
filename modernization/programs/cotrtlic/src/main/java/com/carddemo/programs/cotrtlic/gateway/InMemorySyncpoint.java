package com.carddemo.programs.cotrtlic.gateway;

/** Counts SYNCPOINT requests. */
public final class InMemorySyncpoint implements CicsSyncpoint {
    private int count;

    @Override
    public void syncpoint() {
        count++;
    }

    public int count() {
        return count;
    }
}
