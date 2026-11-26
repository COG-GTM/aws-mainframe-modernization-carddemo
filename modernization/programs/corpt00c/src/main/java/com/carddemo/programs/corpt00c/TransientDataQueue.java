package com.carddemo.programs.corpt00c;

/** Gateway for {@code EXEC CICS WRITEQ TD QUEUE(..) FROM(..) LENGTH(..) RESP RESP2}. */
public interface TransientDataQueue {

    CicsResponse writeq(String queue, String record);
}
