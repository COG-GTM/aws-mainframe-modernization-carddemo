package com.carddemo.programs.cotrtlic;

import com.carddemo.programs.cotrtlic.gateway.CicsSyncpoint;
import com.carddemo.programs.cotrtlic.gateway.Dsntiac;
import com.carddemo.programs.cotrtlic.gateway.TransactionTypeDb2Gateway;
import java.time.Clock;
import java.time.LocalDateTime;

/**
 * COTRTLIC (transaction CTLI): list, filter, page, update and delete CARDDEMO.TRANSACTION_TYPE rows.
 * Each call is one pseudo-conversational CICS task; state round-trips in the returned commarea.
 */
public final class CotrtlicService {

    private final TransactionTypeDb2Gateway db;
    private final Dsntiac dsntiac;
    private final CicsSyncpoint cics;
    private final Clock clock;

    public CotrtlicService(TransactionTypeDb2Gateway db, Dsntiac dsntiac, CicsSyncpoint cics, Clock clock) {
        this.db = db;
        this.dsntiac = dsntiac;
        this.cics = cics;
        this.clock = clock;
    }

    public CicsResponse handle(CicsRequest request) {
        return new CotrtlicTask(db, dsntiac, cics, LocalDateTime.now(clock)).run(request);
    }
}
