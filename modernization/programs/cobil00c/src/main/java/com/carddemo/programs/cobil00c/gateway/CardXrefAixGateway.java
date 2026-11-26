package com.carddemo.programs.cobil00c.gateway;

import com.carddemo.programs.cobil00c.copybook.CardXrefRecord;

/** CICS file CXACAIX: alternate index path over CARDXREF keyed on XREF-ACCT-ID (11 bytes). */
public interface CardXrefAixGateway {

    /** {@code EXEC CICS READ DATASET('CXACAIX') RIDFLD(XREF-ACCT-ID)}. */
    CicsResponse<CardXrefRecord> readByAcctId(String xrefAcctIdKey);
}
