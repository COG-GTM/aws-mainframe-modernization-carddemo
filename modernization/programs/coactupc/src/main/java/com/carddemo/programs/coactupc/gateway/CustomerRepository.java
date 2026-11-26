package com.carddemo.programs.coactupc.gateway;

import com.carddemo.programs.coactupc.copybook.CustomerRecord;

/** CUSTDAT - customer master VSAM KSDS. */
public interface CustomerRepository {

    /** EXEC CICS READ DATASET('CUSTDAT') RIDFLD(cust-id X(09)). */
    FileResponse<CustomerRecord> read(String custIdKey);

    /** EXEC CICS READ FILE('CUSTDAT') UPDATE RIDFLD(cust-id X(09)). */
    FileResponse<CustomerRecord> readForUpdate(String custIdKey);

    /** EXEC CICS REWRITE FILE('CUSTDAT') FROM(500-byte record image). */
    FileResponse<Void> rewrite(String recordImage);
}
