package com.carddemo.programs.copaus0c.gateway;

import com.carddemo.programs.copaus0c.copybook.CustomerRecord;

/** CICS READ DATASET('CUSTDAT ') keyed by customer id. */
public interface CustomerRepository {
    String DATASET = "CUSTDAT ";

    /** @param custIdKey WS-CARD-RID-CUST-ID-X PIC X(09) */
    FileReadResult<CustomerRecord> read(String custIdKey);
}
