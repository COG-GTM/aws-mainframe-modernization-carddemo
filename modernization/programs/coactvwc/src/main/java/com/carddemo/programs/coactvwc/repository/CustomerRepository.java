package com.carddemo.programs.coactvwc.repository;

import com.carddemo.programs.coactvwc.copybook.CustomerRecord;

/** Dataset CUSTDAT: customer master KSDS keyed by CUST-ID. */
public interface CustomerRepository {
    /** READ DATASET('CUSTDAT') RIDFLD(custIdX) KEYLENGTH(9). */
    FileReadResult<CustomerRecord> read(String custIdX);
}
