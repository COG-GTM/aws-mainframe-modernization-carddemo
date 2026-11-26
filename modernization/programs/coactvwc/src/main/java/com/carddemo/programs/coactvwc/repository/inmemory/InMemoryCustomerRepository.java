package com.carddemo.programs.coactvwc.repository.inmemory;

import com.carddemo.programs.coactvwc.copybook.CustomerRecord;
import com.carddemo.programs.coactvwc.repository.CustomerRepository;
import com.carddemo.programs.coactvwc.repository.FileReadResult;
import com.carddemo.programs.coactvwc.support.Cobol;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/** In-memory CUSTDAT KSDS. */
public final class InMemoryCustomerRepository implements CustomerRepository {
    private final Map<String, CustomerRecord> byKey = new HashMap<>();

    public InMemoryCustomerRepository(Collection<CustomerRecord> records) {
        records.forEach(this::put);
    }

    public void put(CustomerRecord r) {
        byKey.put(Cobol.digits(r.custId(), 9), r);
    }

    @Override
    public FileReadResult<CustomerRecord> read(String custIdX) {
        CustomerRecord r = byKey.get(custIdX);
        return r == null ? FileReadResult.notFound() : FileReadResult.normal(r);
    }
}
