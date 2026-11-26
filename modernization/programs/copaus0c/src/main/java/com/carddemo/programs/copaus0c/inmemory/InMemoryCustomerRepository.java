package com.carddemo.programs.copaus0c.inmemory;

import com.carddemo.programs.copaus0c.copybook.CustomerRecord;
import com.carddemo.programs.copaus0c.gateway.CustomerRepository;
import com.carddemo.programs.copaus0c.gateway.FileReadResult;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

/** In-memory CUSTDAT KSDS keyed by the 9-digit customer id. */
public final class InMemoryCustomerRepository implements CustomerRepository {
    private final Map<String, CustomerRecord> records = new HashMap<>();
    private FileReadResult<CustomerRecord> forced;

    public InMemoryCustomerRepository add(CustomerRecord record) {
        records.put(String.format("%09d", record.custId()), record);
        return this;
    }

    public void forceResponse(int resp, int resp2) {
        forced = new FileReadResult<>(resp, resp2, null);
    }

    public static InMemoryCustomerRepository load(Path file) throws IOException {
        InMemoryCustomerRepository repo = new InMemoryCustomerRepository();
        for (String line : Files.readAllLines(file)) {
            if (!line.isBlank()) {
                repo.add(CustomerRecord.parse(line));
            }
        }
        return repo;
    }

    @Override
    public FileReadResult<CustomerRecord> read(String custIdKey) {
        if (forced != null) {
            return forced;
        }
        CustomerRecord r = records.get(custIdKey);
        return r == null ? FileReadResult.notFound() : FileReadResult.normal(r);
    }
}
