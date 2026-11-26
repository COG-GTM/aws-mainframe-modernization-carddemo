package com.carddemo.programs.coactupc.gateway;

import com.carddemo.programs.coactupc.copybook.AccountRecord;
import com.carddemo.programs.coactupc.copybook.CardXrefRecord;
import com.carddemo.programs.coactupc.copybook.CustomerRecord;
import com.carddemo.programs.coactupc.support.Cobol;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * In-memory stand-in for the ACCTDAT / CUSTDAT KSDS files and the CXACAIX alternate index,
 * with READ UPDATE record locks and a recoverable unit of work (SYNCPOINT / ROLLBACK).
 * Responses can be forced per operation to drive error paths.
 */
public final class InMemoryDataStore
        implements CardXrefRepository, TransactionManager {

    /** File control operations that can have a RESP forced. */
    public enum Operation {
        XREF_READ, ACCT_READ, ACCT_READ_UPDATE, ACCT_REWRITE, CUST_READ, CUST_READ_UPDATE, CUST_REWRITE
    }

    private record Forced(int resp, int resp2) {
    }

    private final Map<String, String> accounts = new TreeMap<>();
    private final Map<String, String> customers = new TreeMap<>();
    private final List<CardXrefRecord> xrefs = new ArrayList<>();
    private final Map<Operation, Forced> forced = new EnumMap<>(Operation.class);
    private final Set<String> lockedAccounts = new HashSet<>();
    private final Set<String> lockedCustomers = new HashSet<>();
    private final Map<String, String> accountBeforeImages = new HashMap<>();
    private final Map<String, String> customerBeforeImages = new HashMap<>();
    private int syncpoints;
    private int rollbacks;

    public InMemoryDataStore putAccount(AccountRecord record) {
        accounts.put(record.key(), record.toFixedWidth());
        return this;
    }

    public InMemoryDataStore putCustomer(CustomerRecord record) {
        customers.put(record.key(), record.toFixedWidth());
        return this;
    }

    public InMemoryDataStore putXref(CardXrefRecord record) {
        xrefs.add(record);
        return this;
    }

    /** Loads the fixed-width ASCII extracts (acctdata.txt / custdata.txt / cardxref.txt). */
    public InMemoryDataStore loadAsciiExtracts(Path accountFile, Path customerFile, Path xrefFile) throws IOException {
        for (String line : Files.readAllLines(accountFile, StandardCharsets.UTF_8)) {
            if (!line.isBlank()) {
                putAccount(AccountRecord.parse(line));
            }
        }
        for (String line : Files.readAllLines(customerFile, StandardCharsets.UTF_8)) {
            if (!line.isBlank()) {
                putCustomer(CustomerRecord.parse(line));
            }
        }
        for (String line : Files.readAllLines(xrefFile, StandardCharsets.UTF_8)) {
            if (!line.isBlank()) {
                putXref(CardXrefRecord.parse(line));
            }
        }
        return this;
    }

    /** Forces the next execution of {@code op} to return the given RESP/RESP2. */
    public InMemoryDataStore forceNext(Operation op, int resp, int resp2) {
        forced.put(op, new Forced(resp, resp2));
        return this;
    }

    /** Simulates another task changing the stored account between browse and update. */
    public InMemoryDataStore replaceAccount(AccountRecord record) {
        return putAccount(record);
    }

    public InMemoryDataStore replaceCustomer(CustomerRecord record) {
        return putCustomer(record);
    }

    public AccountRecord account(String key) {
        String image = accounts.get(key);
        return image == null ? null : AccountRecord.parse(image);
    }

    public String accountImage(String key) {
        return accounts.get(key);
    }

    public CustomerRecord customer(String key) {
        String image = customers.get(key);
        return image == null ? null : CustomerRecord.parse(image);
    }

    public int syncpointCount() {
        return syncpoints;
    }

    public int rollbackCount() {
        return rollbacks;
    }

    public boolean isAccountLocked(String key) {
        return lockedAccounts.contains(key);
    }

    @Override
    public FileResponse<CardXrefRecord> readByAcctId(String acctIdKey) {
        Forced f = forced.remove(Operation.XREF_READ);
        if (f != null) {
            return FileResponse.of(f.resp, f.resp2);
        }
        List<CardXrefRecord> matches = new ArrayList<>();
        for (CardXrefRecord x : xrefs) {
            if (x.acctKey().equals(acctIdKey)) {
                matches.add(x);
            }
        }
        if (matches.isEmpty()) {
            return FileResponse.of(CicsResp.NOTFND, 0);
        }
        if (matches.size() > 1) {
            // non-unique alternate index: first record is returned together with DUPKEY
            return new FileResponse<>(CicsResp.DUPKEY, 0, matches.get(0));
        }
        return FileResponse.normal(matches.get(0));
    }

    private FileResponse<AccountRecord> readAccount(String acctIdKey) {
        Forced f = forced.remove(Operation.ACCT_READ);
        if (f != null) {
            return FileResponse.of(f.resp, f.resp2);
        }
        String image = accounts.get(acctIdKey);
        return image == null ? FileResponse.of(CicsResp.NOTFND, 0) : FileResponse.normal(AccountRecord.parse(image));
    }

    private FileResponse<AccountRecord> readAccountForUpdate(String acctIdKey) {
        Forced f = forced.remove(Operation.ACCT_READ_UPDATE);
        if (f != null) {
            return FileResponse.of(f.resp, f.resp2);
        }
        String image = accounts.get(acctIdKey);
        if (image == null) {
            return FileResponse.of(CicsResp.NOTFND, 0);
        }
        lockedAccounts.add(acctIdKey);
        return FileResponse.normal(AccountRecord.parse(image));
    }

    private FileResponse<Void> rewriteAccount(String recordImage) {
        Forced f = forced.remove(Operation.ACCT_REWRITE);
        if (f != null) {
            return FileResponse.of(f.resp, f.resp2);
        }
        String key = recordImage.substring(0, 11);
        if (!lockedAccounts.contains(key)) {
            return FileResponse.of(CicsResp.INVREQ, 30);
        }
        accountBeforeImages.putIfAbsent(key, accounts.get(key));
        accounts.put(key, Cobol.pad(recordImage, AccountRecord.RECORD_LENGTH));
        lockedAccounts.remove(key);
        return FileResponse.normal(null);
    }

    private FileResponse<Void> rewriteCustomer(String recordImage) {
        Forced f = forced.remove(Operation.CUST_REWRITE);
        if (f != null) {
            return FileResponse.of(f.resp, f.resp2);
        }
        String key = recordImage.substring(0, 9);
        if (!lockedCustomers.contains(key)) {
            return FileResponse.of(CicsResp.INVREQ, 30);
        }
        customerBeforeImages.putIfAbsent(key, customers.get(key));
        customers.put(key, Cobol.pad(recordImage, CustomerRecord.RECORD_LENGTH));
        lockedCustomers.remove(key);
        return FileResponse.normal(null);
    }

    /** CUSTDAT view. */
    public CustomerRepository customers() {
        return new CustomerRepository() {
            @Override
            public FileResponse<CustomerRecord> read(String custIdKey) {
                return readCustomer(custIdKey);
            }

            @Override
            public FileResponse<CustomerRecord> readForUpdate(String custIdKey) {
                return readCustomerForUpdate(custIdKey);
            }

            @Override
            public FileResponse<Void> rewrite(String recordImage) {
                return rewriteCustomer(Cobol.pad(recordImage, CustomerRecord.RECORD_LENGTH));
            }
        };
    }

    /** ACCTDAT view. */
    public AccountRepository accounts() {
        return new AccountRepository() {
            @Override
            public FileResponse<AccountRecord> read(String acctIdKey) {
                return readAccount(acctIdKey);
            }

            @Override
            public FileResponse<AccountRecord> readForUpdate(String acctIdKey) {
                return readAccountForUpdate(acctIdKey);
            }

            @Override
            public FileResponse<Void> rewrite(String recordImage) {
                return rewriteAccount(Cobol.pad(recordImage, AccountRecord.RECORD_LENGTH));
            }
        };
    }

    private FileResponse<CustomerRecord> readCustomer(String custIdKey) {
        Forced f = forced.remove(Operation.CUST_READ);
        if (f != null) {
            return FileResponse.of(f.resp, f.resp2);
        }
        String image = customers.get(custIdKey);
        return image == null ? FileResponse.of(CicsResp.NOTFND, 0)
                : FileResponse.normal(CustomerRecord.parse(image));
    }

    private FileResponse<CustomerRecord> readCustomerForUpdate(String custIdKey) {
        Forced f = forced.remove(Operation.CUST_READ_UPDATE);
        if (f != null) {
            return FileResponse.of(f.resp, f.resp2);
        }
        String image = customers.get(custIdKey);
        if (image == null) {
            return FileResponse.of(CicsResp.NOTFND, 0);
        }
        lockedCustomers.add(custIdKey);
        return FileResponse.normal(CustomerRecord.parse(image));
    }

    @Override
    public void syncpoint() {
        syncpoints++;
        accountBeforeImages.clear();
        customerBeforeImages.clear();
        lockedAccounts.clear();
        lockedCustomers.clear();
    }

    @Override
    public void rollback() {
        rollbacks++;
        accountBeforeImages.forEach(accounts::put);
        customerBeforeImages.forEach(customers::put);
        accountBeforeImages.clear();
        customerBeforeImages.clear();
        lockedAccounts.clear();
        lockedCustomers.clear();
    }
}
