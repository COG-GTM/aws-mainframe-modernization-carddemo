package com.carddemo.batch.load;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.card.Card;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardRepository;
import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.common.file.RecordFiles;
import com.carddemo.customer.Customer;
import com.carddemo.customer.CustomerRecord;
import com.carddemo.customer.CustomerRepository;
import com.carddemo.transaction.DailyTransaction;
import com.carddemo.transaction.DailyTransactionRecord;
import com.carddemo.transaction.DailyTransactionRepository;
import com.carddemo.transaction.DisclosureGroup;
import com.carddemo.transaction.DisclosureGroupRecord;
import com.carddemo.transaction.DisclosureGroupRepository;
import com.carddemo.transaction.TranCatBalance;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TranCatBalanceRepository;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionCategory;
import com.carddemo.transaction.TransactionCategoryRecord;
import com.carddemo.transaction.TransactionCategoryRepository;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import com.carddemo.transaction.TransactionType;
import com.carddemo.transaction.TransactionTypeRecord;
import com.carddemo.transaction.TransactionTypeRepository;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRecord;
import com.carddemo.user.UserSecurityRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.PersistenceUnitUtil;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * Loads a VSAM/sequential dataset image (the IDCAMS {@code REPRO} of the setup jobs) into its table through the
 * copybook mappers. EBCDIC files are fixed-length records, ASCII files are line sequential (as under
 * {@code app/data}). Card cross-references reference cards, customers and accounts; the FKs are deferred, so a
 * transaction may load them in any order.
 */
@Component
public class VsamDatasetLoader {

    /** The eleven datasets, by ddname/dataset suffix, with their copybook mapper. */
    public enum Dataset {
        USRSEC(UserSecurityRecord.MAPPER),
        CUSTDATA(CustomerRecord.MAPPER),
        ACCTDATA(AccountRecord.MAPPER),
        CARDDATA(CardRecord.MAPPER),
        CARDXREF(CardXrefRecord.MAPPER),
        TRANSACT(TransactionRecord.MAPPER),
        DALYTRAN(DailyTransactionRecord.MAPPER),
        TRANTYPE(TransactionTypeRecord.MAPPER),
        TRANCATG(TransactionCategoryRecord.MAPPER),
        DISCGRP(DisclosureGroupRecord.MAPPER),
        TCATBALF(TranCatBalanceRecord.MAPPER);

        private final CopybookRecordMapper<?> mapper;

        Dataset(CopybookRecordMapper<?> mapper) {
            this.mapper = mapper;
        }

        public CopybookRecordMapper<?> mapper() {
            return mapper;
        }

        public List<FixedWidthRecord> read(Path file, RecordEncoding encoding) {
            return encoding == RecordEncoding.EBCDIC
                    ? RecordFiles.readFixed(name(), file, mapper.layout(), encoding)
                    : RecordFiles.readLines(name(), file, mapper.layout(), encoding);
        }
    }

    private static final int FLUSH_INTERVAL = 500;

    private final UserSecurityRepository users;
    private final CustomerRepository customers;
    private final AccountRepository accounts;
    private final CardRepository cards;
    private final CardXrefRepository xrefs;
    private final TransactionRepository transactions;
    private final DailyTransactionRepository dailyTransactions;
    private final TransactionTypeRepository types;
    private final TransactionCategoryRepository categories;
    private final DisclosureGroupRepository disclosureGroups;
    private final TranCatBalanceRepository balances;

    @PersistenceContext
    private EntityManager entityManager;

    public VsamDatasetLoader(UserSecurityRepository users, CustomerRepository customers, AccountRepository accounts,
                             CardRepository cards, CardXrefRepository xrefs, TransactionRepository transactions,
                             DailyTransactionRepository dailyTransactions, TransactionTypeRepository types,
                             TransactionCategoryRepository categories, DisclosureGroupRepository disclosureGroups,
                             TranCatBalanceRepository balances) {
        this.users = users;
        this.customers = customers;
        this.accounts = accounts;
        this.cards = cards;
        this.xrefs = xrefs;
        this.transactions = transactions;
        this.dailyTransactions = dailyTransactions;
        this.types = types;
        this.categories = categories;
        this.disclosureGroups = disclosureGroups;
        this.balances = balances;
    }

    /** Replaces the table with the dataset image; any reject fails the load. Returns the record count. */
    @Transactional
    public int load(Dataset dataset, Path file, RecordEncoding encoding) {
        return load(dataset, dataset.read(file, encoding));
    }

    /**
     * Replaces the table's contents with the dataset image (IDCAMS DELETE/DEFINE + REPRO): existing rows are deleted,
     * then every record is inserted. A record that cannot be mapped fails the load. Returns the record count.
     */
    @Transactional
    public int load(Dataset dataset, List<FixedWidthRecord> records) {
        LoadResult result = load(dataset, records, LoadMode.REPLACE);
        if (!result.rejects().isEmpty()) {
            LoadResult.Reject first = result.rejects().get(0);
            throw new IllegalArgumentException(dataset + " record " + first.recordNumber() + ": " + first.reason());
        }
        return records.size();
    }

    @Transactional
    public LoadResult load(Dataset dataset, Path file, RecordEncoding encoding, LoadMode mode) {
        return load(dataset, dataset.read(file, encoding), mode);
    }

    /**
     * Loads the dataset image in {@code mode}. Records whose data items are all LOW-VALUES are counted as
     * {@link LoadResult#empty()} and not stored; records the mapper refuses are returned as rejects and not stored.
     * Inserts go through {@link EntityManager#persist} in JDBC batches ({@code hibernate.jdbc.batch_size}).
     */
    @Transactional
    public LoadResult load(Dataset dataset, List<FixedWidthRecord> records, LoadMode mode) {
        List<Object> entities = new ArrayList<>(records.size());
        List<LoadResult.Reject> rejects = new ArrayList<>();
        int empty = 0;
        for (int i = 0; i < records.size(); i++) {
            FixedWidthRecord record = records.get(i);
            if (dataset.mapper().isLowValues(record)) {
                empty++;
                continue;
            }
            try {
                entities.add(toEntity(dataset, i + 1, record));
            } catch (RuntimeException e) {
                rejects.add(new LoadResult.Reject(i + 1, String.valueOf(e.getMessage())));
            }
        }
        if (mode == LoadMode.REPLACE) {
            repository(dataset).deleteAllInBatch();
        } else {
            removeExisting(entities);
        }
        entityManager.flush();
        entityManager.clear();
        for (int i = 0; i < entities.size(); i++) {
            entityManager.persist(entities.get(i));
            if ((i + 1) % FLUSH_INTERVAL == 0) {
                entityManager.flush();
                entityManager.clear();
            }
        }
        entityManager.flush();
        entityManager.clear();
        return new LoadResult(dataset, records.size(), entities.size(), empty, rejects);
    }

    /** Rows currently in the dataset's table. */
    @Transactional(readOnly = true)
    public long count(Dataset dataset) {
        return repository(dataset).count();
    }

    private void removeExisting(List<Object> entities) {
        PersistenceUnitUtil ids = entityManager.getEntityManagerFactory().getPersistenceUnitUtil();
        for (Object entity : entities) {
            Object existing = entityManager.find(entity.getClass(), ids.getIdentifier(entity));
            if (existing != null) {
                entityManager.remove(existing);
            }
        }
    }

    private static Object toEntity(Dataset dataset, int recordNumber, FixedWidthRecord record) {
        return switch (dataset) {
            case USRSEC -> UserSecurity.from(UserSecurityRecord.MAPPER.fromRecord(record));
            case CUSTDATA -> Customer.from(CustomerRecord.MAPPER.fromRecord(record));
            case ACCTDATA -> Account.from(AccountRecord.MAPPER.fromRecord(record));
            case CARDDATA -> Card.from(CardRecord.MAPPER.fromRecord(record));
            case CARDXREF -> CardXref.from(CardXrefRecord.MAPPER.fromRecord(record));
            case TRANSACT -> Transaction.from(TransactionRecord.MAPPER.fromRecord(record));
            case DALYTRAN -> DailyTransaction.from(recordNumber, DailyTransactionRecord.MAPPER.fromRecord(record));
            case TRANTYPE -> TransactionType.from(TransactionTypeRecord.MAPPER.fromRecord(record));
            case TRANCATG -> TransactionCategory.from(TransactionCategoryRecord.MAPPER.fromRecord(record));
            case DISCGRP -> DisclosureGroup.from(DisclosureGroupRecord.MAPPER.fromRecord(record));
            case TCATBALF -> TranCatBalance.from(TranCatBalanceRecord.MAPPER.fromRecord(record));
        };
    }

    private JpaRepository<?, ?> repository(Dataset dataset) {
        return switch (dataset) {
            case USRSEC -> users;
            case CUSTDATA -> customers;
            case ACCTDATA -> accounts;
            case CARDDATA -> cards;
            case CARDXREF -> xrefs;
            case TRANSACT -> transactions;
            case DALYTRAN -> dailyTransactions;
            case TRANTYPE -> types;
            case TRANCATG -> categories;
            case DISCGRP -> disclosureGroups;
            case TCATBALF -> balances;
        };
    }
}
