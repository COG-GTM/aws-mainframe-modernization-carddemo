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

    @Transactional
    public int load(Dataset dataset, Path file, RecordEncoding encoding) {
        return load(dataset, dataset.read(file, encoding));
    }

    /**
     * Replaces the table's contents with the dataset image (IDCAMS DELETE/DEFINE + REPRO): existing rows are deleted,
     * then one row is inserted per record; DALYTRAN rows are numbered 1..n in file order. The deferred card_xref and
     * transaction_category FKs are checked at commit, so parents can be reloaded in the same transaction.
     * Returns the record count.
     */
    @Transactional
    public int load(Dataset dataset, List<FixedWidthRecord> records) {
        repository(dataset).deleteAllInBatch();
        entityManager.flush();
        entityManager.clear();
        switch (dataset) {
            case USRSEC -> users.saveAll(map(records, UserSecurityRecord.MAPPER).stream().map(UserSecurity::from)
                    .toList());
            case CUSTDATA -> customers.saveAll(map(records, CustomerRecord.MAPPER).stream().map(Customer::from)
                    .toList());
            case ACCTDATA -> accounts.saveAll(map(records, AccountRecord.MAPPER).stream().map(Account::from).toList());
            case CARDDATA -> cards.saveAll(map(records, CardRecord.MAPPER).stream().map(Card::from).toList());
            case CARDXREF -> xrefs.saveAll(map(records, CardXrefRecord.MAPPER).stream().map(CardXref::from).toList());
            case TRANSACT -> transactions.saveAll(map(records, TransactionRecord.MAPPER).stream()
                    .map(Transaction::from).toList());
            case DALYTRAN -> {
                List<DailyTransactionRecord> daily = map(records, DailyTransactionRecord.MAPPER);
                List<DailyTransaction> rows = new ArrayList<>(daily.size());
                for (int i = 0; i < daily.size(); i++) {
                    rows.add(DailyTransaction.from(i + 1, daily.get(i)));
                }
                dailyTransactions.saveAll(rows);
            }
            case TRANTYPE -> types.saveAll(map(records, TransactionTypeRecord.MAPPER).stream()
                    .map(TransactionType::from).toList());
            case TRANCATG -> categories.saveAll(map(records, TransactionCategoryRecord.MAPPER).stream()
                    .map(TransactionCategory::from).toList());
            case DISCGRP -> disclosureGroups.saveAll(map(records, DisclosureGroupRecord.MAPPER).stream()
                    .map(DisclosureGroup::from).toList());
            case TCATBALF -> balances.saveAll(map(records, TranCatBalanceRecord.MAPPER).stream()
                    .map(TranCatBalance::from).toList());
        }
        return records.size();
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

    private static <D extends Record> List<D> map(List<FixedWidthRecord> records, CopybookRecordMapper<D> mapper) {
        return mapper.fromRecords(records);
    }
}
