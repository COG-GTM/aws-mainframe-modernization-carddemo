package com.carddemo.data.load;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.codec.RecordCodec;
import com.carddemo.common.io.RecordFileReader;
import com.carddemo.common.io.RecordFormat;
import com.carddemo.data.catalog.CardDemoFile;
import com.carddemo.data.mapper.AccountMapper;
import com.carddemo.data.mapper.CardCrossReferenceMapper;
import com.carddemo.data.mapper.CardMapper;
import com.carddemo.data.mapper.CustomerMapper;
import com.carddemo.data.mapper.DailyTransactionMapper;
import com.carddemo.data.mapper.DisclosureGroupMapper;
import com.carddemo.data.mapper.TransactionCategoryBalanceMapper;
import com.carddemo.data.mapper.TransactionCategoryMapper;
import com.carddemo.data.mapper.TransactionMapper;
import com.carddemo.data.mapper.TransactionTypeMapper;
import com.carddemo.data.mapper.UserSecurityMapper;
import com.carddemo.data.repository.AccountRepository;
import com.carddemo.data.repository.CardCrossReferenceRepository;
import com.carddemo.data.repository.CardRepository;
import com.carddemo.data.repository.CustomerRepository;
import com.carddemo.data.repository.DailyTransactionRepository;
import com.carddemo.data.repository.DisclosureGroupRepository;
import com.carddemo.data.repository.TransactionCategoryBalanceRepository;
import com.carddemo.data.repository.TransactionCategoryRepository;
import com.carddemo.data.repository.TransactionRepository;
import com.carddemo.data.repository.TransactionTypeRepository;
import com.carddemo.data.repository.UserSecurityRepository;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * Loads mainframe datasets into the relational model and unloads them back to fixed-width records: the
 * equivalent of the IDCAMS REPRO steps of ACCTFILE, CARDFILE, CUSTFILE, XREFFILE, TRANFILE, DISCGRP, TCATBALF,
 * TRANCATG, TRANTYPE and DUSRSECJ. Data migration tooling only; no business rules.
 */
@Component
public class CardDemoDataImporter {

    private record Handler<R extends Record, E>(Function<R, E> toEntity, Function<E, R> toRecord,
                                                 JpaRepository<E, ?> repository) {
    }

    private final Map<CardDemoFile, Handler<?, ?>> handlers = new EnumMap<>(CardDemoFile.class);

    public CardDemoDataImporter(UserSecurityMapper userMapper, UserSecurityRepository users,
                                AccountMapper accountMapper, AccountRepository accounts,
                                CardMapper cardMapper, CardRepository cards,
                                CustomerMapper customerMapper, CustomerRepository customers,
                                CardCrossReferenceMapper xrefMapper, CardCrossReferenceRepository xrefs,
                                TransactionMapper transactionMapper, TransactionRepository transactions,
                                DailyTransactionMapper dailyMapper, DailyTransactionRepository dailyTransactions,
                                TransactionCategoryBalanceMapper balanceMapper, TransactionCategoryBalanceRepository balances,
                                DisclosureGroupMapper groupMapper, DisclosureGroupRepository groups,
                                TransactionTypeMapper typeMapper, TransactionTypeRepository types,
                                TransactionCategoryMapper categoryMapper, TransactionCategoryRepository categories) {
        handlers.put(CardDemoFile.USER_SECURITY, new Handler<>(userMapper::toEntity, userMapper::toRecord, users));
        handlers.put(CardDemoFile.ACCOUNT, new Handler<>(accountMapper::toEntity, accountMapper::toRecord, accounts));
        handlers.put(CardDemoFile.CARD, new Handler<>(cardMapper::toEntity, cardMapper::toRecord, cards));
        handlers.put(CardDemoFile.CUSTOMER, new Handler<>(customerMapper::toEntity, customerMapper::toRecord, customers));
        handlers.put(CardDemoFile.CARD_XREF, new Handler<>(xrefMapper::toEntity, xrefMapper::toRecord, xrefs));
        handlers.put(CardDemoFile.TRANSACTION,
                new Handler<>(transactionMapper::toEntity, transactionMapper::toRecord, transactions));
        handlers.put(CardDemoFile.DAILY_TRANSACTION,
                new Handler<>(dailyMapper::toEntity, dailyMapper::toRecord, dailyTransactions));
        handlers.put(CardDemoFile.TRAN_CAT_BALANCE, new Handler<>(balanceMapper::toEntity, balanceMapper::toRecord, balances));
        handlers.put(CardDemoFile.DISCLOSURE_GROUP, new Handler<>(groupMapper::toEntity, groupMapper::toRecord, groups));
        handlers.put(CardDemoFile.TRANSACTION_TYPE, new Handler<>(typeMapper::toEntity, typeMapper::toRecord, types));
        handlers.put(CardDemoFile.TRANSACTION_CATEGORY,
                new Handler<>(categoryMapper::toEntity, categoryMapper::toRecord, categories));
    }

    /** Loads every record of a dataset (REPRO INFILE -> OUTFILE). Returns the number of records loaded. */
    @Transactional
    public int load(CardDemoFile file, Path dataset, RecordFormat format, DataEncoding encoding) {
        return loadInternal(file, RecordFileReader.readAll(dataset, format, file.lrecl(), encoding), encoding);
    }

    @Transactional
    public int load(CardDemoFile file, List<byte[]> records, DataEncoding encoding) {
        return loadInternal(file, records, encoding);
    }

    /** Unloads a table to fixed-width records in VSAM key order (EBCDIC collation when unloading to EBCDIC). */
    @Transactional(readOnly = true)
    public List<byte[]> unload(CardDemoFile file, DataEncoding encoding) {
        List<byte[]> out = unloadInternal(file, encoding);
        out.sort((a, b) -> Arrays.compareUnsigned(a, 0, file.keyLength(), b, 0, file.keyLength()));
        return out;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private int loadInternal(CardDemoFile file, List<byte[]> records, DataEncoding encoding) {
        Handler handler = handlers.get(file);
        RecordCodec codec = file.codec(encoding);
        List<Object> entities = new ArrayList<>(records.size());
        for (byte[] record : records) {
            entities.add(handler.toEntity().apply(codec.decode(record)));
        }
        handler.repository().saveAll(entities);
        return entities.size();
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private List<byte[]> unloadInternal(CardDemoFile file, DataEncoding encoding) {
        Handler handler = handlers.get(file);
        RecordCodec codec = file.codec(encoding);
        List<byte[]> out = new ArrayList<>();
        for (Object entity : handler.repository().findAll()) {
            out.add(codec.encode((Record) handler.toRecord().apply(entity)));
        }
        return out;
    }
}
