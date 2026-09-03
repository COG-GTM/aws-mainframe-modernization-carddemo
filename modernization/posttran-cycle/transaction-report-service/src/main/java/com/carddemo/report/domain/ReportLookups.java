package com.carddemo.report.domain;

import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.TransactionCategory;
import com.carddemo.recordio.layout.TransactionType;
import com.carddemo.recordio.store.KeyedRecordStore;
import com.carddemo.recordio.store.RecordNotFoundException;

/**
 * CBTRN03C 1500-A/B/C lookups (lines 484-513). Each miss is fatal: the COBOL displays the key,
 * sets IO-STATUS 23 and abends. There is no reject path in the report program.
 */
public final class ReportLookups {

    private final KeyedRecordStore<CardXref> cardXref;
    private final KeyedRecordStore<TransactionType> types;
    private final KeyedRecordStore<TransactionCategory> categories;

    public ReportLookups(KeyedRecordStore<CardXref> cardXref, KeyedRecordStore<TransactionType> types,
                         KeyedRecordStore<TransactionCategory> categories) {
        this.cardXref = cardXref;
        this.types = types;
        this.categories = categories;
    }

    public CardXref card(String cardNumber) {
        return cardXref.read(cardNumber).orElseThrow(() ->
                new RecordNotFoundException("CARDXREF", cardNumber, "INVALID CARD NUMBER : " + cardNumber));
    }

    public TransactionType type(String typeCode) {
        return types.read(typeCode).orElseThrow(() ->
                new RecordNotFoundException("TRANTYPE", typeCode, "INVALID TRANSACTION TYPE : " + typeCode));
    }

    public TransactionCategory category(String typeCode, int categoryCode) {
        String key = typeCode + String.format("%04d", categoryCode);
        return categories.read(key).orElseThrow(() ->
                new RecordNotFoundException("TRANCATG", key, "INVALID TRAN CATG KEY : " + key));
    }
}
