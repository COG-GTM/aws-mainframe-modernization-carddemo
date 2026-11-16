package com.carddemo.legacyio;

import com.carddemo.legacyio.copybook.Copybook;
import java.util.EnumMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/** The CardDemo data files and the copybook record each one holds. */
public enum CardDemoFile {
    /** ACCTDATA (VSAM KSDS ACCTDAT), key ACCT-ID. */
    ACCOUNT("CVACT01Y", 300),
    /** CARDDATA (CARDDAT), key CARD-NUM. */
    CARD("CVACT02Y", 150),
    /** CARDXREF (CCXREF), key XREF-CARD-NUM, AIX on XREF-ACCT-ID. */
    CARD_XREF("CVACT03Y", 50),
    /** CUSTDATA (CUSTDAT), key CUST-ID. */
    CUSTOMER("CVCUS01Y", 500),
    /** DALYTRAN: the daily transactions POSTTRAN posts. */
    DAILY_TRANSACTION("CVTRA06Y", 350),
    /** TRANSACT: posted transactions, key TRAN-ID. */
    TRANSACTION("CVTRA05Y", 350),
    /** DISCGRP: interest rate per disclosure group, key DIS-GROUP-KEY. */
    DISCLOSURE_GROUP("CVTRA02Y", 50),
    /** TCATBALF: balance per account and transaction category, key TRAN-CAT-KEY. */
    TRAN_CAT_BALANCE("CVTRA01Y", 50),
    /** TRANCATG: transaction categories, key TRAN-CAT-KEY. */
    TRAN_CATEGORY("CVTRA04Y", 60),
    /** TRANTYPE: transaction types, key TRAN-TYPE. */
    TRAN_TYPE("CVTRA03Y", 60),
    /** USRSEC: sign-on users, key SEC-USR-ID. */
    USER_SECURITY("CSUSR01Y", 80),
    /**
     * EXPORT: the branch-migration export written by CBEXPORT; EXPORT-RECORD-DATA is redefined per
     * EXPORT-REC-TYPE (C customer, A account, X card xref, T transaction, D card).
     */
    EXPORT("CVEXPORT", 500);

    private static final Map<Character, String> EXPORT_VARIANTS = Map.of(
            'C', "EXPORT-CUSTOMER-DATA",
            'A', "EXPORT-ACCOUNT-DATA",
            'X', "EXPORT-CARD-XREF-DATA",
            'T', "EXPORT-TRANSACTION-DATA",
            'D', "EXPORT-CARD-DATA");

    private static final Map<CardDemoFile, RecordLayout> LAYOUTS = new EnumMap<>(CardDemoFile.class);
    private static final Map<String, RecordLayout> VARIANTS = new ConcurrentHashMap<>();
    private static final Map<CodecKey, RecordCodec> CODECS = new ConcurrentHashMap<>();

    private final String copybook;
    private final int recordLength;

    CardDemoFile(String copybook, int recordLength) {
        this.copybook = copybook;
        this.recordLength = recordLength;
    }

    /** Copybook member in {@code app/cpy}. */
    public String copybook() {
        return copybook;
    }

    /** LRECL of the dataset. */
    public int recordLength() {
        return recordLength;
    }

    /** The record layout with no {@code REDEFINES} alternative selected. */
    public RecordLayout layout() {
        synchronized (LAYOUTS) {
            return LAYOUTS.computeIfAbsent(this, f -> checked(RecordLayout.of(Copybook.load(f.copybook))));
        }
    }

    /** The layout with one {@code REDEFINES} alternative selected, e.g. {@code EXPORT-ACCOUNT-DATA}. */
    public RecordLayout layout(String redefinition) {
        return VARIANTS.computeIfAbsent(name() + "/" + redefinition,
                k -> checked(RecordLayout.of(Copybook.load(copybook), redefinition)));
    }

    /** The layout that applies to a record's bytes; for {@link #EXPORT} it depends on EXPORT-REC-TYPE. */
    public RecordLayout layoutFor(byte[] record, Encoding encoding) {
        if (this != EXPORT) {
            return layout();
        }
        char type = encoding.decode(record, 0, 1).charAt(0);
        String variant = EXPORT_VARIANTS.get(type);
        return variant == null ? layout() : layout(variant);
    }

    RecordCodec codec(byte[] record, Encoding encoding) {
        RecordLayout layout = layoutFor(record, encoding);
        return CODECS.computeIfAbsent(new CodecKey(layout, encoding), k -> new RecordCodec(layout, encoding));
    }

    private RecordLayout checked(RecordLayout layout) {
        if (layout.length() != recordLength) {
            throw new IllegalStateException(copybook + " describes " + layout.length() + "-byte records; "
                    + name() + " has LRECL " + recordLength);
        }
        return layout;
    }

    private record CodecKey(RecordLayout layout, Encoding encoding) {
    }
}
