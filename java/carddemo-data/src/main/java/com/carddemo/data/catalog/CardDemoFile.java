package com.carddemo.data.catalog;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.codec.RecordCodec;
import com.carddemo.common.copybook.RecordLayout;
import com.carddemo.data.record.AccountRecord;
import com.carddemo.data.record.CardRecord;
import com.carddemo.data.record.CardXrefRecord;
import com.carddemo.data.record.CustomerRecord;
import com.carddemo.data.record.DailyTransactionRecord;
import com.carddemo.data.record.DisclosureGroupRecord;
import com.carddemo.data.record.TranCatBalanceRecord;
import com.carddemo.data.record.TransactionCategoryRecord;
import com.carddemo.data.record.TransactionRecord;
import com.carddemo.data.record.TransactionTypeRecord;
import com.carddemo.data.record.UserSecurityRecord;

/**
 * The core CardDemo files: CICS file / DD name, copybook, LRECL, primary key length (keys start at offset 0)
 * and the Java record type that mirrors the copybook.
 */
public enum CardDemoFile {
    USER_SECURITY("USRSEC", "CSUSR01Y", 80, 8, UserSecurityRecord.class),
    ACCOUNT("ACCTDAT", "CVACT01Y", 300, 11, AccountRecord.class),
    CARD("CARDDAT", "CVACT02Y", 150, 16, CardRecord.class),
    CUSTOMER("CUSTDAT", "CVCUS01Y", 500, 9, CustomerRecord.class),
    CARD_XREF("CCXREF", "CVACT03Y", 50, 16, CardXrefRecord.class),
    TRANSACTION("TRANSACT", "CVTRA05Y", 350, 16, TransactionRecord.class),
    DAILY_TRANSACTION("DALYTRAN", "CVTRA06Y", 350, 16, DailyTransactionRecord.class),
    TRAN_CAT_BALANCE("TCATBALF", "CVTRA01Y", 50, 17, TranCatBalanceRecord.class),
    DISCLOSURE_GROUP("DISCGRP", "CVTRA02Y", 50, 16, DisclosureGroupRecord.class),
    TRANSACTION_TYPE("TRANTYPE", "CVTRA03Y", 60, 2, TransactionTypeRecord.class),
    TRANSACTION_CATEGORY("TRANCATG", "CVTRA04Y", 60, 6, TransactionCategoryRecord.class);

    private final String ddName;
    private final String copybook;
    private final int lrecl;
    private final int keyLength;
    private final Class<? extends Record> recordType;

    CardDemoFile(String ddName, String copybook, int lrecl, int keyLength, Class<? extends Record> recordType) {
        this.ddName = ddName;
        this.copybook = copybook;
        this.lrecl = lrecl;
        this.keyLength = keyLength;
        this.recordType = recordType;
    }

    public String ddName() {
        return ddName;
    }

    public String copybook() {
        return copybook;
    }

    public int lrecl() {
        return lrecl;
    }

    public int keyLength() {
        return keyLength;
    }

    public Class<? extends Record> recordType() {
        return recordType;
    }

    public RecordLayout layout() {
        return CopybookCatalog.layout(copybook);
    }

    @SuppressWarnings("unchecked")
    public <R extends Record> RecordCodec<R> codec(DataEncoding encoding) {
        return RecordCodec.of((Class<R>) recordType, layout(), encoding);
    }
}
