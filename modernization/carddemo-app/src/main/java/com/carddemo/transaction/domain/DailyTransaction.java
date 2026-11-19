package com.carddemo.transaction.domain;

import com.carddemo.common.record.FixedRecord;

import java.math.BigDecimal;

import static com.carddemo.common.record.Copybooks.CVTRA06Y;

/**
 * One DALYTRAN record ({@code app/cpy/CVTRA06Y.cpy}). {@code record} keeps the 350 characters verbatim because a reject
 * copies DALYTRAN-RECORD unchanged into DALYREJS ({@code CBTRN02C.cbl} 2500-WRITE-REJECT-REC).
 */
public record DailyTransaction(String record) {

    public DailyTransaction {
        CVTRA06Y.parse(record);
    }

    private FixedRecord fields() {
        return CVTRA06Y.parse(record);
    }

    public String id() {
        return fields().raw("DALYTRAN-ID");
    }

    public String typeCd() {
        return fields().raw("DALYTRAN-TYPE-CD");
    }

    public int catCd() {
        return (int) fields().number("DALYTRAN-CAT-CD");
    }

    public String source() {
        return fields().raw("DALYTRAN-SOURCE");
    }

    public String desc() {
        return fields().raw("DALYTRAN-DESC");
    }

    public BigDecimal amount() {
        return fields().decimal("DALYTRAN-AMT");
    }

    public long merchantId() {
        return fields().number("DALYTRAN-MERCHANT-ID");
    }

    public String merchantName() {
        return fields().raw("DALYTRAN-MERCHANT-NAME");
    }

    public String merchantCity() {
        return fields().raw("DALYTRAN-MERCHANT-CITY");
    }

    public String merchantZip() {
        return fields().raw("DALYTRAN-MERCHANT-ZIP");
    }

    public String cardNum() {
        return fields().raw("DALYTRAN-CARD-NUM");
    }

    public String origTs() {
        return fields().raw("DALYTRAN-ORIG-TS");
    }

    public String procTs() {
        return fields().raw("DALYTRAN-PROC-TS");
    }
}
