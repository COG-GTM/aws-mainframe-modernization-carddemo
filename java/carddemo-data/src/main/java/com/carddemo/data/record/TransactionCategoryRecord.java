package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

/** Fixed-width TRAN-CAT-RECORD. Layout: copybook CVTRA04Y. */
public record TransactionCategoryRecord(
        @CobolField("TRAN-CAT-KEY") Key id,
        @CobolField("TRAN-CAT-TYPE-DESC") String description) {

    /** TRAN-CAT-KEY. */
    public record Key(
            @CobolField("TRAN-TYPE-CD") String typeCode,
            @CobolField("TRAN-CAT-CD") Integer categoryCode) {
    }
}
