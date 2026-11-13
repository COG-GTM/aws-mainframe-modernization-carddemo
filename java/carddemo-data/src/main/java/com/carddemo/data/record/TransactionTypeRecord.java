package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

/** Fixed-width TRAN-TYPE-RECORD. Layout: copybook CVTRA03Y. */
public record TransactionTypeRecord(
        @CobolField("TRAN-TYPE") String typeCode,
        @CobolField("TRAN-TYPE-DESC") String description) {
}
