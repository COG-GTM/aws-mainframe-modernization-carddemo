package com.carddemo.transaction;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;

/**
 * One TRANTYPE record (copybook CVTRA03Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link TransactionType} persists it.
 *
 * @param tranTypeCd TRAN-TYPE PIC X(02)
 * @param description TRAN-TYPE-DESC PIC X(50)
 */
public record TransactionTypeRecord(
        @CobolField("TRAN-TYPE") String tranTypeCd,
        @CobolField("TRAN-TYPE-DESC") String description) {

    public static final CopybookRecordMapper<TransactionTypeRecord> MAPPER =
            CopybookRecordMapper.of(TransactionTypeRecord.class, "CVTRA03Y");
}
