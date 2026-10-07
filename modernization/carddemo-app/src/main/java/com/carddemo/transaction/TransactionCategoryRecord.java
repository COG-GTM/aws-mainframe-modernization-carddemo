package com.carddemo.transaction;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;

/**
 * One TRANCATG record (copybook CVTRA04Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link TransactionCategory} persists it.
 *
 * @param tranTypeCd TRAN-TYPE-CD PIC X(02)
 * @param tranCatCd TRAN-CAT-CD PIC 9(04)
 * @param description TRAN-CAT-TYPE-DESC PIC X(50)
 */
public record TransactionCategoryRecord(
        @CobolField("TRAN-TYPE-CD") String tranTypeCd,
        @CobolField("TRAN-CAT-CD") int tranCatCd,
        @CobolField("TRAN-CAT-TYPE-DESC") String description) {

    public static final CopybookRecordMapper<TransactionCategoryRecord> MAPPER =
            CopybookRecordMapper.of(TransactionCategoryRecord.class, "CVTRA04Y");
}
