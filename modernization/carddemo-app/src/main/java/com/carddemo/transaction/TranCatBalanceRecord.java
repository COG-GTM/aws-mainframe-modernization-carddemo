package com.carddemo.transaction;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;
import java.math.BigDecimal;

/**
 * One TCATBALF record (copybook CVTRA01Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link TranCatBalance} persists it.
 *
 * @param acctId TRANCAT-ACCT-ID PIC 9(11)
 * @param tranTypeCd TRANCAT-TYPE-CD PIC X(02)
 * @param tranCatCd TRANCAT-CD PIC 9(04)
 * @param balance TRAN-CAT-BAL PIC S9(09)V99
 */
public record TranCatBalanceRecord(
        @CobolField("TRANCAT-ACCT-ID") long acctId,
        @CobolField("TRANCAT-TYPE-CD") String tranTypeCd,
        @CobolField("TRANCAT-CD") int tranCatCd,
        @CobolField("TRAN-CAT-BAL") BigDecimal balance) {

    public static final CopybookRecordMapper<TranCatBalanceRecord> MAPPER =
            CopybookRecordMapper.of(TranCatBalanceRecord.class, "CVTRA01Y");
}
