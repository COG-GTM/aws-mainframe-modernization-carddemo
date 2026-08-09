package com.carddemo.interest.domain;

import java.math.BigDecimal;

/**
 * The balance an account carries in one transaction category — the unit of work of the interest
 * batch.
 *
 * <p>Source copybook {@code CVTRA01Y} ({@code app/cpy/CVTRA01Y.cpy:4-10}), read sequentially in
 * key order by paragraph {@code 1000-TCATBALF-GET-NEXT} ({@code app/cbl/CBACT04C.cbl:325-348}).
 *
 * @param accountId {@code TRANCAT-ACCT-ID PIC 9(11)}
 * @param category  {@code TRANCAT-TYPE-CD PIC X(02)} + {@code TRANCAT-CD PIC 9(04)}
 * @param balance   {@code TRAN-CAT-BAL PIC S9(09)V99}
 */
public record TransactionCategoryBalance(AccountId accountId, TransactionCategory category, BigDecimal balance) {
}
