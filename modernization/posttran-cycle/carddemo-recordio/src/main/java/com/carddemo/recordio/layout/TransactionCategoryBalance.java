package com.carddemo.recordio.layout;

import java.math.BigDecimal;

/**
 * {@code app/cpy/CVTRA01Y.cpy TRAN-CAT-BAL-RECORD}, RECLN 50. Key = account + type + category
 * (17 bytes, see {@code app/jcl/TCATBALF.jcl KEYS(17 0)}).
 */
public record TransactionCategoryBalance(String accountId, String typeCode, int categoryCode, BigDecimal balance) {

    public static final int LENGTH = 50;
    public static final int BALANCE_INT_DIGITS = 9;
    public static final int BALANCE_SCALE = 2;

    public TransactionCategoryBalance withBalance(BigDecimal newBalance) {
        return new TransactionCategoryBalance(accountId, typeCode, categoryCode, newBalance);
    }

    public String key() {
        return accountId + typeCode + String.format("%04d", categoryCode);
    }
}
