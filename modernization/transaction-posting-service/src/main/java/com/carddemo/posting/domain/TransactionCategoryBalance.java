package com.carddemo.posting.domain;

import com.carddemo.mainframe.io.CobolFixedPoint;

import java.math.BigDecimal;

/**
 * A transaction-category balance ({@code CVTRA01Y}, {@code app/cpy/CVTRA01Y.cpy:4-10}): the
 * running total of everything posted to one account for one transaction type and category.
 *
 * <p>{@code TRAN-CAT-BAL} is {@code PIC S9(09)V99}, two digits narrower than the account balance,
 * which is why accumulation has its own capacity.
 */
public record TransactionCategoryBalance(TransactionCategoryKey key, BigDecimal balance) {

    /** Integer digits of {@code TRAN-CAT-BAL PIC S9(09)V99}. */
    private static final int INTEGER_DIGITS = 9;
    private static final int SCALE = 2;

    /** A newly created bucket, opened at zero. Business rule BR-8. */
    public static TransactionCategoryBalance opened(TransactionCategoryKey key) {
        return new TransactionCategoryBalance(key, BigDecimal.ZERO.setScale(SCALE));
    }

    /**
     * Accumulates a posted amount into the bucket.
     *
     * <p>Business rule BR-8/BR-9, COBOL {@code ADD DALYTRAN-AMT TO TRAN-CAT-BAL}
     * ({@code app/cbl/CBTRN02C.cbl:508} on creation, {@code app/cbl/CBTRN02C.cbl:527} on update).
     * The unguarded {@code ADD} truncates into {@code PIC S9(09)V99}, which
     * {@link CobolFixedPoint#fit} reproduces.
     */
    public TransactionCategoryBalance withAmountAdded(BigDecimal amount) {
        return new TransactionCategoryBalance(key,
                CobolFixedPoint.fit(balance.add(amount), INTEGER_DIGITS, SCALE));
    }
}
