package com.carddemo.posting.domain;

/**
 * The composite key of the transaction-category-balance file
 * ({@code TRAN-CAT-KEY}, {@code app/cpy/CVTRA01Y.cpy:5-8}): account, transaction type and
 * transaction category.
 *
 * <p>Built at {@code app/cbl/CBTRN02C.cbl:469-471} from the cross-referenced account id and the
 * type and category codes carried on the daily transaction, i.e. a transaction is accumulated
 * into the bucket for its own type/category, not the account as a whole.
 */
public record TransactionCategoryKey(AccountId accountId, String typeCode, String categoryCode)
        implements Comparable<TransactionCategoryKey> {

    public TransactionCategoryKey {
        if (typeCode == null || typeCode.length() != 2) {
            throw new IllegalArgumentException("Transaction type code is PIC X(02): " + typeCode);
        }
        if (categoryCode == null || categoryCode.length() != 4) {
            throw new IllegalArgumentException("Transaction category code is PIC 9(04): " + categoryCode);
        }
    }

    /** The 17-character key as the VSAM KSDS stores it, used for key-order output. */
    public String keyText() {
        return accountId.value() + typeCode + categoryCode;
    }

    @Override
    public int compareTo(TransactionCategoryKey other) {
        return keyText().compareTo(other.keyText());
    }
}
