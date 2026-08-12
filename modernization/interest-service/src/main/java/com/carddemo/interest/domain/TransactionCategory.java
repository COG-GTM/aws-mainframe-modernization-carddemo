package com.carddemo.interest.domain;

/**
 * A transaction type/category pair — the dimension on which interest rates are disclosed.
 *
 * <p>Type code is {@code PIC X(02)} and category code {@code PIC 9(04)}; both appear in the
 * transaction-category-balance key ({@code app/cpy/CVTRA01Y.cpy:6-8}) and in the disclosure group
 * key ({@code app/cpy/CVTRA02Y.cpy:6-8}).
 */
public record TransactionCategory(String typeCode, int categoryCode) {

    public TransactionCategory {
        if (typeCode == null || typeCode.isBlank()) {
            throw new IllegalArgumentException("Transaction type code must not be blank");
        }
        typeCode = typeCode.trim();
    }

    public static TransactionCategory of(String typeCode, int categoryCode) {
        return new TransactionCategory(typeCode, categoryCode);
    }

    @Override
    public String toString() {
        return typeCode + "/" + "%04d".formatted(categoryCode);
    }
}
