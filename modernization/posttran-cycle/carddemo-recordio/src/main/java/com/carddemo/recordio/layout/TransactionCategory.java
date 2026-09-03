package com.carddemo.recordio.layout;

/** {@code app/cpy/CVTRA04Y.cpy TRAN-CAT-RECORD}, RECLN 60. Key = type + category (6 bytes). */
public record TransactionCategory(String typeCode, int categoryCode, String description) {

    public static final int LENGTH = 60;

    public String key() {
        return typeCode + String.format("%04d", categoryCode);
    }
}
