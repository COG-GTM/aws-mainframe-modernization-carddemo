package com.carddemo.recordio.layout;

/** {@code app/cpy/CVTRA03Y.cpy TRAN-TYPE-RECORD}, RECLN 60. */
public record TransactionType(String typeCode, String description) {

    public static final int LENGTH = 60;
}
