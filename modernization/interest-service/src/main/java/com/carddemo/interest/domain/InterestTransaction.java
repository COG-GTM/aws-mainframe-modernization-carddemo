package com.carddemo.interest.domain;

import java.math.BigDecimal;

/**
 * A system-generated interest transaction ({@code CVTRA05Y},
 * {@code app/cpy/CVTRA05Y.cpy:4-18}) written to the {@code TRANSACT} output file.
 *
 * <p>Built by {@code 1300-B-WRITE-TX} ({@code app/cbl/CBACT04C.cbl:473-500}); the constant
 * type/category/source/merchant values below are literals in that paragraph.
 */
public record InterestTransaction(String transactionId,
                                  String typeCode,
                                  int categoryCode,
                                  String source,
                                  String description,
                                  BigDecimal amount,
                                  String merchantId,
                                  String merchantName,
                                  String merchantCity,
                                  String merchantZip,
                                  String cardNumber,
                                  String originTimestamp,
                                  String processTimestamp) {

    /** {@code MOVE '01' TO TRAN-TYPE-CD} ({@code app/cbl/CBACT04C.cbl:482}). */
    public static final String INTEREST_TYPE_CODE = "01";

    /** {@code MOVE '05' TO TRAN-CAT-CD} ({@code app/cbl/CBACT04C.cbl:483}). */
    public static final int INTEREST_CATEGORY_CODE = 5;

    /** {@code MOVE 'System' TO TRAN-SOURCE} ({@code app/cbl/CBACT04C.cbl:484}). */
    public static final String SYSTEM_SOURCE = "System";
}
