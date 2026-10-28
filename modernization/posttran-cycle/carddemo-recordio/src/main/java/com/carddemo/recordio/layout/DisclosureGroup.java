package com.carddemo.recordio.layout;

import java.math.BigDecimal;

/**
 * {@code app/cpy/CVTRA02Y.cpy DIS-GROUP-RECORD}, RECLN 50. Key = account group + type + category
 * (16 bytes, see {@code app/jcl/DISCGRP.jcl KEYS(16 0)}). Rate is {@code S9(04)V99}, a percentage.
 */
public record DisclosureGroup(String accountGroupId, String typeCode, int categoryCode, BigDecimal interestRate) {

    public static final int LENGTH = 50;
    public static final String DEFAULT_GROUP_ID = "DEFAULT";

    /** 16-byte KSDS key: group id padded to X(10) + type X(02) + category 9(04). */
    public static String key(String accountGroupId, String typeCode, int categoryCode) {
        return String.format("%-10s%s%04d", accountGroupId, typeCode, categoryCode);
    }

    public String key() {
        return key(accountGroupId, typeCode, categoryCode);
    }
}
