package com.carddemo.batch.posttran.record;

import com.carddemo.batch.posttran.cobol.ZonedDecimal;
import java.math.BigDecimal;

/**
 * DALYTRAN-RECORD (copybook CVTRA06Y, 350 bytes). {@code raw} is the record exactly as read; it is
 * what 2500-WRITE-REJECT-REC copies into the reject file.
 */
public record DailyTransaction(
        String raw,
        String id,
        String typeCd,
        int catCd,
        String source,
        String desc,
        BigDecimal amt,
        long merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNum,
        String origTs,
        String procTs) {

    public static final int LENGTH = 350;

    public static DailyTransaction parse(String r) {
        if (r.length() != LENGTH) {
            throw new IllegalArgumentException("DALYTRAN record must be 350 bytes, got " + r.length());
        }
        return new DailyTransaction(
                r,
                r.substring(0, 16),
                r.substring(16, 18),
                (int) ZonedDecimal.decodeUnsigned(r.substring(18, 22)),
                r.substring(22, 32),
                r.substring(32, 132),
                ZonedDecimal.decodeSigned(r.substring(132, 143), 2),
                ZonedDecimal.decodeUnsigned(r.substring(143, 152)),
                r.substring(152, 202),
                r.substring(202, 252),
                r.substring(252, 262),
                r.substring(262, 278),
                r.substring(278, 304),
                r.substring(304, 330));
    }
}
