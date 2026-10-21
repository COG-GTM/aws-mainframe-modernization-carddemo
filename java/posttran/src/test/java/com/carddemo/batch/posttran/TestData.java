package com.carddemo.batch.posttran;

import com.carddemo.batch.posttran.cobol.Pic;
import com.carddemo.batch.posttran.cobol.ZonedDecimal;
import com.carddemo.batch.posttran.record.DailyTransaction;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

public final class TestData {

    public static final Clock FIXED_CLOCK = Clock.fixed(Instant.parse("2026-10-14T09:08:07.123Z"), ZoneOffset.UTC);

    private TestData() {
    }

    public static Path dataDir() {
        return Path.of(System.getProperty("carddemo.app.dir", "../../app")).resolve("data");
    }

    /** A DALYTRAN record laid out per CVTRA06Y. */
    public static DailyTransaction dalytran(String id, String typeCd, int catCd, String amount, String cardNum,
            String origTs) {
        String r = Pic.x(id, 16)
                + Pic.x(typeCd, 2)
                + ZonedDecimal.encodeUnsigned(catCd, 4)
                + Pic.x("POS TERM", 10)
                + Pic.x("Purchase at Test Store", 100)
                + ZonedDecimal.encodeSigned(new BigDecimal(amount), 9, 2)
                + ZonedDecimal.encodeUnsigned(800000001, 9)
                + Pic.x("Test Store", 50)
                + Pic.x("Seattle", 50)
                + Pic.x("98101", 10)
                + Pic.x(cardNum, 16)
                + Pic.x(origTs, 26)
                + Pic.spaces(26)
                + Pic.spaces(20);
        return DailyTransaction.parse(r);
    }
}
