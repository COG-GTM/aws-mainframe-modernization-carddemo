package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.TranRecord;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

final class Fixtures {

    static final Clock CLOCK = Clock.fixed(LocalDateTime.of(2026, 3, 7, 9, 5, 4).toInstant(ZoneOffset.UTC),
            ZoneOffset.UTC);

    private Fixtures() {
    }

    /** app/data/ASCII/dailytran.txt (same 350-byte layout as TRAN-RECORD). */
    static List<TranRecord> dailyTran() {
        try (var in = Objects.requireNonNull(Fixtures.class.getResourceAsStream("/fixtures/dailytran.txt"))) {
            return TranRecordReader.readAll(new InputStreamReader(in, StandardCharsets.US_ASCII));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    static String id(int n) {
        return String.format("%016d", n);
    }

    static TranRecord tran(int n, String amount) {
        return new TranRecord(id(n), "01", 1, "POS TERM", "Desc " + n, new BigDecimal(amount), 800000000L,
                "Merchant", "City", "12345", "4859452612877065", "2022-06-10 19:27:53.000000",
                "2022-06-11 10:00:00.000000");
    }

    /** Records with ids 1..count. */
    static List<TranRecord> trans(int count) {
        List<TranRecord> out = new ArrayList<>();
        for (int i = 1; i <= count; i++) {
            out.add(tran(i, i + ".00"));
        }
        return out;
    }
}
