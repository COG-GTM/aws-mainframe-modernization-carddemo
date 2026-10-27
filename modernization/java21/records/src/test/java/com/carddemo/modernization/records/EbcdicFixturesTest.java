package com.carddemo.modernization.records;

import com.carddemo.modernization.records.codec.RecordCodec;
import com.carddemo.modernization.records.codec.TextCodec;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import static org.junit.jupiter.api.Assertions.*;

class EbcdicFixturesTest {
    private record Difference(String key, String field, String ascii, String ebcdic) {}

    @ParameterizedTest
    @ValueSource(strings = {"ACCTDATA", "ACCDATA", "CARDXREF", "TCATBALF", "DISCGRP", "DALYTRAN", "TRANSACTION"})
    void shippedRecords(String dataset) throws Exception {
        switch (dataset) {
            case "ACCTDATA", "ACCDATA" -> compare(dataset, "acctdata", AccountRecord.CODEC,
                    AccountRecord::acctId, List.of(new Difference("00000000049", "addressZip",
                            "A000000000", "ZEROAPR   ")));
            case "CARDXREF" -> compare(dataset, "cardxref", CardXrefRecord.CODEC,
                    CardXrefRecord::cardNumber, List.of());
            case "TCATBALF" -> compare(dataset, "tcatbal", TranCatBalanceRecord.CODEC,
                    r -> r.accountId() + r.typeCode() + r.categoryCode(), List.of());
            case "DISCGRP" -> compare(dataset, "discgrp", DisclosureGroupRecord.CODEC,
                    r -> r.groupId() + r.typeCode() + r.categoryCode(),
                    List.of(new Difference("DEFAULT   070001", "interestRate", "0.00", "15.00")));
            case "DALYTRAN" -> compare(dataset, "dailytran", DailyTransactionRecord.CODEC,
                    DailyTransactionRecord::id, List.of());
            case "TRANSACTION" -> compare("DALYTRAN", "dailytran", TransactionRecord.CODEC,
                    TransactionRecord::id, List.of());
            default -> throw new IllegalArgumentException(dataset);
        }
    }

    private static <T extends Record> void compare(String dataset, String asciiFile, RecordCodec<T> codec,
            Function<T, String> key, List<Difference> expected) throws Exception {
        Path data = Path.of("../../../app/data");
        byte[] shipped = Files.readAllBytes(data.resolve("EBCDIC/AWS.M2.CARDDEMO." + dataset + ".PS"));
        List<String> ascii = Files.readAllLines(data.resolve("ASCII/" + asciiFile + ".txt"), TextCodec.ASCII);
        // These PS fixtures are contiguous fixed-length records, without RDWs or separators.
        assertEquals(ascii.size() * codec.length(), shipped.length, dataset);
        List<Difference> actual = new ArrayList<>();
        for (int i = 0; i < ascii.size(); i++) {
            byte[] bytes = Arrays.copyOfRange(shipped, i * codec.length(), (i + 1) * codec.length());
            T ebcdic = codec.decode(bytes, TextCodec.EBCDIC);
            assertArrayEquals(bytes, codec.encode(ebcdic, TextCodec.EBCDIC), dataset + " record " + i);
            String line = ascii.get(i);
            if (asciiFile.equals("cardxref")) {
                assertEquals(36, line.length());
                line += " ".repeat(14);
            }
            T text = codec.decode(line.getBytes(TextCodec.ASCII));
            assertEquals(key.apply(text), key.apply(ebcdic), dataset + " key at record " + i);
            for (var component : text.getClass().getRecordComponents()) {
                Object left = component.getAccessor().invoke(text);
                Object right = component.getAccessor().invoke(ebcdic);
                if (!left.equals(right)) {
                    actual.add(new Difference(key.apply(text), component.getName(), display(left), display(right)));
                }
            }
        }
        assertEquals(expected, actual, dataset + " fixture drift (including missing known differences)");
    }

    private static String display(Object value) {
        if (value instanceof ZonedDecimal decimal) {
            return (decimal.negativeZero() ? "-" : "") + decimal.value().toPlainString();
        }
        return value.toString();
    }
}
