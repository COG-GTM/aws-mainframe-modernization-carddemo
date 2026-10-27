package com.carddemo.modernization.records;

import com.carddemo.modernization.records.codec.*;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import static org.junit.jupiter.api.Assertions.*;

class CodecsTest {
    @ParameterizedTest
    @ValueSource(strings = {"{","A","B","C","D","E","F","G","H","I","}","J","K","L","M","N","O","P","Q","R"})
    void overpunch(String sign) {
        for (Charset charset : new Charset[]{TextCodec.ASCII, TextCodec.EBCDIC}) {
            ZonedDecimalCodec codec = new ZonedDecimalCodec(3, 2);
            byte[] bytes = TextCodec.encode("00" + sign, 3, charset);
            ZonedDecimal value = codec.decode(bytes, charset);
            int index = "{ABCDEFGHI}JKLMNOPQR".indexOf(sign);
            assertEquals(BigDecimal.valueOf(index < 10 ? index : -(index - 10), 2), value.value());
            assertEquals(sign.equals("}"), value.negativeZero());
            assertArrayEquals(bytes, codec.encode(value, charset));
        }
    }
    @ParameterizedTest @ValueSource(strings = {"0.00","1.23","-1.23","999.99","-999.99"})
    void decimals(String text) {
        BigDecimal value = new BigDecimal(text);
        PackedDecimalCodec packed = new PackedDecimalCodec(5, 2);
        assertEquals(value, packed.decode(packed.encode(value)));
        ZonedDecimalCodec zoned = new ZonedDecimalCodec(5, 2);
        assertEquals(value, zoned.decode(zoned.encode(new ZonedDecimal(value), TextCodec.ASCII), TextCodec.ASCII).value());
    }
    @Test void invalidDecimals() {
        PackedDecimalCodec packed = new PackedDecimalCodec(3, 2);
        ZonedDecimalCodec zoned = new ZonedDecimalCodec(3, 2);
        for (String text : new String[]{"10.00","-10.00","1.0","1.000"}) {
            BigDecimal value = new BigDecimal(text);
            assertThrows(IllegalArgumentException.class, () -> packed.encode(value));
            assertThrows(IllegalArgumentException.class, () -> zoned.encode(new ZonedDecimal(value), TextCodec.ASCII));
        }
        for (String text : new String[]{"","00","000","0X{","00!"})
            assertThrows(IllegalArgumentException.class, () -> zoned.decode(text.getBytes(TextCodec.ASCII), TextCodec.ASCII));
        for (byte[] bytes : new byte[][]{new byte[0], {0x12,0x3A}, {0x1A,0x3C}})
            assertThrows(IllegalArgumentException.class, () -> packed.decode(bytes));
        PackedDecimalCodec even = new PackedDecimalCodec(2, 0);
        assertEquals(new BigDecimal("12"), even.decode(new byte[]{0x01,0x2F}));
        assertThrows(IllegalArgumentException.class, () -> even.decode(new byte[]{0x11,0x2C}));
    }
    @ParameterizedTest @ValueSource(ints = {1,4,5,9,10,18})
    void binary(int digits) {
        BinaryCodec codec = new BinaryCodec(digits);
        long max = java.math.BigInteger.TEN.pow(digits).longValueExact() - 1;
        for (long value : new long[]{0,1,-1,max,-max})
            assertEquals(value, codec.decode(codec.encode(value)));
        assertEquals(digits <= 4 ? 2 : digits <= 9 ? 4 : 8, codec.encode(0).length);
        assertThrows(IllegalArgumentException.class, () -> codec.encode(max + 1));
        assertThrows(IllegalArgumentException.class, () -> codec.encode(-max - 1));
        assertThrows(IllegalArgumentException.class, () -> codec.decode(new byte[0]));
        assertArrayEquals(new byte[]{-1,-2}, new BinaryCodec(4).encode(-2));
    }
    @Test void textAndUnsigned() {
        for (Charset charset : new Charset[]{TextCodec.ASCII,TextCodec.EBCDIC}) {
            byte[] bytes = TextCodec.encode("AB", 4, charset);
            assertEquals("AB  ", TextCodec.decode(bytes, charset));
            assertArrayEquals(bytes, TextCodec.encode(TextCodec.decode(bytes, charset), 4, charset));
            assertEquals("    ", TextCodec.decode(TextCodec.encode("", 4, charset), charset));
            assertThrows(IllegalArgumentException.class, () -> TextCodec.encode("ABCDE", 4, charset));
            assertThrows(IllegalArgumentException.class, () -> TextCodec.encode("😀", 4, charset));
        }
        assertArrayEquals(new byte[]{(byte)0xC1,0x40}, TextCodec.encode("A", 2, TextCodec.EBCDIC));
        for (String text : new String[]{"0000","0009","9999"})
            assertEquals(text, ZonedDecimalCodec.unsigned(text, 4));
        for (String text : new String[]{"","10000","-001","00A1"})
            assertThrows(IllegalArgumentException.class, () -> ZonedDecimalCodec.unsigned(text, 4));
    }
    private static java.util.List<String> lines(String file) throws Exception {
        return Files.readAllLines(Path.of("../../../app/data/ASCII/" + file + ".txt"), TextCodec.ASCII);
    }
    private static <T> void roundTrip(RecordCodec<T> codec, String file, int count, boolean pad) throws Exception {
        var lines = lines(file);
        assertEquals(count, lines.size());
        for (String line : lines) {
            String raw = pad ? line + " ".repeat(codec.length() - line.length()) : line;
            byte[] bytes = raw.getBytes(TextCodec.ASCII);
            T value = codec.decode(bytes);
            assertArrayEquals(bytes, codec.encode(value));
            byte[] ebcdic = codec.encode(value, TextCodec.EBCDIC);
            assertArrayEquals(bytes, codec.encode(codec.decode(ebcdic, TextCodec.EBCDIC)));
            assertArrayEquals(ebcdic, codec.encode(codec.decode(ebcdic, TextCodec.EBCDIC), TextCodec.EBCDIC));
        }
        assertThrows(IllegalArgumentException.class, () -> codec.decode(new byte[0]));
        assertThrows(IllegalArgumentException.class, () -> codec.decode(new byte[codec.length()+1]));
    }
    @Test void accounts() throws Exception {
        roundTrip(AccountRecord.CODEC,"acctdata",50,false);
        String raw = lines("acctdata").getFirst();
        AccountRecord value = AccountRecord.CODEC.decode(raw.getBytes(TextCodec.ASCII));
        assertEquals(raw.substring(0,11), value.acctId());
        assertEquals(new BigDecimal("194.00"), value.currentBalance().value());
        assertEquals(raw.substring(122), value.filler());
    }
    @Test void xrefs() throws Exception {
        roundTrip(CardXrefRecord.CODEC,"cardxref",50,true);
        String raw = lines("cardxref").getFirst();
        assertEquals(36, raw.length());
        var value = CardXrefRecord.CODEC.decode((raw+" ".repeat(14)).getBytes(TextCodec.ASCII));
        assertEquals(raw.substring(16,25), value.customerId());
        assertEquals(raw.substring(25), value.accountId());
    }
    @Test void balances() throws Exception {
        roundTrip(TranCatBalanceRecord.CODEC,"tcatbal",50,false);
        var value = TranCatBalanceRecord.CODEC.decode(lines("tcatbal").getFirst().getBytes(TextCodec.ASCII));
        assertEquals("00000000001", value.accountId());
        assertEquals(new BigDecimal("0.00"), value.balance().value());
    }
    @Test void disclosures() throws Exception {
        roundTrip(DisclosureGroupRecord.CODEC,"discgrp",51,false);
        var value = DisclosureGroupRecord.CODEC.decode(lines("discgrp").getFirst().getBytes(TextCodec.ASCII));
        assertEquals("A000000000", value.groupId());
        assertEquals(new BigDecimal("15.00"), value.interestRate().value());
        assertTrue(lines("discgrp").getLast().startsWith("ZEROAPR   070001"));
    }
    @Test void transactions() throws Exception {
        roundTrip(TransactionRecord.CODEC,"dailytran",300,false);
        String raw = lines("dailytran").getFirst();
        var value = TransactionRecord.CODEC.decode(raw.getBytes(TextCodec.ASCII));
        assertEquals(raw.substring(0,16), value.id());
        assertEquals(new BigDecimal("504.77"), value.amount().value());
        assertEquals(raw.substring(143,152), value.merchantId());
    }
    @Test void daily() throws Exception { roundTrip(DailyTransactionRecord.CODEC,"dailytran",300,false); }
    @Test void reject() throws Exception {
        String raw = lines("dailytran").getFirst() + "0100" + "INVALID ACCOUNT" + " ".repeat(61);
        var value = RejectRecord.CODEC.decode(raw.getBytes(TextCodec.ASCII));
        assertEquals("0100", value.reasonCode());
        assertEquals("INVALID ACCOUNT", value.reasonDescription().stripTrailing());
        assertArrayEquals(raw.getBytes(TextCodec.ASCII), RejectRecord.CODEC.encode(value));
        assertEquals(value, RejectRecord.CODEC.decode(RejectRecord.CODEC.encode(value,TextCodec.EBCDIC),TextCodec.EBCDIC));
        assertThrows(IllegalArgumentException.class, () -> RejectRecord.CODEC.decode(new byte[0]));
    }
    @Test void negativeZeroRecord() throws Exception {
        String raw = lines("dailytran").getFirst();
        String zero = raw.substring(0,132) + "0000000000}" + raw.substring(143);
        var value = TransactionRecord.CODEC.decode(zero.getBytes(TextCodec.ASCII));
        assertTrue(value.amount().negativeZero());
        assertEquals(new BigDecimal("0.00"), value.amount().value());
        assertArrayEquals(zero.getBytes(TextCodec.ASCII), TransactionRecord.CODEC.encode(value));
    }
}
