package com.carddemo.programs.cbimport.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.HexFormat;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ExportRecordTest {

    private static final Charset EBCDIC = Charset.forName("IBM037");

    @Test
    void decodesHeaderAndTimestampRedefines() {
        byte[] raw = ExportRecord.encode("C", "2025-09-28 22:53:40.000000", 509, "0001", "NORTH",
                new byte[0], EBCDIC);
        ExportRecord r = ExportRecord.decode(raw, EBCDIC);
        assertEquals("C", r.recType());
        assertEquals(509, r.sequenceNum());
        assertEquals("0001", r.branchId());
        assertEquals("NORTH", r.regionCode());
        assertEquals("2025-09-28", r.exportDate());
        assertEquals(" ", r.exportDateTimeSep());
        assertEquals("22:53:40.000000", r.exportTime());
        assertEquals("000001FD", HexFormat.of().withUpperCase().formatHex(raw, 27, 31));
    }

    @Test
    void sequenceNumberIsUnsignedFullword() {
        byte[] raw = ExportRecord.encode("Z", "", 4_294_967_295L, "", "", new byte[0], EBCDIC);
        assertEquals(4_294_967_295L, ExportRecord.decode(raw, EBCDIC).sequenceNum());
    }

    @Test
    void rejectsWrongLengthRecord() {
        assertThrows(IllegalArgumentException.class, () -> ExportRecord.decode(new byte[499], EBCDIC));
    }

    @Test
    void decodesPackedAndBinaryAccountFields() {
        var acct = new ExportRecord.AccountData(BigInteger.valueOf(42), "Y", new BigDecimal("-1234.56"),
                new BigDecimal("5000.00"), new BigDecimal("250.75"), "2020-10-22", "2025-06-20", "2025-05-20",
                new BigDecimal("-0.01"), new BigDecimal("-987654321.09"), "12345", "GOLD");
        byte[] data = acct.encode(EBCDIC);
        assertEquals("0000000123456D", HexFormat.of().withUpperCase().formatHex(data, 12, 19));
        ExportRecord r = ExportRecord.decode(ExportRecord.encode("A", "", 1, "", "", data, EBCDIC), EBCDIC);
        assertEquals(acct.currBal(), r.accountData().currBal());
        assertEquals(acct.cashCreditLimit(), r.accountData().cashCreditLimit());
        assertEquals(acct.currCycCredit(), r.accountData().currCycCredit());
        assertEquals(acct.currCycDebit(), r.accountData().currCycDebit());
    }

    @Test
    void unsignedPackedIgnoresSignNibble() {
        var cust = new ExportRecord.CustomerData(BigInteger.ONE, "", "", "", List.of("", "", ""), "", "", "",
                List.of("", ""), BigInteger.ZERO, "", "", "", "", BigInteger.valueOf(812));
        byte[] data = cust.encode(EBCDIC);
        assertEquals("812F", HexFormat.of().withUpperCase().formatHex(data, 324, 326));
        data[325] = (byte) 0x2D;
        ExportRecord r = ExportRecord.decode(ExportRecord.encode("C", "", 1, "", "", data, EBCDIC), EBCDIC);
        assertEquals(BigInteger.valueOf(812), r.customerData().ficoCreditScore());
    }

    @Test
    void decodesDoublewordAndHalfwordBinaryInCardViews() {
        var card = new ExportRecord.CardData("4111111111111111", new BigInteger("18446744073709551615"),
                BigInteger.valueOf(65535), "NAME", "2030-12-31", "Y");
        ExportRecord r = ExportRecord.decode(
                ExportRecord.encode("D", "", 1, "", "", card.encode(EBCDIC), EBCDIC), EBCDIC);
        assertEquals(new BigInteger("18446744073709551615"), r.cardData().acctId());
        assertEquals(BigInteger.valueOf(65535), r.cardData().cvvCd());
    }

    @Test
    void ebcdicZonedSignAcceptsAlternateNegativeAndPositiveZones() {
        byte[] negB = HexFormat.of().parseHex("F0F0B1");
        assertEquals(new BigDecimal("-0.01"), new FieldCursor(negB, 0, EBCDIC).zonedSigned(1, 2));
        byte[] negD = HexFormat.of().parseHex("F1F2D3");
        assertEquals(new BigDecimal("-1.23"), new FieldCursor(negD, 0, EBCDIC).zonedSigned(1, 2));
        byte[] posA = HexFormat.of().parseHex("F4F5A6");
        assertEquals(new BigDecimal("4.56"), new FieldCursor(posA, 0, EBCDIC).zonedSigned(1, 2));
        byte[] posC = HexFormat.of().parseHex("F7F8C9");
        assertEquals(new BigDecimal("7.89"), new FieldCursor(posC, 0, EBCDIC).zonedSigned(1, 2));
        assertEquals(BigInteger.valueOf(201), new FieldCursor(HexFormat.of().parseHex("F2F0B1"), 0, EBCDIC)
                .zonedUnsigned(3));
    }
}
