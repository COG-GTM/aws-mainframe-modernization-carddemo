package com.carddemo.programs.cbexport.copybook;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;

class ExportRecordTest {

    private static byte[] slice(byte[] b, int from, int len) {
        return Arrays.copyOfRange(b, from, from + len);
    }

    @Test
    void layoutTotalsFiveHundredBytes() {
        assertEquals(500, ExportRecord.LENGTH);
        assertEquals(ExportRecord.LENGTH, ExportRecord.DATA_OFFSET + ExportRecord.DATA_LENGTH);
        assertEquals(27, ExportRecord.SEQUENCE_NUM_OFFSET);
        assertEquals(4, ExportRecord.SEQUENCE_NUM_LENGTH);
    }

    @Test
    void initializeSetsSpacesAndZeroBinarySequence() {
        ExportRecord r = new ExportRecord();
        r.setRecType('C');
        r.setSequenceNum(99);
        r.setTimestamp("x");
        r.initialize();
        byte[] b = r.toBytes();
        assertEquals(' ', r.recType());
        assertEquals(0, r.sequenceNum());
        assertArrayEquals(new byte[4], slice(b, 27, 4));
        assertEquals(" ".repeat(26), r.timestamp());
        assertEquals(" ".repeat(460), r.recordData());
    }

    @Test
    void timestampRedefinesSplitsDateSeparatorTime() {
        ExportRecord r = new ExportRecord();
        r.setTimestamp("2026-11-26 17:22:05.00");
        assertEquals("2026-11-26", r.exportDate());
        assertEquals(" ", r.exportDateTimeSep());
        assertEquals("17:22:05.00    ", r.exportTime());
    }

    @Test
    void customerLayoutUsesCompIdAndComp3Fico() {
        ExportRecord r = new ExportRecord();
        ExportCustomerData c = new ExportCustomerData(500, "A", "B", "C", List.of("L1", "L2", "L3"),
                "NC", "USA", "12546", List.of("P1", "P2"), 20973888L, "G", "1961-06-08", "E", "Y", 274);
        r.setCustomerData(c);
        byte[] b = r.toBytes();
        assertArrayEquals(new byte[] {0, 0, 1, (byte) 0xF4}, slice(b, 40, 4));
        assertEquals("020973888", new String(slice(b, 40 + 274, 9), CobolFieldCodec.CHARSET));
        assertArrayEquals(new byte[] {0x27, 0x4F}, slice(b, 40 + 324, 2));
        assertEquals(" ".repeat(134), new String(slice(b, 40 + 326, 134), CobolFieldCodec.CHARSET));
        ExportCustomerData back = r.customerData();
        assertEquals(500, back.custId());
        assertEquals("L3", back.addrLines().get(2).strip());
        assertEquals("P2", back.phoneNums().get(1).strip());
        assertEquals(274, back.ficoCreditScore());
    }

    @Test
    void customerDataRequiresOccursCounts() {
        assertThrows(IllegalArgumentException.class, () -> new ExportCustomerData(1, "", "", "",
                List.of("a"), "", "", "", List.of("a", "b"), 0, "", "", "", "", 0));
    }

    @Test
    void accountLayoutMixesComp3DisplayAndComp() {
        ExportRecord r = new ExportRecord();
        r.setAccountData(new ExportAccountData(1, "Y", new BigDecimal("-194.00"), new BigDecimal("2020.00"),
                new BigDecimal("1020.00"), "2014-11-20", "2025-05-20", "2025-05-20",
                new BigDecimal("-5.01"), new BigDecimal("-1.50"), "A000000000", "GRP"));
        byte[] b = r.toBytes();
        assertEquals("00000000001", new String(slice(b, 40, 11), CobolFieldCodec.CHARSET));
        assertArrayEquals(new byte[] {0, 0, 0, 0, 0x19, 0x40, 0x0D}, slice(b, 52, 7));
        assertEquals("00000020200{", new String(slice(b, 59, 12), CobolFieldCodec.CHARSET));
        assertEquals("00000000050J", new String(slice(b, 40 + 68, 12), CobolFieldCodec.CHARSET));
        assertEquals(-150, CobolFieldCodec.getBinary(b, 40 + 80, 12, true));
        ExportAccountData a = r.accountData();
        assertEquals(new BigDecimal("-194.00"), a.currBal());
        assertEquals(new BigDecimal("1020.00"), a.cashCreditLimit());
        assertEquals(new BigDecimal("-1.50"), a.currCycDebit());
        assertEquals("GRP       ", a.groupId());
    }

    @Test
    void transactionLayoutPacksAmountAndBinaryMerchant() {
        ExportRecord r = new ExportRecord();
        r.setTransactionData(new ExportTransactionData("T1", "01", 1, "POS TERM", "D",
                new BigDecimal("504.77"), 800000000L, "M", "C", "Z", "4859452612877065",
                "2022-06-10 19:27:53.000000", ""));
        byte[] b = r.toBytes();
        assertEquals("0001", new String(slice(b, 40 + 18, 4), CobolFieldCodec.CHARSET));
        assertArrayEquals(new byte[] {0, 0, 0, 0x50, 0x47, 0x7C}, slice(b, 40 + 132, 6));
        assertEquals(800000000L, CobolFieldCodec.getBinary(b, 40 + 138, 9, false));
        ExportTransactionData t = r.transactionData();
        assertEquals(new BigDecimal("504.77"), t.amt());
        assertEquals("4859452612877065", t.cardNum());
    }

    @Test
    void xrefAndCardLayouts() {
        ExportRecord x = new ExportRecord();
        x.setCardXrefData(new ExportCardXrefData("0500024453765740", 50, 50));
        byte[] xb = x.toBytes();
        assertEquals("000000050", new String(slice(xb, 56, 9), CobolFieldCodec.CHARSET));
        assertArrayEquals(new byte[] {0, 0, 0, 0, 0, 0, 0, 50}, slice(xb, 65, 8));
        assertEquals(new ExportCardXrefData("0500024453765740", 50, 50), x.cardXrefData());

        ExportRecord c = new ExportRecord();
        c.setCardData(new ExportCardData("0500024453765740", 50, 747, "Aniya Von", "2023-03-09", "Y"));
        byte[] cb = c.toBytes();
        assertArrayEquals(new byte[] {0x02, (byte) 0xEB}, slice(cb, 40 + 24, 2));
        assertEquals(747, c.cardData().cvvCd());
        assertEquals("Y", c.cardData().activeStatus());
    }

    @Test
    void recordTypeCodes() {
        assertEquals(ExportRecordType.CARD, ExportRecordType.fromCode('D'));
        assertEquals('X', ExportRecordType.XREF.code());
        assertNull(ExportRecordType.fromCode('Z'));
        assertThrows(IllegalArgumentException.class, () -> ExportRecord.fromBytes(new byte[10]));
    }
}
