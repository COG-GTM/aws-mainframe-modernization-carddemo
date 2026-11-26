package com.carddemo.programs.cbtrn01c.copybook;

import com.carddemo.programs.cbtrn01c.SampleData;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class CopybookLayoutTest {

    @Test
    void recordLengthsMatchCopybooks() {
        assertEquals(350, DalytranRecord.initial().toFixedWidth().length());
        assertEquals(350, TranRecord.initial().toFixedWidth().length());
        assertEquals(500, CustomerRecord.initial().toFixedWidth().length());
        assertEquals(50, CardXrefRecord.initial().toFixedWidth().length());
        assertEquals(150, CardRecord.initial().toFixedWidth().length());
        assertEquals(300, AccountRecord.initial().toFixedWidth().length());
    }

    @Test
    void dalytranFieldOffsetsFromSampleRecord() throws Exception {
        List<String> lines = lines("dailytran.txt");
        DalytranRecord d = DalytranRecord.CODEC.parse(lines.get(0));
        assertEquals("0000000000683580", d.id());
        assertEquals("01", d.typeCd());
        assertEquals(1, d.catCd());
        assertEquals("POS TERM  ", d.source());
        assertEquals("Purchase at Abshire-Lowe", d.desc().strip());
        assertEquals(new BigDecimal("504.77"), d.amt());
        assertEquals(800000000L, d.merchantId());
        assertEquals("Abshire-Lowe", d.merchantName().strip());
        assertEquals("North Enoshaven", d.merchantCity().strip());
        assertEquals("72112     ", d.merchantZip());
        assertEquals("4859452612877065", d.cardNum());
        assertEquals("2022-06-10 19:27:53.000000", d.origTs());
        assertEquals(" ".repeat(26), d.procTs());
        assertEquals(new BigDecimal("-919.00"), DalytranRecord.CODEC.parse(lines.get(1)).amt());
    }

    @Test
    void xrefAndAccountFieldOffsetsFromSampleRecord() throws Exception {
        CardXrefRecord x = CardXrefRecord.CODEC.parse(lines("cardxref.txt").get(0));
        assertEquals("0500024453765740", x.cardNum());
        assertEquals(50, x.custId());
        assertEquals(50, x.acctId());
        assertEquals("00000000050", x.acctIdDisplay());
        assertEquals("000000050", x.custIdDisplay());

        AccountRecord a = AccountRecord.CODEC.parse(lines("acctdata.txt").get(0));
        assertEquals(1, a.acctId());
        assertEquals("Y", a.activeStatus());
        assertEquals(new BigDecimal("194.00"), a.currBal());
        assertEquals(new BigDecimal("2020.00"), a.creditLimit());
        assertEquals(new BigDecimal("1020.00"), a.cashCreditLimit());
        assertEquals("2014-11-20", a.openDate());
        assertEquals("2025-05-20", a.expiraionDate());
        assertEquals("2025-05-20", a.reissueDate());
        assertEquals(new BigDecimal("0.00"), a.currCycCredit());
        assertEquals(new BigDecimal("0.00"), a.currCycDebit());
        assertEquals("A000000000", a.addrZip());
    }

    @Test
    void cardFieldOffsetsFromSampleRecord() throws Exception {
        CardRecord c = CardRecord.CODEC.parse(lines("carddata.txt").get(0));
        assertEquals("0500024453765740", c.cardNum());
        assertEquals(50, c.acctId());
        assertEquals(747, c.cvvCd());
        assertEquals("2023-03-09", c.expiraionDate());
        assertEquals("Y", c.activeStatus());
    }

    @Test
    void sampleFilesRoundTripByteForByte() throws Exception {
        roundTrip("dailytran.txt", DalytranRecord.CODEC);
        roundTrip("cardxref.txt", CardXrefRecord.CODEC);
        roundTrip("acctdata.txt", AccountRecord.CODEC);
        roundTrip("carddata.txt", CardRecord.CODEC);
        roundTrip("custdata.txt", CustomerRecord.CODEC);
    }

    @Test
    void customerRecordSyntheticRoundTrip() {
        CustomerRecord c = new CustomerRecord(123, "FIRST", "MIDDLE", "LAST", "1 TEST ST", "", "TESTTOWN", "NY",
                "USA", "10001", "(000)000-0000", "", 999, "ID-1", "1990-01-31", "EFT0000001", "Y", 750, "");
        String image = c.toFixedWidth();
        assertEquals(500, image.length());
        assertEquals("000000123FIRST", image.substring(0, 14));
        assertEquals(c, CustomerRecord.CODEC.parse(image));
        assertEquals(234, new CustomerRecord(1234, "", "", "", "", "", "", "", "", "", "", "", 0, "", "", "",
                "", 1234, "").ficoCreditScore());
    }

    @Test
    void tranRecordRoundTripAndTruncation() {
        TranRecord t = new TranRecord("TID", "01", 12345, "SRC", "DESC", new BigDecimal("-1234567890.129"), 7,
                "M", "C", "Z", "CARD", "TS1", "TS2", "");
        assertEquals("TID" + " ".repeat(13), t.id());
        assertEquals(2345, t.catCd());
        assertEquals(new BigDecimal("-234567890.12"), t.amt());
        assertEquals(t, TranRecord.CODEC.parse(t.toFixedWidth()));
    }

    private static <R> void roundTrip(String file, RecordCodec<R> codec) throws Exception {
        for (String line : lines(file)) {
            String image = Pic.alpha(line, codec.length());
            assertEquals(image, codec.format(codec.parse(image)), file);
        }
    }

    private static List<String> lines(String file) throws Exception {
        Path p = SampleData.file(file);
        assumeTrue(p != null && Files.exists(p), "sample data not available");
        return Files.readAllLines(p, StandardCharsets.ISO_8859_1);
    }
}
