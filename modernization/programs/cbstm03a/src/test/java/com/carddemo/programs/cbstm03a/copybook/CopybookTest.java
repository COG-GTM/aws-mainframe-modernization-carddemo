package com.carddemo.programs.cbstm03a.copybook;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;

class CopybookTest {

    private static final Path ASCII = Path.of("../../../app/data/ASCII");

    private static List<String> lines(String name) throws IOException {
        return Files.readAllLines(ASCII.resolve(name), StandardCharsets.ISO_8859_1);
    }

    @Test
    void cardXrefRoundTripsSampleData() throws IOException {
        String raw = lines("cardxref.txt").get(0);
        CardXrefRecord r = CardXrefRecord.parse(raw);
        assertEquals("0500024453765740", r.xrefCardNum());
        assertEquals(50L, r.xrefCustId());
        assertEquals(50L, r.xrefAcctId());
        assertEquals("000000050", r.xrefCustIdDisplay());
        assertEquals("00000000050", r.xrefAcctIdDisplay());
        assertEquals(CardXrefRecord.LENGTH, r.toFixedWidth().length());
        for (String line : lines("cardxref.txt")) {
            assertEquals(String.format("%-50s", line), CardXrefRecord.parse(line).toFixedWidth());
        }
    }

    @Test
    void customerRoundTripsSampleData() throws IOException {
        CustomerRecord r = CustomerRecord.parse(lines("custdata.txt").get(0));
        assertEquals(1L, r.custId());
        assertEquals("Immanuel                 ", r.custFirstName());
        assertEquals("Madeline                 ", r.custMiddleName());
        assertEquals("Kessler                  ", r.custLastName());
        assertEquals("NC", r.custAddrStateCd());
        assertEquals("USA", r.custAddrCountryCd());
        assertEquals(274, r.custFicoCreditScore());
        assertEquals("274", r.custFicoCreditScoreDisplay());
        for (String line : lines("custdata.txt")) {
            assertEquals(String.format("%-500s", line), CustomerRecord.parse(line).toFixedWidth());
        }
    }

    @Test
    void accountRoundTripsSampleDataWithSignedDecimals() throws IOException {
        AccountRecord r = AccountRecord.parse(lines("acctdata.txt").get(0));
        assertEquals(1L, r.acctId());
        assertEquals("Y", r.acctActiveStatus());
        assertEquals(new BigDecimal("194.00"), r.acctCurrBal());
        assertEquals(new BigDecimal("2020.00"), r.acctCreditLimit());
        assertEquals(new BigDecimal("1020.00"), r.acctCashCreditLimit());
        assertEquals("2014-11-20", r.acctOpenDate());
        assertEquals("00000000001", r.acctIdDisplay());
        for (String line : lines("acctdata.txt")) {
            assertEquals(String.format("%-300s", line), AccountRecord.parse(line).toFixedWidth());
        }
    }

    @Test
    void transactionLayoutSplitsKeyAndRest() throws IOException {
        String daily = String.format("%-350s", lines("dailytran.txt").get(0));
        String reshaped = daily.substring(262, 278) + daily.substring(0, 262) + daily.substring(278, 328);
        TrnxRecord r = TrnxRecord.parse(reshaped);
        assertEquals(daily.substring(262, 278), r.trnxCardNum());
        assertEquals("0000000000683580", r.trnxId());
        assertEquals("01", r.trnxTypeCd());
        assertEquals(1, r.trnxCatCd());
        assertEquals("POS TERM  ", r.trnxSource());
        assertEquals(new BigDecimal("504.77"), r.trnxAmt());
        assertEquals(800000000L, r.trnxMerchantId());
        assertEquals(TrnxRecord.REST_LENGTH, r.rest().length());
        assertEquals(String.format("%-350s", reshaped), r.toFixedWidth());
        assertEquals(r, TrnxRecord.of(r.trnxCardNum(), r.trnxId(), r.rest()));
    }

    @Test
    void m03bAreaFieldsAreFixedWidthAnd88LevelsMapToCodes() {
        M03bArea a = new M03bArea();
        a.dd("XREF");
        a.key("000000050");
        a.fldt("abc");
        a.rc("1");
        assertEquals("XREF    ", a.dd());
        assertEquals(25, a.key().length());
        assertEquals(M03bArea.FLDT_LENGTH, a.fldt().length());
        assertEquals("1 ", a.rc());
        assertEquals('O', M03bArea.Oper.OPEN.code());
        assertEquals('C', M03bArea.Oper.CLOSE.code());
        assertEquals('R', M03bArea.Oper.READ.code());
        assertEquals('K', M03bArea.Oper.READ_K.code());
        assertEquals('W', M03bArea.Oper.WRITE.code());
        assertEquals('Z', M03bArea.Oper.REWRITE.code());
    }
}
