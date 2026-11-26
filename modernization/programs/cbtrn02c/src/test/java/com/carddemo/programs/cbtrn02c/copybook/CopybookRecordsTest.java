package com.carddemo.programs.cbtrn02c.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CopybookRecordsTest {

    static final String DALYTRAN_SAMPLE = "0000000000683580010001POS TERM  Purchase at Abshire-Lowe"
            + " ".repeat(76) + "0000005047G800000000Abshire-Lowe" + " ".repeat(38) + "North Enoshaven"
            + " ".repeat(35) + "72112     48594526128770652022-06-10 19:27:53.000000" + " ".repeat(46);

    @Test
    void dalytranRecordLayoutCvtra06y() {
        assertEquals(350, DALYTRAN_SAMPLE.length());
        DalytranRecord d = DalytranRecord.parse(DALYTRAN_SAMPLE);
        assertEquals("0000000000683580", d.dalytranId());
        assertEquals("01", d.dalytranTypeCd());
        assertEquals(1, d.dalytranCatCd());
        assertEquals("POS TERM  ", d.dalytranSource());
        assertEquals(new BigDecimal("504.77"), d.dalytranAmt());
        assertEquals(800000000L, d.dalytranMerchantId());
        assertEquals("72112     ", d.dalytranMerchantZip());
        assertEquals("4859452612877065", d.dalytranCardNum());
        assertEquals("2022-06-10 19:27:53.000000", d.dalytranOrigTs());
        assertEquals(" ".repeat(26), d.dalytranProcTs());
        assertEquals(DALYTRAN_SAMPLE, d.format());
    }

    @Test
    void tranRecordLayoutCvtra05yRoundTrips() {
        TranRecord t = TranRecord.parse(DALYTRAN_SAMPLE);
        assertEquals(DALYTRAN_SAMPLE, t.format());
        assertEquals(350, TranRecord.RECORD_LENGTH);
    }

    @Test
    void cardXrefLayoutCvact03yPadsShortLines() {
        CardXrefRecord x = CardXrefRecord.parse("050002445376574000000005000000000050");
        assertEquals("0500024453765740", x.xrefCardNum());
        assertEquals(50L, x.xrefCustId());
        assertEquals(50L, x.xrefAcctId());
        assertEquals("050002445376574000000005000000000050" + " ".repeat(14), x.format());
    }

    @Test
    void accountLayoutCvact01y() {
        String raw = "00000000001Y00000001940{00000020200{00000010200{2014-11-202025-05-202025-05-20"
                + "00000000000{00000000000{A000000000" + " ".repeat(188);
        AccountRecord a = AccountRecord.parse(raw);
        assertEquals(1L, a.acctId());
        assertEquals("Y", a.acctActiveStatus());
        assertEquals(new BigDecimal("194.00"), a.acctCurrBal());
        assertEquals(new BigDecimal("2020.00"), a.acctCreditLimit());
        assertEquals(new BigDecimal("1020.00"), a.acctCashCreditLimit());
        assertEquals("2014-11-20", a.acctOpenDate());
        assertEquals("2025-05-20", a.acctExpiraionDate());
        assertEquals("2025-05-20", a.acctReissueDate());
        assertEquals(new BigDecimal("0.00"), a.acctCurrCycCredit());
        assertEquals(new BigDecimal("0.00"), a.acctCurrCycDebit());
        assertEquals("A000000000", a.acctAddrZip());
        assertEquals("          ", a.acctGroupId());
        assertEquals(raw, a.format());
    }

    @Test
    void tranCatBalLayoutCvtra01y() {
        String raw = "000000000010100010000011648G0000000000000000000000";
        TranCatBalRecord t = TranCatBalRecord.parse(raw);
        assertEquals(new TranCatBalRecord.TranCatKey(1L, "01", 1), t.tranCatKey());
        assertEquals("00000000001010001", t.tranCatKey().format());
        assertEquals(new BigDecimal("1164.87"), t.tranCatBal());
        assertEquals("0000000000000000000000", t.filler());
        assertEquals(raw, t.format());
    }
}
