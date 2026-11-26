package com.carddemo.programs.cbact04c.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Byte layouts of the copybooks COPYed by CBACT04C, checked against app/data/ASCII sample records. */
class CopybookLayoutTest {

    @Test
    void cvtra01yTranCatBalRecord() {
        String image = "000000000010100010000000000{0000000000000000000000";
        TranCatBalRecord r = TranCatBalRecord.CODEC.decode(image);
        assertEquals(1L, r.trancatAcctId());
        assertEquals("01", r.trancatTypeCd());
        assertEquals(1, r.trancatCd());
        assertEquals(new BigDecimal("0.00"), r.tranCatBal());
        assertEquals(image, TranCatBalRecord.CODEC.encode(r));
        assertEquals(50, TranCatBalRecord.LENGTH);
        assertEquals(new TranCatBalRecord.Key(1L, "01", 1), r.key());
    }

    @Test
    void cvact03yCardXrefRecord() {
        String image = "050002445376574000000005000000000050              ";
        CardXrefRecord r = CardXrefRecord.CODEC.decode(image);
        assertEquals("0500024453765740", r.xrefCardNum());
        assertEquals(50L, r.xrefCustId());
        assertEquals(50L, r.xrefAcctId());
        assertEquals(image, CardXrefRecord.CODEC.encode(r));
    }

    @Test
    void cvtra02yDisGroupRecord() {
        String image = "DEFAULT   01000100150{0000000000000000000000000000";
        DisGroupRecord r = DisGroupRecord.CODEC.decode(image);
        assertEquals("DEFAULT   ", r.disAcctGroupId());
        assertEquals("01", r.disTranTypeCd());
        assertEquals(1, r.disTranCatCd());
        assertEquals(new BigDecimal("15.00"), r.disIntRate());
        assertEquals(image, DisGroupRecord.CODEC.encode(r));
        assertEquals(new DisGroupRecord.Key("DEFAULT", "01", 1), r.key());
    }

    @Test
    void cvact01yAccountRecord() {
        String image = ("00000000001Y00000001940{00000020200{00000010200{2014-11-202025-05-202025-05-20"
                + "00000000000{00000000000{A000000000");
        image = image + " ".repeat(300 - image.length());
        AccountRecord r = AccountRecord.CODEC.decode(image);
        assertEquals(1L, r.acctId());
        assertEquals("Y", r.acctActiveStatus());
        assertEquals(new BigDecimal("194.00"), r.acctCurrBal());
        assertEquals(new BigDecimal("2020.00"), r.acctCreditLimit());
        assertEquals(new BigDecimal("1020.00"), r.acctCashCreditLimit());
        assertEquals("2014-11-20", r.acctOpenDate());
        assertEquals("2025-05-20", r.acctExpiraionDate());
        assertEquals("2025-05-20", r.acctReissueDate());
        assertEquals(new BigDecimal("0.00"), r.acctCurrCycCredit());
        assertEquals(new BigDecimal("0.00"), r.acctCurrCycDebit());
        assertEquals("A000000000", r.acctAddrZip());
        assertEquals("          ", r.acctGroupId());
        assertEquals(image, AccountRecord.CODEC.encode(r));
    }

    @Test
    void cvtra05yTranRecordIs350Bytes() {
        TranRecord r = new TranRecord("2022071800000001", "01", 5, "System", "Int. for a/c 00000000001",
                new BigDecimal("-1.25"), 0, "", "", "", "4111111111111111",
                "2022-07-18-10.15.30.120000", "2022-07-18-10.15.30.120000", "");
        String image = TranRecord.CODEC.encode(r);
        assertEquals(350, image.length());
        assertEquals("2022071800000001010005System    Int. for a/c 00000000001", image.substring(0, 56));
        assertEquals("0000000012N", image.substring(132, 143));
        assertEquals(r, TranRecord.CODEC.decode(image));
    }
}
