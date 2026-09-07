package com.carddemo.poc.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AccountRecordTest {

    private static final String FIRST_SAMPLE_RECORD =
            "00000000001" + "Y" + "00000001940{" + "00000020200{" + "00000010200{"
            + "2014-11-20" + "2025-05-20" + "2025-05-20" + "00000000000{" + "00000000000{"
            + "A000000000" + "          " + " ".repeat(178);

    @Test
    void decodesZonedDecimalMoneyFields() {
        AccountRecord rec = AccountRecord.fromImage(FIRST_SAMPLE_RECORD);
        assertEquals(1L, rec.getAcctId());
        assertEquals("Y", rec.getActiveStatus());
        assertEquals(new BigDecimal("194.00"), rec.getCurrBal());
        assertEquals(new BigDecimal("2020.00"), rec.getCreditLimit());
        assertEquals(new BigDecimal("1020.00"), rec.getCashCreditLimit());
        assertEquals("2014-11-20", rec.getOpenDate());
        assertEquals("2025-05-20", rec.getExpirationDate());
        assertEquals("2025-05-20", rec.getReissueDate());
        assertEquals(new BigDecimal("0.00"), rec.getCurrCycCredit());
        assertEquals(new BigDecimal("0.00"), rec.getCurrCycDebit());
        assertEquals("A000000000", rec.getAddrZip());
        assertEquals("          ", rec.getGroupId());
    }
}
