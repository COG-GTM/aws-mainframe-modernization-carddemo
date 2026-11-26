package com.carddemo.programs.coacct01.dto;

import com.carddemo.programs.coacct01.copybook.AccountRecord;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;

class DtoTest {

    @Test
    void requestCopySplitsFuncKeyAndFiller() {
        AccountInquiryRequest r = AccountInquiryRequest.fromMessage("INQA00000000042REST");
        assertEquals("INQA", r.func());
        assertEquals("00000000042", r.keyText());
        assertEquals(42, r.keyValue());
        assertEquals(985, r.filler().length());
        assertEquals("REST", r.filler().strip());
        assertEquals(1000, r.toMessage().length());
    }

    @Test
    void initialRequestCopyHasZeroKeyAndSpaces() {
        AccountInquiryRequest r = AccountInquiryRequest.initial();
        assertEquals("    ", r.func());
        assertEquals("00000000000", r.keyText());
    }

    @Test
    void responseIs252BytesWithLabelsAndZonedAmounts() {
        AccountRecord a = new AccountRecord(7, "N", new BigDecimal("-12.34"), new BigDecimal("500"),
                new BigDecimal("100.5"), "2020-01-01", "2025-01-01", "2024-01-01",
                new BigDecimal("1.01"), new BigDecimal("2.02"), "12345", "GOLD");
        String msg = AccountInquiryResponse.from(a).toMessage();
        assertEquals(AccountInquiryResponse.LENGTH, msg.length());
        assertEquals("ACCOUNT ID : 00000000007ACCOUNT STATUS : NBALANCE : 00000000123M"
                + "CREDIT LIMIT : 00000005000{CASH LIMIT : 00000001005{OPEN DATE : 2020-01-01"
                + "EXPR DATE : 2025-01-01REIS DATE : 2024-01-01CREDIT BAL : 00000000010A"
                + "DEBIT BAL : 00000000020BGROUP ID : GOLD      ", msg);
    }
}
