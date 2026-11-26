package com.carddemo.programs.cbtrn03c.copybook;

import com.carddemo.programs.cbtrn03c.Fixtures;
import com.carddemo.programs.cbtrn03c.copybook.ReportLayouts.ReportNameHeader;
import com.carddemo.programs.cbtrn03c.copybook.ReportLayouts.TransactionDetailReport;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CopybookLayoutTest {

    @Test
    void cvtra05yParsesAndRoundTripsEverySampleTransaction() throws Exception {
        List<String> records = Fixtures.lines("dailytran.txt", TranRecord.LENGTH);
        for (String r : records) {
            assertEquals(r, TranRecord.parse(r).toFixedWidth());
        }
        TranRecord first = TranRecord.parse(records.get(0));
        assertEquals("0000000000683580", first.tranId());
        assertEquals("01", first.tranTypeCd());
        assertEquals(1, first.tranCatCd());
        assertEquals("POS TERM  ", first.tranSource());
        assertEquals(new BigDecimal("504.77"), first.tranAmt());
        assertEquals(800000000L, first.tranMerchantId());
        assertEquals("4859452612877065", first.tranCardNum());
        assertEquals("2022-06-10 19:27:53.000000", first.tranOrigTs());
        assertEquals(TranRecord.LENGTH, TranRecord.initial().toFixedWidth().length());
    }

    @Test
    void cvact03yParsesAndRoundTrips() throws Exception {
        for (String r : Fixtures.lines("cardxref.txt", CardXrefRecord.LENGTH)) {
            assertEquals(r, CardXrefRecord.parse(r).toFixedWidth());
        }
        CardXrefRecord x = CardXrefRecord.parse(Fixtures.lines("cardxref.txt", 50).get(0));
        assertEquals("0500024453765740", x.xrefCardNum());
        assertEquals(50L, x.xrefCustId());
        assertEquals(50L, x.xrefAcctId());
        assertEquals("00000000050", x.xrefAcctIdDisplay());
    }

    @Test
    void cvtra03yAndCvtra04yParseAndRoundTrip() throws Exception {
        for (String r : Fixtures.lines("trantype.txt", TranTypeRecord.LENGTH)) {
            assertEquals(r, TranTypeRecord.parse(r).toFixedWidth());
        }
        for (String r : Fixtures.lines("trancatg.txt", TranCatRecord.LENGTH)) {
            assertEquals(r, TranCatRecord.parse(r).toFixedWidth());
        }
        TranCatRecord c = TranCatRecord.parse(Fixtures.lines("trancatg.txt", 60).get(1));
        assertEquals("010002", c.tranCatKey().toFixedWidth());
        assertEquals("Regular Cash Advance", c.tranCatTypeDesc().trim());
        assertEquals("Purchase", TranTypeRecord.parse(Fixtures.lines("trantype.txt", 60).get(0)).tranTypeDesc().trim());
    }

    @Test
    void cvtra07yLayoutLengthsMatchTheCopybook() {
        assertEquals(115, ReportNameHeader.initial().format().length());
        assertEquals(114, TransactionDetailReport.initialized().format().length());
        assertEquals(114, ReportLayouts.TRANSACTION_HEADER_1.length());
        assertEquals(133, ReportLayouts.TRANSACTION_HEADER_2.length());
        assertEquals(112, ReportLayouts.reportPageTotals(BigDecimal.ONE).length());
        assertEquals(112, ReportLayouts.reportAccountTotals(BigDecimal.ONE).length());
        assertEquals(112, ReportLayouts.reportGrandTotals(BigDecimal.ONE).length());
    }

    @Test
    void cvtra07yHeaderAndTotalsText() {
        assertEquals("DALYREPT                              Daily Transaction Report                 "
                + "Date Range: 2022-01-01 to 2022-07-06", new ReportNameHeader("2022-01-01", "2022-07-06").format());
        assertEquals("Transaction ID   Account ID  Transaction Type   Tran Category                      "
                + "Tran Source            Amount  ", ReportLayouts.TRANSACTION_HEADER_1);
        assertEquals("Page Total " + ".".repeat(86) + "+      5,401.30",
                ReportLayouts.reportPageTotals(new BigDecimal("5401.30")));
        assertEquals("Account Total" + ".".repeat(84) + "-          1.00",
                ReportLayouts.reportAccountTotals(new BigDecimal("-1.00")));
        assertEquals("Grand Total" + ".".repeat(86) + "+    154,352.80",
                ReportLayouts.reportGrandTotals(new BigDecimal("154352.80")));
    }

    @Test
    void cvtra07yDetailLineTruncatesDescriptions() {
        TransactionDetailReport d = new TransactionDetailReport("0000000058866561", "00000000050", "01",
                "Purchase" + " ".repeat(30) + "XYZ", 1, "Regular Sales Draft" + " ".repeat(20) + "XYZ",
                "POS TERM", new BigDecimal("-183.80"));
        assertEquals("0000000058866561 00000000050 01-Purchase        0001-Regular Sales Draft           "
                + "POS TERM      -        183.80  ", d.format());
    }
}
