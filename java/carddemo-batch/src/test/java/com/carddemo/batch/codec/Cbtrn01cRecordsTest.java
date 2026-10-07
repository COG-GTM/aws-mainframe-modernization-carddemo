package com.carddemo.batch.codec;

import com.carddemo.batch.record.CardRecord;
import com.carddemo.batch.record.CardXrefRecord;
import com.carddemo.batch.record.CustomerRecord;
import com.carddemo.batch.record.DalytranRecord;
import com.carddemo.batch.record.TranRecord;
import com.carddemo.batch.support.Cbtrn01cRun;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

/** The CBTRN01C copybook layouts (CVTRA06Y, CVACT03Y, CVACT02Y, CVCUS01Y, CVTRA05Y) and their codec. */
class Cbtrn01cRecordsTest {

    @Test
    void layoutLengthsMatchTheCopybooks() {
        assertEquals(350, DalytranRecord.LENGTH, "CVTRA06Y DALYTRAN-RECORD");
        assertEquals(50, CardXrefRecord.LENGTH, "CVACT03Y CARD-XREF-RECORD");
        assertEquals(150, CardRecord.LENGTH, "CVACT02Y CARD-RECORD");
        assertEquals(500, CustomerRecord.LENGTH, "CVCUS01Y CUSTOMER-RECORD");
        assertEquals(350, TranRecord.LENGTH, "CVTRA05Y TRAN-RECORD");
    }

    @Test
    void dalytranOffsetsMatchCvtra06y() {
        assertEquals(0, DalytranRecord.DALYTRAN_ID.offset());
        assertEquals(16, DalytranRecord.DALYTRAN_TYPE_CD.offset());
        assertEquals(18, DalytranRecord.DALYTRAN_CAT_CD.offset());
        assertEquals(22, DalytranRecord.DALYTRAN_SOURCE.offset());
        assertEquals(32, DalytranRecord.DALYTRAN_DESC.offset());
        assertEquals(132, DalytranRecord.DALYTRAN_AMT.offset());
        assertEquals(11, DalytranRecord.DALYTRAN_AMT.length());
        assertEquals(2, DalytranRecord.DALYTRAN_AMT.scale());
        assertEquals(Field.Usage.ZONED, DalytranRecord.DALYTRAN_AMT.usage());
        assertEquals(143, DalytranRecord.DALYTRAN_MERCHANT_ID.offset());
        assertEquals(152, DalytranRecord.DALYTRAN_MERCHANT_NAME.offset());
        assertEquals(202, DalytranRecord.DALYTRAN_MERCHANT_CITY.offset());
        assertEquals(252, DalytranRecord.DALYTRAN_MERCHANT_ZIP.offset());
        assertEquals(262, DalytranRecord.DALYTRAN_CARD_NUM.offset());
        assertEquals(278, DalytranRecord.DALYTRAN_ORIG_TS.offset());
        assertEquals(304, DalytranRecord.DALYTRAN_PROC_TS.offset());
        assertEquals(330, DalytranRecord.FILLER.offset());
        assertEquals(20, DalytranRecord.FILLER.length());
        assertEquals(DalytranRecord.LAYOUT.fields().size(), TranRecord.LAYOUT.fields().size(),
                "CVTRA05Y has the same shape as CVTRA06Y");
    }

    @Test
    void keyedRecordOffsetsMatchTheCopybooks() {
        assertEquals(16, CardXrefRecord.XREF_CUST_ID.offset());
        assertEquals(25, CardXrefRecord.XREF_ACCT_ID.offset());
        assertEquals(36, CardXrefRecord.FILLER.offset());
        assertEquals(16, CardRecord.CARD_ACCT_ID.offset());
        assertEquals(27, CardRecord.CARD_CVV_CD.offset());
        assertEquals(30, CardRecord.CARD_EMBOSSED_NAME.offset());
        assertEquals(80, CardRecord.CARD_EXPIRAION_DATE.offset());
        assertEquals(90, CardRecord.CARD_ACTIVE_STATUS.offset());
        assertEquals(91, CardRecord.FILLER.offset());
        assertEquals(9, CustomerRecord.CUST_FIRST_NAME.offset());
        assertEquals(84, CustomerRecord.CUST_ADDR_LINE_1.offset());
        assertEquals(234, CustomerRecord.CUST_ADDR_STATE_CD.offset());
        assertEquals(279, CustomerRecord.CUST_SSN.offset());
        assertEquals(308, CustomerRecord.CUST_DOB_YYYY_MM_DD.offset());
        assertEquals(328, CustomerRecord.CUST_PRI_CARD_HOLDER_IND.offset());
        assertEquals(329, CustomerRecord.CUST_FICO_CREDIT_SCORE.offset());
        assertEquals(332, CustomerRecord.FILLER.offset());
        assertEquals(168, CustomerRecord.FILLER.length());
    }

    @Test
    void dalytranDecodesTheFirstSampleRecordExactly() {
        List<byte[]> recs = Cbtrn01cRun.readRecords(Repo.SAMPLE_DAILYTRAN, DalytranRecord.LENGTH);
        DalytranRecord r = DalytranRecord.decode(recs.get(0));
        assertEquals("0000000000683580", r.dalytranId());
        assertEquals("01", r.dalytranTypeCd());
        assertEquals(1L, r.dalytranCatCd());
        assertEquals("POS TERM  ", r.dalytranSource(), "trailing spaces preserved");
        assertEquals(0, new BigDecimal("504.77").compareTo(r.dalytranAmt()));
        assertEquals(2, r.dalytranAmt().scale());
        assertEquals(800000000L, r.dalytranMerchantId());
        assertEquals("72112     ", r.dalytranMerchantZip());
        assertEquals("4859452612877065", r.dalytranCardNum());
        assertEquals("2022-06-10 19:27:53.000000", r.dalytranOrigTs(), "timestamp kept as text");
        assertEquals(" ".repeat(26), r.dalytranProcTs(), "unset timestamp is 26 spaces");
        assertArrayEquals(recs.get(0), r.encode(), "encode round-trips the raw bytes incl. FILLER");
        assertEquals(new String(recs.get(0), StandardCharsets.ISO_8859_1), r.display(), "DISPLAY DALYTRAN-RECORD is the raw 350 bytes");
    }

    @Test
    void signedAmountUsesZonedOverpunchAndDisplaysWithTrailingSign() {
        DalytranRecord r = new DalytranRecord();
        r.setDalytranAmt(new BigDecimal("-919.00"));
        assertEquals("0000009190}", new String(r.encode(), 132, 11, StandardCharsets.ISO_8859_1), "negative zero overpunch");
        assertEquals("00000091900-", r.display(DalytranRecord.DALYTRAN_AMT));
        assertEquals(0, new BigDecimal("-919.00").compareTo(r.dalytranAmt()));
        assertEquals(2, r.dalytranAmt().scale());
        r.setDalytranAmt(new BigDecimal("504.77"));
        assertEquals("0000005047G", new String(r.encode(), 132, 11, StandardCharsets.ISO_8859_1), "positive 7 overpunch");
        assertEquals("00000050477+", r.display(DalytranRecord.DALYTRAN_AMT));
    }

    @Test
    void unsignedKeysDisplayAsZeroPaddedDigits() {
        CardXrefRecord x = new CardXrefRecord();
        x.setXrefCardNum("4859452612877065");
        x.setXrefCustId(7);
        x.setXrefAcctId(7);
        assertEquals("00000000007", x.display(CardXrefRecord.XREF_ACCT_ID));
        assertEquals("000000007", x.display(CardXrefRecord.XREF_CUST_ID));
        assertEquals("4859452612877065" + "000000007" + "00000000007" + " ".repeat(14), x.display());
        CustomerRecord c = new CustomerRecord();
        c.setCustId(50);
        c.setCustFicoCreditScore(750);
        assertEquals("000000050", c.display(CustomerRecord.CUST_ID));
        assertEquals(750L, c.custFicoCreditScore());
        CardRecord card = new CardRecord();
        card.setCardCvvCd(42);
        card.setCardActiveStatus("Y");
        assertEquals("042", card.display(CardRecord.CARD_CVV_CD));
        assertEquals("Y", card.cardActiveStatus());
    }

    @Test
    void moveFromReplacesTheWholeRecordAreaAndInitializeResetsIt() {
        List<byte[]> recs = Cbtrn01cRun.readRecords(Repo.SAMPLE_DAILYTRAN, DalytranRecord.LENGTH);
        DalytranRecord r = new DalytranRecord();
        r.moveFrom(recs.get(1));
        assertArrayEquals(recs.get(1), r.encode());
        r.moveFrom(recs.get(0));
        assertArrayEquals(recs.get(0), r.encode(), "READ INTO overwrites the previous record completely");
        r.initialize();
        assertEquals(" ".repeat(16), r.dalytranId());
        assertEquals(0, BigDecimal.ZERO.compareTo(r.dalytranAmt()));
        TranRecord t = new TranRecord();
        t.moveFrom(recs.get(0));
        assertEquals("0000000000683580", t.tranId(), "CVTRA05Y shares the CVTRA06Y layout");
        assertEquals(0, new BigDecimal("504.77").compareTo(t.tranAmt()));
    }
}
