package com.carddemo.batch.codec;

import com.carddemo.batch.record.AccountRecord;
import com.carddemo.batch.record.ArrArrayRec;
import com.carddemo.batch.record.CodatecnRec;
import com.carddemo.batch.record.OutAcctRec;
import com.carddemo.batch.record.VbrcRec1;
import com.carddemo.batch.record.VbrcRec2;
import com.carddemo.batch.support.Cbact01cRun;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/** The copybook layouts and the byte-for-byte decode/encode of the FixedWidth codec. */
class FixedWidthRecordTest {

    @Test
    void layoutLengthsMatchTheCopybooks() {
        assertEquals(300, AccountRecord.LENGTH, "CVACT01Y ACCOUNT-RECORD");
        assertEquals(107, OutAcctRec.LENGTH, "OUT-ACCT-REC (JCL LRECL=107)");
        assertEquals(110, ArrArrayRec.LENGTH, "ARR-ARRAY-REC (JCL LRECL=110)");
        assertEquals(12, VbrcRec1.LENGTH, "VBRC-REC1");
        assertEquals(39, VbrcRec2.LENGTH, "VBRC-REC2");
        assertEquals(1 + 20 + 1 + 20 + 38, CodatecnRec.LENGTH, "CODATECN-REC");
    }

    @Test
    void accountRecordOffsetsMatchCvact01y() {
        assertEquals(0, AccountRecord.ACCT_ID.offset());
        assertEquals(11, AccountRecord.ACCT_ACTIVE_STATUS.offset());
        assertEquals(12, AccountRecord.ACCT_CURR_BAL.offset());
        assertEquals(24, AccountRecord.ACCT_CREDIT_LIMIT.offset());
        assertEquals(36, AccountRecord.ACCT_CASH_CREDIT_LIMIT.offset());
        assertEquals(48, AccountRecord.ACCT_OPEN_DATE.offset());
        assertEquals(58, AccountRecord.ACCT_EXPIRAION_DATE.offset());
        assertEquals(68, AccountRecord.ACCT_REISSUE_DATE.offset());
        assertEquals(78, AccountRecord.ACCT_CURR_CYC_CREDIT.offset());
        assertEquals(90, AccountRecord.ACCT_CURR_CYC_DEBIT.offset());
        assertEquals(102, AccountRecord.ACCT_ADDR_ZIP.offset());
        assertEquals(112, AccountRecord.ACCT_GROUP_ID.offset());
        assertEquals(122, AccountRecord.FILLER.offset());
        assertEquals(178, AccountRecord.FILLER.length());
    }

    @Test
    void outAcctRecPackedFieldIsSevenBytesAtOffset90() {
        assertEquals(90, OutAcctRec.OUT_ACCT_CURR_CYC_DEBIT.offset());
        assertEquals(7, OutAcctRec.OUT_ACCT_CURR_CYC_DEBIT.length());
        assertEquals(Field.Usage.PACKED, OutAcctRec.OUT_ACCT_CURR_CYC_DEBIT.usage());
        assertEquals(97, OutAcctRec.OUT_ACCT_GROUP_ID.offset());
    }

    @Test
    void occursFiveIsAFixedSizeArrayOfNineteenByteEntries() {
        assertEquals(5, ArrArrayRec.OCCURS);
        assertEquals(5, ArrArrayRec.ARR_ACCT_BAL.occurs());
        assertEquals(19, ArrArrayRec.ARR_ACCT_BAL.length(), "12 zoned + 7 packed");
        for (int i = 0; i < 5; i++) {
            Field occ = ArrArrayRec.ARR_ACCT_BAL.occurrence(i);
            assertEquals(11 + 19 * i, occ.offset(), "ARR-ACCT-BAL(" + (i + 1) + ") offset");
            assertEquals(11 + 19 * i, occ.child("ARR-ACCT-CURR-BAL").offset());
            assertEquals(11 + 19 * i + 12, occ.child("ARR-ACCT-CURR-CYC-DEBIT").offset());
        }
        assertEquals(106, ArrArrayRec.ARR_FILLER.offset());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrArrayRec.ARR_ACCT_BAL.occurrence(5));
        assertThrows(IndexOutOfBoundsException.class, () -> new ArrArrayRec().arrAcctBal(6));
    }

    @Test
    void everySampleAccountRecordRoundTripsByteForByte() {
        List<byte[]> recs = Cbact01cRun.readAccountRecords(Repo.SAMPLE_ACCTDATA);
        assertEquals(50, recs.size());
        for (byte[] raw : recs) {
            AccountRecord rec = AccountRecord.decode(raw);
            assertArrayEquals(raw, rec.encode(), "ACCOUNT-RECORD " + rec.acctId() + " decode/encode");
            AccountRecord copy = new AccountRecord();
            copy.setAcctId(rec.acctId());
            copy.setAcctActiveStatus(rec.acctActiveStatus());
            copy.setAcctCurrBal(rec.acctCurrBal());
            copy.setAcctCreditLimit(rec.acctCreditLimit());
            copy.setAcctCashCreditLimit(rec.acctCashCreditLimit());
            copy.setAcctOpenDate(rec.acctOpenDate());
            copy.setAcctExpiraionDate(rec.acctExpiraionDate());
            copy.setAcctReissueDate(rec.acctReissueDate());
            copy.setAcctCurrCycCredit(rec.acctCurrCycCredit());
            copy.setAcctCurrCycDebit(rec.acctCurrCycDebit());
            copy.setAcctAddrZip(rec.acctAddrZip());
            copy.setAcctGroupId(rec.acctGroupId());
            assertArrayEquals(raw, copy.encode(), "ACCOUNT-RECORD " + rec.acctId() + " rebuilt through typed setters");
            assertEquals(2, rec.acctCurrBal().scale());
            assertEquals(10, rec.acctOpenDate().length());
        }
    }

    @Test
    void textFieldsKeepTrailingSpacesAndAreTruncatedOrPaddedToWidth() {
        AccountRecord rec = new AccountRecord();
        rec.setAcctGroupId("AB");
        assertEquals("AB        ", rec.acctGroupId());
        rec.setAcctGroupId("ABCDEFGHIJKLMNOP");
        assertEquals("ABCDEFGHIJ", rec.acctGroupId());
        rec.setAcctGroupId("");
        assertEquals("          ", rec.acctGroupId());
        assertEquals(10, rec.acctGroupId().length());
        OutAcctRec out = new OutAcctRec();
        out.setOutAcctReissueDate("20250520");
        assertEquals("20250520  ", out.outAcctReissueDate());
    }

    @Test
    void initializeFollowsCobolRules() {
        ArrArrayRec arr = ArrArrayRec.decode(Cbact01cRun.sample().arryfileRecords().get(0));
        arr.initialize();
        byte[] b = arr.encode();
        assertEquals("00000000000", new String(b, 0, 11, StandardCharsets.ISO_8859_1), "unsigned numeric -> zeros");
        for (int i = 0; i < 5; i++) {
            assertEquals(0, arr.arrAcctBal(i + 1).arrAcctCurrBal().signum());
            assertEquals(0, arr.arrAcctBal(i + 1).arrAcctCurrCycDebit().signum());
            assertEquals("000000000000", new String(b, 11 + 19 * i, 12, StandardCharsets.ISO_8859_1),
                    "INITIALIZE writes plain zero digits (no overpunch), unlike MOVE 0 which writes ...{");
            assertEquals(0x0C, b[11 + 19 * i + 18] & 0xFF, "packed zero sign nibble C");
        }
        assertEquals("    ", arr.arrFiller(), "PIC X -> spaces");
        VbrcRec1 vb1 = new VbrcRec1();
        vb1.setVb1AcctId(7);
        vb1.setVb1AcctActiveStatus("N");
        vb1.initialize();
        assertEquals("00000000000 ", vb1.display());
    }

    @Test
    void lowValuesMirrorAnUnassignedFdRecordArea() {
        OutAcctRec out = new OutAcctRec();
        out.lowValues();
        assertArrayEquals(new byte[107], out.encode());
        assertThrows(CodecException.class, out::outAcctCurrCycDebit, "LOW-VALUES is not a valid COMP-3");
    }

    @Test
    void toMapExposesTypedValuesAndOccursList() {
        OutAcctRec out = OutAcctRec.decode(Cbact01cRun.sample().outfileRecords().get(0));
        Map<String, Object> m = out.toMap();
        assertEquals(new BigDecimal("1"), m.get("OUT-ACCT-ID"));
        assertEquals("Y", m.get("OUT-ACCT-ACTIVE-STATUS"));
        assertEquals(0, new BigDecimal("2525.00").compareTo((BigDecimal) m.get("OUT-ACCT-CURR-CYC-DEBIT")));
        assertEquals("20250520  ", m.get("OUT-ACCT-REISSUE-DATE"));
        Map<String, Object> arr = ArrArrayRec.decode(Cbact01cRun.sample().arryfileRecords().get(0)).toMap();
        assertEquals(5, ((List<?>) arr.get("ARR-ACCT-BAL")).size());
        assertEquals(List.of("ARR-ACCT-ID", "ARR-ACCT-BAL", "ARR-FILLER"), List.copyOf(arr.keySet()));
    }

    @Test
    void decodeRejectsWrongRecordLength() {
        assertThrows(CodecException.class, () -> AccountRecord.decode(new byte[299]));
        assertThrows(CodecException.class, () -> OutAcctRec.decode(new byte[108]));
    }
}
