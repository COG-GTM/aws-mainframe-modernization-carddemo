package com.carddemo.programs.cocrdupc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cocrdupc.CardUpdateProgramArea.ChangeAction;
import com.carddemo.programs.cocrdupc.copybook.AbendData;
import com.carddemo.programs.cocrdupc.copybook.AidKey;
import com.carddemo.programs.cocrdupc.copybook.CardDemoCommarea;
import com.carddemo.programs.cocrdupc.copybook.CardRecord;
import com.carddemo.programs.cocrdupc.copybook.CcWorkArea;
import com.carddemo.programs.cocrdupc.copybook.CcWorkArea.CcardAid;
import com.carddemo.programs.cocrdupc.copybook.CcrdupaMap;
import com.carddemo.programs.cocrdupc.copybook.CommonMessages;
import com.carddemo.programs.cocrdupc.copybook.CustomerRecord;
import com.carddemo.programs.cocrdupc.copybook.DateTimeFields;
import com.carddemo.programs.cocrdupc.copybook.ScreenTitles;
import com.carddemo.programs.cocrdupc.copybook.SecUserData;
import com.carddemo.programs.cocrdupc.copybook.StorePfKey;

import java.time.LocalDateTime;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CopybookTest {

    @Nested
    class Cocom01y {
        @Test
        void layoutIs160BytesAndRoundTrips() {
            CardDemoCommarea c = CardDemoCommarea.initialized();
            c.setFromTranid("CCLI");
            c.setFromProgram("COCRDLIC");
            c.setUserId("USER0001");
            c.setUsrtypAdmin();
            c.setPgmReenter();
            c.setCustId(123456789);
            c.setAcctId(50);
            c.setAcctStatus("Y");
            c.setCardNum(500024453765740L);
            c.setLastMap("CCRDSLA");
            c.setLastMapset("COCRDLI");
            String raw = c.encode();
            assertEquals(CardDemoCommarea.LENGTH, raw.length());
            assertEquals("00000000050", raw.substring(118, 129));
            assertEquals("0500024453765740", raw.substring(130, 146));
            assertEquals(raw, CardDemoCommarea.decode(raw).encode());
            assertTrue(c.isUsrtypAdmin());
            assertTrue(c.isPgmReenter());
        }

        @Test
        void initializeSetsSpacesAndZeros() {
            String raw = CardDemoCommarea.initialized().encode();
            assertEquals(" ".repeat(33) + "0" + "0".repeat(9), raw.substring(0, 43));
            assertTrue(CardDemoCommarea.initialized().isPgmEnter());
        }

        @Test
        void numericFieldsTruncateHighOrderDigits() {
            CardDemoCommarea c = CardDemoCommarea.initialized();
            c.setAcctId(123456789012L);
            assertEquals(23456789012L, c.acctId());
        }
    }

    @Nested
    class Cvcrd01y {
        @Test
        void aidValuesAndRedefines() {
            CcWorkArea cc = CcWorkArea.initialized();
            assertEquals("     ", cc.ccardAid());
            cc.setCcardAid(CcardAid.PA1);
            assertEquals("PA1  ", cc.ccardAid());
            cc.setCcAcctIdN(50);
            assertEquals("00000000050", cc.ccAcctId());
            cc.setCcCardNum("0500024453765740");
            assertEquals(500024453765740L, cc.ccCardNumN());
            cc.setCcCustId("000000001");
            assertEquals(1, cc.ccCustIdN());
        }
    }

    @Nested
    class Csstrpfy {
        @ParameterizedTest
        @CsvSource({"ENTER,ENTER", "CLEAR,CLEAR", "PA1,PA1", "PA2,PA2", "PF3,PFK03", "PF5,PFK05",
                "PF12,PFK12", "PF13,PFK01", "PF15,PFK03", "PF17,PFK05", "PF24,PFK12"})
        void mapsEibaidToCcardAid(AidKey key, CcardAid expected) {
            CcWorkArea cc = CcWorkArea.initialized();
            StorePfKey.yyyyStorePfkey(key, cc);
            assertTrue(cc.is(expected));
        }

        @Test
        void unmappedAidLeavesCcardAidUnchanged() {
            CcWorkArea cc = CcWorkArea.initialized();
            StorePfKey.yyyyStorePfkey(AidKey.PA3, cc);
            assertEquals("     ", cc.ccardAid());
        }
    }

    @Nested
    class Cvact02y {
        @Test
        void parsesAndFormatsFixedWidth150() {
            String line = "0500024453765740" + "00000000050" + "747" + Cobol.alnum("Aniya Von", 50)
                    + "2023-03-09" + "Y" + " ".repeat(59);
            CardRecord r = CardRecord.parse(line);
            assertEquals(50, r.cardAcctId());
            assertEquals(747, r.cardCvvCd());
            assertEquals("2023-03-09", r.cardExpiraionDate());
            assertEquals(CardRecord.LENGTH, r.toFixedWidth().length());
            assertEquals(line, r.toFixedWidth());
            assertEquals("007", new CardRecord("1", 1, 7, "", "", "", "").cardCvvCdDisplay());
        }
    }

    @Nested
    class OtherCopybooks {
        @Test
        void customerRecordIs500Bytes() {
            CustomerRecord c = CustomerRecord.parse("000000001" + Cobol.alnum("Immanuel", 25));
            assertEquals(1, c.custId());
            assertEquals("Immanuel", c.custFirstName().strip());
            assertEquals(CustomerRecord.LENGTH, c.toFixedWidth().length());
            assertEquals(c, CustomerRecord.parse(c.toFixedWidth()));
        }

        @Test
        void secUserDataIs80Bytes() {
            SecUserData u = SecUserData.parse("ADMIN001");
            assertEquals("ADMIN001", u.secUsrId());
            assertEquals(SecUserData.LENGTH, u.toFixedWidth().length());
        }

        @Test
        void abendDataIs134BytesOfSpaces() {
            AbendData a = new AbendData();
            assertEquals(" ".repeat(AbendData.LENGTH), a.encode());
        }

        @Test
        void titlesAndCommonMessagesKeepTheirPicLengths() {
            assertEquals(40, ScreenTitles.CCDA_TITLE01.length());
            assertEquals(40, ScreenTitles.CCDA_TITLE02.length());
            assertEquals(40, ScreenTitles.CCDA_THANK_YOU.length());
            assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
            assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        }

        @Test
        void currentDateFormats() {
            DateTimeFields d = new DateTimeFields();
            d.setCurrentDate(LocalDateTime.of(2026, 11, 26, 7, 5, 9, 120_000_000));
            assertEquals("11/26/26", d.wsCurdateMmDdYy());
            assertEquals("07:05:09", d.wsCurtimeHhMmSs());
            assertEquals("20261126", d.wsCurdateN());
            assertEquals("07050912", d.wsCurtimeN());
            assertEquals("2026-11-26 07:05:09.000012", d.wsTimestamp());
        }

        @Test
        void symbolicMapFieldLengthsMatchCocrdup() {
            CcrdupaMap m = new CcrdupaMap();
            assertEquals(11, m.get(CcrdupaMap.Field.ACCTSID).data().length());
            assertEquals(80, m.get(CcrdupaMap.Field.ERRMSG).data().length());
            assertTrue(Cobol.isLowValues(m.get(CcrdupaMap.Field.CRDNAME).data()));
        }
    }

    @Nested
    class ProgramCommarea {
        @Test
        void layoutIs329BytesAndRoundTrips() {
            CardUpdateProgramArea a = CardUpdateProgramArea.uninitialized();
            a.initialize();
            a.set(ChangeAction.CHANGES_OK_NOT_CONFIRMED);
            a.oldDetails().setAcctId("00000000050");
            a.newDetails().setCrdName("ANIYA VON");
            String raw = a.encode();
            assertEquals(CardUpdateProgramArea.LENGTH, raw.length());
            assertEquals('N', raw.charAt(0));
            assertEquals(raw, CardUpdateProgramArea.decode(raw).encode());
        }

        @Test
        void changeAction88Levels() {
            CardUpdateProgramArea a = CardUpdateProgramArea.uninitialized();
            assertTrue(a.isDetailsNotFetched());
            a.initialize();
            assertEquals(' ', a.changeAction());
            assertTrue(a.isDetailsNotFetched());
            for (char c : "ENCLF".toCharArray()) {
                a.setChangeAction(c);
                assertTrue(a.isChangesMade());
            }
            a.set(ChangeAction.CHANGES_OKAYED_LOCK_ERROR);
            assertTrue(a.isChangesFailed());
            a.set(ChangeAction.CHANGES_OKAYED_BUT_FAILED);
            assertTrue(a.isChangesFailed());
            a.set(ChangeAction.SHOW_DETAILS);
            assertFalse(a.isChangesMade());
        }

        @Test
        void initializeLeavesCardUpdateFillerUntouched() {
            CardUpdateProgramArea a = CardUpdateProgramArea.uninitialized();
            a.initialize();
            assertTrue(Cobol.isLowValues(a.cardUpdateRecord().filler()));
            assertEquals(" ".repeat(16), a.cardUpdateRecord().num());
        }
    }
}
