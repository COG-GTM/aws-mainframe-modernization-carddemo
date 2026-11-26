package com.carddemo.programs.cocrdlic;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.cocrdlic.copybook.CardDemoCommarea;
import com.carddemo.programs.cocrdlic.copybook.CardRecord;
import com.carddemo.programs.cocrdlic.copybook.CardWorkAreas;
import com.carddemo.programs.cocrdlic.copybook.CardWorkAreas.CcardAid;
import com.carddemo.programs.cocrdlic.copybook.CommonMessages;
import com.carddemo.programs.cocrdlic.copybook.DateTimeFields;
import com.carddemo.programs.cocrdlic.copybook.EibAid;
import com.carddemo.programs.cocrdlic.copybook.PfKeyStore;
import com.carddemo.programs.cocrdlic.copybook.Pic;
import com.carddemo.programs.cocrdlic.copybook.ScreenTitles;
import com.carddemo.programs.cocrdlic.copybook.SecUserData;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;

class CopybookTest {

    @Test
    void storePfKeyMapsEnterClearPaAndFoldsPf13To24() {
        assertEquals(CcardAid.ENTER, PfKeyStore.storePfKey(EibAid.ENTER, CcardAid.NONE));
        assertEquals(CcardAid.CLEAR, PfKeyStore.storePfKey(EibAid.CLEAR, CcardAid.NONE));
        assertEquals(CcardAid.PA1, PfKeyStore.storePfKey(EibAid.PA1, CcardAid.NONE));
        assertEquals(CcardAid.PA2, PfKeyStore.storePfKey(EibAid.PA2, CcardAid.NONE));
        assertEquals(CcardAid.PFK01, PfKeyStore.storePfKey(EibAid.PF13, CcardAid.NONE));
        assertEquals(CcardAid.PFK07, PfKeyStore.storePfKey(EibAid.PF19, CcardAid.NONE));
        assertEquals(CcardAid.PFK08, PfKeyStore.storePfKey(EibAid.PF20, CcardAid.NONE));
        assertEquals(CcardAid.PFK12, PfKeyStore.storePfKey(EibAid.PF24, CcardAid.NONE));
        assertEquals(CcardAid.NONE, PfKeyStore.storePfKey(EibAid.PA3, CcardAid.NONE));
        assertEquals("PA1  ", CcardAid.PA1.value);
    }

    @Test
    void picMoveSemantics() {
        assertEquals("AB   ", Pic.x("AB", 5));
        assertEquals("ABCDE", Pic.x("ABCDEFG", 5));
        assertEquals("045", Pic.nine(12045, 3), "high-order truncation");
        assertEquals("007", Pic.nine(-7, 3), "unsigned field drops the sign");
        assertEquals(1, Pic.truncUnsigned(-1, 1));
        assertEquals(0, Pic.truncUnsigned(10, 1));
        assertEquals(1200, Pic.toUnsigned("12 0", 4));
        assertTrue(Pic.isNumeric("0123"));
        assertFalse(Pic.isNumeric("012 "));
        assertFalse(Pic.isNumeric(Pic.lowValues(3)));
        assertTrue(Pic.isZeros("000"));
        assertFalse(Pic.isZeros("00 "));
    }

    @Test
    void cardRecordFixedWidthRoundTripAndPicLimits() {
        CardRecord c = new CardRecord("4000000000000001", 123L, 7, "NAME", "2025-01-31", 'Y', "");
        String image = c.toFixedWidth();
        assertEquals(CardRecord.RECORD_LENGTH, image.length());
        assertEquals("4000000000000001" + "00000000123" + "007", image.substring(0, 30));
        assertEquals('Y', image.charAt(90));
        assertEquals(c, CardRecord.parse(image));
        assertEquals(1, new CardRecord("1", 100_000_000_001L, 1003, "", "", 'N', "").cardAcctId(),
                "PIC 9(11) keeps the low-order 11 digits");
        assertEquals("00000000123", c.cardAcctIdDisplay());
    }

    @Test
    void carddemoCommareaIs160BytesAndRoundTrips() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.setFromTranid("CCLI");
        c.setFromProgram("COCRDLIC");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmReenter();
        c.setCustId(123456789);
        c.setAcctId(42);
        c.setCardNum(4000000000000001L);
        c.setLastMap("CCRDLIA");
        String image = c.toFixedWidth();
        assertEquals(CardDemoCommarea.LENGTH, image.length());
        CardDemoCommarea back = CardDemoCommarea.fromFixedWidth(image);
        assertEquals(image, back.toFixedWidth());
        assertTrue(back.isUserTypeAdmin());
        assertFalse(back.isUserTypeUser());
        assertTrue(back.isPgmReenter());
        assertEquals("00000000042", image.substring(118, 129));
    }

    @Test
    void thisProgCommareaIs254Bytes() {
        ThisProgCommarea s = new ThisProgCommarea();
        s.setRow(1, "00000000001", "4000000000000001", 'Y');
        s.setNextPageExists();
        s.setScreenNum(3);
        String image = s.toFixedWidth();
        assertEquals(ThisProgCommarea.LENGTH, image.length());
        ThisProgCommarea back = ThisProgCommarea.fromFixedWidth(image);
        assertEquals(image, back.toFixedWidth());
        assertEquals(3, back.screenNum());
        assertTrue(back.isNextPageExists());
        assertEquals("4000000000000001", back.rowCardNum(1));
    }

    @Test
    void workAreasInitialiseToSpaces() {
        CardWorkAreas w = new CardWorkAreas();
        assertEquals(CcardAid.NONE, w.ccardAid);
        assertEquals(75, w.ccardErrorMsg.length());
        assertFalse(w.isReturnMsgOff());
        w.ccAcctId = "00000000123";
        assertEquals(123, w.ccAcctIdN());
    }

    @Test
    void dateTimeFormats() {
        DateTimeFields d = new DateTimeFields();
        d.setFromCurrentDate(LocalDateTime.of(2024, 3, 7, 9, 5, 4, 120_000_000));
        assertEquals("03/07/24", d.curdateMmDdYy());
        assertEquals("09:05:04", d.curtimeHhMmSs());
        assertEquals("20240307", d.curdateN());
        assertEquals("2024-03-07 09:05:04.120000", d.timestamp());
    }

    @Test
    void constantCopybooks() {
        assertEquals(40, ScreenTitles.CCDA_TITLE01.length());
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        assertEquals(80, new SecUserData("U1", "F", "L", "P", 'A', "").toFixedWidth().length());
    }
}
