package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.AbendData;
import com.carddemo.programs.cotrtupc.copybook.CarddemoCommarea;
import com.carddemo.programs.cotrtupc.copybook.CcWorkArea;
import com.carddemo.programs.cotrtupc.copybook.CommonMessages;
import com.carddemo.programs.cotrtupc.copybook.CtrtupaMap;
import com.carddemo.programs.cotrtupc.copybook.DateEditArea;
import com.carddemo.programs.cotrtupc.copybook.DateTimeArea;
import com.carddemo.programs.cotrtupc.copybook.DclTransactionType;
import com.carddemo.programs.cotrtupc.copybook.ScreenTitles;
import com.carddemo.programs.cotrtupc.copybook.SecUserData;
import com.carddemo.programs.cotrtupc.copybook.Sqlca;
import com.carddemo.programs.cotrtupc.copybook.TtypUpdateRecord;
import com.carddemo.programs.cotrtupc.support.CobolText;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Record lengths, offsets and 88-levels of the translated copybooks. */
class CopybookLayoutTest {

    @Test
    void recordLengthsMatchCopybooks() {
        assertEquals(160, new CarddemoCommarea().length());
        assertEquals(105, new TtupProgCommarea().length());
        assertEquals(213, new CcWorkArea().length());
        assertEquals(134, new AbendData().length());
        assertEquals(80, new SecUserData().length());
        assertEquals(60, new TtypUpdateRecord().length());
        assertEquals(40, ScreenTitles.CCDA_TITLE01.length());
        assertEquals(40, ScreenTitles.CCDA_TITLE02.length());
        assertEquals(40, ScreenTitles.CCDA_THANK_YOU.length());
        assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        // COTRTUP: 12 + sum(7 + size) over 15 fields
        assertEquals(12 + 15 * 7 + (4 + 40 + 8 + 8 + 40 + 8 + 2 + 50 + 45 + 78 + 21 + 9 + 8 + 6 + 10),
                new CtrtupaMap().byteLength());
    }

    @Test
    void commareaOffsetsAndInitialize() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.initialize();
        c.fromTranid("CTLI");
        c.fromProgram("COTRTLIC");
        c.userId("USER0001");
        c.setUsrtypUser();
        c.setPgmReenter();
        c.custId(123456789);
        c.acctId(42);
        c.cardNum(4111111111111111L);
        c.lastMap("CTRTLIA");
        c.lastMapset("COTRTLI");
        String f = c.toFixed();
        assertEquals("CTLICOTRTLIC", f.substring(0, 12));
        assertEquals("USER0001U1123456789", f.substring(24, 43));
        assertEquals("00000000042", f.substring(118, 129));
        assertEquals("4111111111111111", f.substring(130, 146));
        assertEquals("CTRTLIACOTRTLI", f.substring(146, 160));
        assertTrue(c.isUsrtypUser());
        assertFalse(c.isUsrtypAdmin());
        assertTrue(c.isPgmReenter());

        CarddemoCommarea fresh = new CarddemoCommarea();
        assertFalse(fresh.isPgmEnter());
        assertFalse(fresh.isPgmReenter());
        fresh.initialize();
        assertTrue(fresh.isPgmEnter());
        assertEquals(0L, fresh.acctId());
    }

    @Test
    void changeAction88Levels() {
        TtupProgCommarea p = new TtupProgCommarea();
        assertTrue(p.isDetailsNotFetched());
        p.initialize();
        assertTrue(p.isDetailsNotFetched());
        for (char c : new char[] {'9', '8', '7', '6'}) {
            p.changeAction(c);
            assertTrue(p.isDeleteInProgress());
        }
        for (char c : new char[] {'E', 'N', 'L', 'F'}) {
            p.changeAction(c);
            assertTrue(p.isChangesMade());
        }
        p.changeAction('L');
        assertTrue(p.isChangesFailed());
        assertTrue(p.isChangesOkayedLockError());
        p.changeAction('C');
        assertFalse(p.isChangesMade());
        assertTrue(p.isChangesOkayedAndDone());
        p.setDetailsNotFetched();
        assertEquals(CobolText.LOW_VALUE, p.changeAction());
    }

    @Test
    void ccardAidValues() {
        CcWorkArea cc = new CcWorkArea();
        cc.initialize();
        cc.setAid(CcWorkArea.CcardAid.PA1);
        assertEquals("PA1  ", cc.aid());
        assertTrue(cc.is(CcWorkArea.CcardAid.PA1));
        assertFalse(cc.isReturnMsgOff());
    }

    @Test
    void sqlerrmGroupCarriesBinaryLength() {
        Sqlca s = Sqlca.of(-532, "TRC_TYPE_CODE", "23504");
        String g = s.sqlerrmGroup();
        assertEquals(72, g.length());
        assertEquals(0, g.charAt(0));
        assertEquals(13, g.charAt(1));
        assertEquals("TRC_TYPE_CODE", g.substring(2, 15));
    }

    @Test
    void dclVarcharUsesLength() {
        DclTransactionType d = new DclTransactionType();
        d.trDescriptionText("PURCHASE");
        d.trDescriptionLen = 8;
        assertEquals("PURCHASE", d.varcharValue());
        d.trDescriptionLen = 50;
        assertEquals(CobolText.pic("PURCHASE", 50), d.varcharValue());
    }

    @Test
    void dateTimeAreaFormats() {
        DateTimeArea dt = new DateTimeArea();
        dt.moveCurrentDate(LocalDateTime.of(2024, 2, 29, 23, 59, 58, 120_000_000));
        assertEquals("20240229", dt.wsCurdateN());
        assertEquals("23595812", dt.wsCurtimeN());
    }

    @Test
    void dateEdit88Levels() {
        DateEditArea d = new DateEditArea();
        d.wsEditDateCc = "20";
        d.wsEditDateMm = "02";
        d.wsEditDateDd = "29";
        assertTrue(d.isThisCentury());
        assertTrue(d.isFebruary());
        assertTrue(d.isValidMonth());
        assertTrue(d.isDay29());
        assertFalse(d.isValidFebDay());
        d.wsEditYearFlg = 'B';
        assertTrue(d.isYearBlank());
    }
}
