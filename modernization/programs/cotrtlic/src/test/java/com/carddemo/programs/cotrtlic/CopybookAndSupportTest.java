package com.carddemo.programs.cotrtlic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cotrtlic.copybook.CardDemoCommarea;
import com.carddemo.programs.cotrtlic.copybook.CardRecord;
import com.carddemo.programs.cotrtlic.copybook.CcWorkArea;
import com.carddemo.programs.cotrtlic.copybook.CcWorkArea.CcardAid;
import com.carddemo.programs.cotrtlic.copybook.DateTimeWork;
import com.carddemo.programs.cotrtlic.copybook.Db2CommonVars;
import com.carddemo.programs.cotrtlic.copybook.DclTransactionType;
import com.carddemo.programs.cotrtlic.copybook.Dfhaid;
import com.carddemo.programs.cotrtlic.copybook.PfKeyStore;
import com.carddemo.programs.cotrtlic.copybook.SecUserData;
import com.carddemo.programs.cotrtlic.copybook.Sqlca;
import com.carddemo.programs.cotrtlic.gateway.Dsntiac;
import com.carddemo.programs.cotrtlic.gateway.InMemoryDsntiac;
import com.carddemo.programs.cotrtlic.support.Cob;
import java.time.LocalDateTime;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.api.Test;

class CopybookAndSupportTest {

    @ParameterizedTest
    @CsvSource({"-911,' -911'", "100,'  100'", "0,'    0'", "-532,' -532'", "-12345,'-2345'"})
    void dispSqlcodeIsFloatingMinusEdited(int sqlcode, String expected) {
        assertEquals(expected, Cob.editFloatingMinus5(sqlcode));
    }

    @Test
    void trimRemovesSpacesOnlyAndComparisonPadsWithSpaces() {
        assertEquals("a b", Cob.trim("  a b  "));
        assertEquals("\u0000a", Cob.trim(" \u0000a "));
        assertEquals("", Cob.trim("   "));
        assertTrue(Cob.eq("*", "*      "));
        assertFalse(Cob.eq("*", "* x"));
        assertTrue(Cob.isNumeric("07"));
        assertFalse(Cob.isNumeric("7 "));
    }

    @Test
    void numericStoresTruncateHighOrderDigits() {
        assertEquals(0, Cob.truncUnsigned(10, 1), "ADD 1 to PIC 9(1) holding 9");
        assertEquals(1, Cob.truncUnsigned(-1, 1), "unsigned field drops the sign");
        assertEquals(-2345, Cob.truncSigned(-12345, 4));
    }

    @Test
    void stringIntoLeavesTailUntouched() {
        assertEquals("%ab%xyz", Cob.stringInto("1234xyz", "%", "ab", "%"));
    }

    @Test
    void ebcdicCollationOrdersSpacesBeforeDigits() {
        assertTrue(Cob.compareEbcdic("  ", "01") < 0);
        assertTrue(Cob.compareEbcdic("\u0000\u0000", "  ") < 0);
        assertTrue(Cob.compareEbcdic("A1", "01") < 0, "letters sort before digits in EBCDIC");
    }

    @ParameterizedTest
    @CsvSource({"DFHENTER,ENTER", "DFHCLEAR,CLEAR", "DFHPA1,PA1", "DFHPF3,PFK03", "DFHPF15,PFK03",
        "DFHPF10,PFK10", "DFHPF22,PFK10", "DFHPF24,PFK12"})
    void storePfKeyMapsAidsAndFoldsUpperFunctionKeys(Dfhaid aid, CcardAid expected) {
        CcWorkArea w = new CcWorkArea();
        PfKeyStore.storePfKey(aid, w);
        assertTrue(w.is(expected));
    }

    @Test
    void storePfKeyLeavesUnmappedAidUntouched() {
        CcWorkArea w = new CcWorkArea();
        PfKeyStore.storePfKey(Dfhaid.DFHPA3, w);
        assertEquals("     ", w.ccardAid);
    }

    @Test
    void cardDemoCommareaRoundTripsThrough160ByteLayout() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranid = "CTLI";
        c.fromProgram = "COTRTLIC";
        c.userId = "ADMIN001";
        c.setUsrtypAdmin();
        c.setPgmReenter();
        c.custId = 123456789L;
        c.acctId = 12345678901L;
        c.cardNum = 4111111111111111L;
        c.lastMap = "CTRTLIA";
        String fixed = c.toFixed();
        assertEquals(CardDemoCommarea.LENGTH, fixed.length());
        CardDemoCommarea back = CardDemoCommarea.fromFixed(fixed);
        assertEquals(fixed, back.toFixed());
        assertTrue(back.isPgmReenter());
        assertEquals(4111111111111111L, back.cardNum);
    }

    @Test
    void dateTimeFormatsAsMmDdYyAndHhMmSs() {
        DateTimeWork d = new DateTimeWork();
        d.setCurrentDate(LocalDateTime.of(1999, 12, 31, 23, 59, 58, 990_000_000));
        assertEquals(19991231, d.curdateN());
        assertEquals(23595899, d.curtimeN());
        assertEquals("1999-12-31 23:59:58.990000", d.timestamp());
    }

    @Test
    void db2CommonVarsInitializeZeroesMessageLength() {
        Db2CommonVars v = new Db2CommonVars();
        assertEquals(720, v.dsntiacMesgLen);
        v.initialize();
        assertEquals(0, v.dsntiacMesgLen);
        assertFalse(v.isDb2Ok());
        assertFalse(v.isDb2Error());
        v.setDsntiacErrCd(12);
        assertEquals("          12", v.dsntiacError());
    }

    @Test
    void inMemoryDsntiacFollowsDsntiarReturnCodes() {
        Sqlca sqlca = new Sqlca();
        sqlca.set(-911, "00C90088", "40001");
        InMemoryDsntiac d = new InMemoryDsntiac();
        assertEquals(8, d.format(sqlca, 720, 10).returnCode());
        Dsntiac.Result small = d.format(sqlca, 0, 72);
        assertEquals(12, small.returnCode());
        assertNull(small.text());
        Dsntiac.Result ok = d.format(sqlca, 720, 72);
        assertEquals(0, ok.returnCode());
        assertTrue(ok.text().startsWith("DSNT408I SQLCODE = -911"));
    }

    @Test
    void dclTransactionTypeVarcharHandling() {
        DclTransactionType init = DclTransactionType.initialized();
        assertEquals("  ", init.trType());
        DclTransactionType f = DclTransactionType.fetched("1", "Purchase");
        assertEquals("1 ", f.trType());
        assertEquals(8, f.descriptionLen());
        assertEquals(Cob.move("Purchase", 50), f.descriptionText());
        assertEquals("Purchase", f.varcharValue());
    }

    @Test
    void unusedCopybooksRoundTrip() {
        String user = Cob.move("ADMIN001Alice", 8 + 20) + Cob.move("Smith", 20) + "PASSWORD" + "A" + Cob.spaces(23);
        assertEquals(user, SecUserData.fromFixed(user).toFixed());
        CardRecord card = new CardRecord("4111111111111111", 1L, 123, "ALICE", "2025-12-31", "Y");
        assertEquals(CardRecord.LENGTH, card.toFixed().length());
        assertEquals(card.toFixed(), CardRecord.fromFixed(card.toFixed()).toFixed());
        assertEquals(1L, CardRecord.fromFixed(card.toFixed()).acctId());
    }
}
