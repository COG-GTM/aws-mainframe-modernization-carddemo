package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.CtrtupaMap;
import com.carddemo.programs.cotrtupc.copybook.Dfhaid;
import com.carddemo.programs.cotrtupc.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtupc.support.CobolText;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** 1000/1200/2000 decision flow, 9000 read, 9600/9700 write and 9800 delete happy paths. */
class CotrtupcMaintenanceTest {

    @Test
    void fetchShowsDetailsWithDescriptionUnprotected() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");

        CtrtupaMap s = t.last.screen();
        assertEquals('S', t.state());
        assertEquals("01", s.trtypcd.data);
        assertEquals(CobolText.pic("PURCHASE", 50), s.trtydsc.data);
        // 3250: SHOW-DETAILS shares the WHEN with PROMPT-FOR-SEARCH-KEYS
        assertEquals("Enter transaction type to be maintained", t.info());
        assertEquals("", t.error());
        assertEquals(Dfhbmsca.DFHBMPRF, s.trtypcd.attribute);
        assertEquals(Dfhbmsca.DFHBMFSE, s.trtydsc.attribute);
        assertEquals("TRTYDSC", s.cursorField());
        assertEquals(Dfhbmsca.DFHBMASB, s.fkey04.attribute);
        assertEquals(CobolText.LOW_VALUE, s.fkey05.attribute);
        assertEquals(Dfhbmsca.DFHBMASB, s.fkey12.attribute);
    }

    @Test
    void singleDigitKeyIsNormalisedToTwoDigits() {
        Terminal t = Terminal.withSeedData().start();
        t.show("5");
        assertEquals('S', t.state());
        assertEquals("05", t.last.screen().trtypcd.data);
        assertEquals(CobolText.pic("REFUND", 50), t.last.screen().trtydsc.data);
    }

    @Test
    void leadingSpaceAndSignedKeysAreNormalised() {
        Terminal t = Terminal.withSeedData().start();
        t.show(" 3");
        assertEquals("03", t.last.screen().trtypcd.data);

        Terminal t2 = Terminal.withSeedData().start();
        t2.show("-2");   // unsigned PIC 9(02) drops the sign
        assertEquals("02", t2.last.screen().trtypcd.data);
    }

    @Test
    void fractionalKeyTruncatesToZeroAndIsNotFound() {
        Terminal t = Terminal.withSeedData().start();
        t.show(".5");    // NUMVAL = 0.5 (not zero) -> PIC 9(02) = 00
        assertEquals('X', t.state());
        assertEquals("00", t.last.screen().trtypcd.data);
        assertEquals("No record found for this key in database", t.error());
    }

    @Test
    void updateDescriptionValidateThenCommit() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHENTER, "01", "Purchase Retail");

        assertEquals('N', t.state());
        assertEquals("Changes validated.Press F5 to save", t.info());
        CtrtupaMap s = t.last.screen();
        assertEquals(CobolText.pic("Purchase Retail", 50), s.trtydsc.data);
        assertEquals(Dfhbmsca.DFHBMPRF, s.trtydsc.attribute);
        assertEquals("TRTYDSC", s.cursorField());
        assertEquals(Dfhbmsca.DFHBMASB, s.fkey05.attribute);
        assertEquals(Dfhbmsca.DFHBMASB, s.fkey12.attribute);
        assertEquals(CobolText.LOW_VALUE, s.fkey04.attribute);

        t.pressResending(Dfhaid.DFHPF5);

        assertEquals('C', t.state());
        assertEquals("Changes committed to database", t.info());
        assertEquals("TRTYPCD", t.last.screen().cursorField());
        // 9600 sets DCL-TR-DESCRIPTION-LEN = LENGTH OF TTUP-NEW-TTYP-TYPE-DESC (50)
        assertEquals(CobolText.pic("Purchase Retail", 50), t.db.rows().get("01"));
        assertEquals(1, t.db.syncpoints());
        assertEquals(java.util.List.of("SELECT", "UPDATE", "SYNCPOINT"), t.db.log());
    }

    @Test
    void enterAfterCommitStartsANewSearch() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHENTER, "01", "Purchase Retail");
        t.pressResending(Dfhaid.DFHPF5);

        t.press(Dfhaid.DFHENTER, "01", "Purchase Retail");

        assertEquals(CobolText.LOW_VALUE, t.state());
        assertEquals("Enter transaction type to be maintained", t.info());
        assertTrue(CobolText.isLowValues(t.last.screen().trtypcd.data));
    }

    @Test
    void sameDescriptionInDifferentCaseIsNoChange() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHENTER, "01", "  purchase");

        assertEquals('S', t.state());
        assertEquals("No change detected with respect to values fetched.", t.error());
        assertEquals("TRTYDSC", t.last.screen().cursorField());
    }

    @Test
    void pf12AtConfirmationBacksOutChanges() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHENTER, "01", "Purchase Retail");

        t.press(Dfhaid.DFHPF12, "01", "Purchase Retail");

        assertEquals('B', t.state());
        assertEquals("Update was cancelled", t.error());
        assertEquals("Update transaction type details shown.", t.info());
        assertEquals(CobolText.pic("PURCHASE", 50), t.last.screen().trtydsc.data);
        assertEquals(Dfhbmsca.DFHBMFSE, t.last.screen().trtydsc.attribute);
        assertEquals("PURCHASE", t.db.rows().get("01"));

        // ENTER with the original values: no change -> 2000 moves BACKED-OUT to CHANGES-NOT-OK
        t.pressResending(Dfhaid.DFHENTER);
        assertEquals('E', t.state());
        assertEquals("No change detected with respect to values fetched.", t.error());
        assertEquals("Update transaction type details shown.", t.info());
    }

    @Test
    void pf12OnShownDetailsReturnsToSearch() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHPF12, "01", "PURCHASE");
        assertEquals(CobolText.LOW_VALUE, t.state());
        assertTrue(t.carddemo().isPgmReenter());
        assertEquals("Enter transaction type to be maintained", t.info());
    }

    @Test
    void deleteRequiresConfirmationThenDeletes() {
        Terminal t = Terminal.withSeedData().start();
        t.show("06");
        t.press(Dfhaid.DFHPF4, "06", "REVERAL");

        assertEquals('9', t.state());
        assertEquals("Delete this record ? Press F4 to confirm", t.info());
        CtrtupaMap s = t.last.screen();
        assertEquals(Dfhbmsca.DFHBMDAR, s.fkeys.attribute);
        assertEquals(Dfhbmsca.DFHBMASB, s.fkey04.attribute);
        assertEquals(Dfhbmsca.DFHBMASB, s.fkey12.attribute);
        assertEquals(Dfhbmsca.DFHBMPRF, s.trtydsc.attribute);

        t.press(Dfhaid.DFHPF4, "06", "REVERAL");

        assertEquals('7', t.state());
        // 18-char message centred: (40 - 18) / 2 + 1 = column 12
        assertEquals(CobolText.pic(CobolText.spaces(11) + "Delete successful.", 45), t.last.screen().infomsg.data);
        assertEquals("06", t.last.screen().trtypcd.data);
        assertFalse(t.db.rows().containsKey("06"));
        assertEquals(1, t.db.syncpoints());

        t.press(Dfhaid.DFHENTER);
        assertEquals(CobolText.LOW_VALUE, t.state());
        assertEquals("Enter transaction type to be maintained", t.info());
    }

    @Test
    void pf12AtDeleteConfirmationCancels() {
        Terminal t = Terminal.withSeedData().start();
        t.show("06");
        t.press(Dfhaid.DFHPF4, "06", "REVERAL");
        t.press(Dfhaid.DFHPF12, "06", "REVERAL");

        assertEquals(CobolText.LOW_VALUE, t.state());
        assertEquals("Delete was cancelled", t.error());
        assertTrue(CobolText.isLowValues(t.last.screen().trtypcd.data));
        assertTrue(t.db.rows().containsKey("06"));
    }

    @Test
    void enterAtDeleteConfirmationIsInvalidKey() {
        Terminal t = Terminal.withSeedData().start();
        t.show("06");
        t.press(Dfhaid.DFHPF4, "06", "REVERAL");
        t.press(Dfhaid.DFHENTER, "06", "REVERAL");
        assertEquals("Invalid key pressed", t.error());
        assertEquals('9', t.state());
    }

    @Test
    void notFoundOffersCreateThenInsertsNewType() {
        Terminal t = Terminal.withSeedData().start();
        t.show("99");

        assertEquals('X', t.state());
        assertEquals("No record found for this key in database", t.error());
        assertEquals("Press F05 to add. F12 to cancel", t.info());
        CtrtupaMap s = t.last.screen();
        assertEquals("99", s.trtypcd.data);
        assertEquals(Dfhbmsca.DFHRED, s.trtypcd.color);
        assertEquals(Dfhbmsca.DFHBMFSE, s.trtypcd.attribute);
        assertEquals(Dfhbmsca.DFHBMASB, s.fkey05.attribute);
        assertEquals(Dfhbmsca.DFHBMASB, s.fkey12.attribute);

        t.press(Dfhaid.DFHPF5, "99", null);
        assertEquals('R', t.state());
        assertEquals("Enter new transaction type details.", t.info());
        assertEquals(Dfhbmsca.DFHBMFSE, t.last.screen().trtydsc.attribute);
        assertEquals("TRTYDSC", t.last.screen().cursorField());

        t.press(Dfhaid.DFHENTER, "99", "Loyalty Points");
        assertEquals('N', t.state());
        assertEquals("Changes validated.Press F5 to save", t.info());

        t.pressResending(Dfhaid.DFHPF5);
        assertEquals('C', t.state());
        assertEquals("Changes committed to database", t.info());
        assertEquals(CobolText.pic("Loyalty Points", 50), t.db.rows().get("99"));
        assertEquals(java.util.List.of("SELECT", "UPDATE", "INSERT", "SYNCPOINT"), t.db.log());
    }

    @Test
    void notFoundThenPf12Cancels() {
        Terminal t = Terminal.withSeedData().start();
        t.show("99");
        t.press(Dfhaid.DFHPF12, "99", null);
        assertEquals(CobolText.LOW_VALUE, t.state());
        assertEquals("Enter transaction type to be maintained", t.info());
    }

    @Test
    void notFoundThenEnterSameKeyRereads() {
        Terminal t = Terminal.withSeedData().start();
        t.show("99");
        t.db.put("99", "ARRIVED");
        t.press(Dfhaid.DFHENTER, "99", null);
        assertEquals('S', t.state());
        assertEquals(CobolText.pic("ARRIVED", 50), t.last.screen().trtydsc.data);
    }

    @Test
    void notFoundThenOtherExistingKeyIsFetchedWithoutNormalisation() {
        Terminal t = Terminal.withSeedData().start();
        t.show("99");
        t.press(Dfhaid.DFHENTER, "02", null);
        assertEquals('S', t.state());
        assertEquals(CobolText.pic("PAYMENT", 50), t.last.screen().trtydsc.data);

        // In NOT-FOUND state 1200 bypasses 1210, so "2" is looked up as "2 " and not found
        Terminal t2 = Terminal.withSeedData().start();
        t2.show("99");
        t2.press(Dfhaid.DFHENTER, "2", null);
        assertEquals('X', t2.state());
        assertEquals("2 ", t2.last.screen().trtypcd.data);
    }
}
