package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.CtrtupaMap;
import com.carddemo.programs.cotrtupc.copybook.Dfhaid;
import com.carddemo.programs.cotrtupc.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtupc.support.CobolText;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** 1210/1245 key edits, 1230 description edits and CSSETATY / 3300 error highlighting. */
class CotrtupcValidationTest {

    @Test
    void blankKeyMustBeSupplied() {
        Terminal t = Terminal.withSeedData().start();
        t.press(Dfhaid.DFHENTER, "  ", null);

        assertEquals("Tran Type code must be supplied.", t.error());
        assertEquals(CobolText.LOW_VALUE, t.state());
        CtrtupaMap s = t.last.screen();
        assertEquals("* ", s.trtypcd.data);
        assertEquals(Dfhbmsca.DFHRED, s.trtypcd.color);
        assertEquals("TRTYPCD", s.cursorField());
    }

    @Test
    void asteriskOrUntransmittedKeyCountsAsBlank() {
        Terminal t = Terminal.withSeedData().start();
        t.press(Dfhaid.DFHENTER, "*", null);
        assertEquals("Tran Type code must be supplied.", t.error());

        t.press(Dfhaid.DFHENTER);
        assertEquals("Tran Type code must be supplied.", t.error());
    }

    @ParameterizedTest
    @CsvSource({"AB", "1A", "+", ".", "A1"})
    void nonNumericKeyIsRejected(String key) {
        Terminal t = Terminal.withSeedData().start();
        t.press(Dfhaid.DFHENTER, key, null);
        assertEquals("Tran Type code must be numeric.", t.error());
        assertEquals(CobolText.LOW_VALUE, t.state());
        assertEquals(Dfhbmsca.DFHRED, t.last.screen().trtypcd.color);
        assertTrue(CobolText.isLowValues(t.last.screen().trtypcd.data));
    }

    @ParameterizedTest
    @CsvSource({"00", "0", "-0"})
    void zeroKeyIsRejected(String key) {
        Terminal t = Terminal.withSeedData().start();
        t.press(Dfhaid.DFHENTER, key, null);
        assertEquals("Tran Type code must not be zero.", t.error());
        assertEquals(Dfhbmsca.DFHRED, t.last.screen().trtypcd.color);
    }

    @Test
    void descriptionWithSpecialCharactersIsRejected() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHENTER, "01", "Purchase-Retail");

        assertEquals("Transaction Desc can have numbers or alphabets only.", t.error());
        assertEquals('E', t.state());
        assertEquals("Update transaction type details shown.", t.info());
        CtrtupaMap s = t.last.screen();
        assertEquals(Dfhbmsca.DFHRED, s.trtydsc.color);
        assertEquals(CobolText.pic("Purchase-Retail", 50), s.trtydsc.data);
        assertEquals(Dfhbmsca.DFHBMFSE, s.trtydsc.attribute);
        assertEquals("TRTYDSC", s.cursorField());
    }

    @Test
    void clearedDescriptionMustBeSupplied() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHENTER, "01", "   ");

        assertEquals("Transaction Desc must be supplied.", t.error());
        assertEquals('E', t.state());
        CtrtupaMap s = t.last.screen();
        assertEquals(CobolText.pic("*", 50), s.trtydsc.data);
        assertEquals(Dfhbmsca.DFHRED, s.trtydsc.color);
    }

    @Test
    void correctedDescriptionAfterErrorIsAccepted() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHENTER, "01", "Bad!");
        t.press(Dfhaid.DFHENTER, "01", "Good 2");
        assertEquals('N', t.state());
        assertEquals("", t.error());
        assertEquals(CobolText.LOW_VALUE, t.last.screen().trtydsc.color);
    }

    @Test
    void descriptionIsLeftTrimmedWhenStored() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHENTER, "01", "   Retail Purchase");
        assertEquals(CobolText.pic("Retail Purchase", 50), t.ttup().newDesc());
    }

    @Test
    void notFoundStateWithChangedKeyAndNoDescriptionAsksForDescription() {
        Terminal t = Terminal.withSeedData().start();
        t.show("99");
        // a different (not yet stored) key: 1150 stores it, 1200 then compares equal and re-reads
        t.press(Dfhaid.DFHENTER, "88", null);
        assertEquals('X', t.state());
        assertEquals("88", t.ttup().newType());
    }
}
