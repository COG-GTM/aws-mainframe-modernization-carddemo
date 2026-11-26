package com.carddemo.programs.cocrdslc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import com.carddemo.programs.cocrdslc.CocrdslcResult.ScreenSent;
import com.carddemo.programs.cocrdslc.copybook.CardRecord;
import com.carddemo.programs.cocrdslc.copybook.CarddemoCommarea;
import com.carddemo.programs.cocrdslc.copybook.CcrdslaField;
import com.carddemo.programs.cocrdslc.copybook.CcrdslaInput;
import com.carddemo.programs.cocrdslc.copybook.Dfhaid;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.util.List;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/** Drives COCRDSLC against every record of app/data/ASCII/carddata.txt. */
class CarddataFixtureTest {

    static List<String> lines;
    static InMemoryCardFileGateway gateway;

    @BeforeAll
    static void load() throws IOException {
        try (InputStream in = CarddataFixtureTest.class.getResourceAsStream("/fixtures/carddata.txt");
             BufferedReader r = new BufferedReader(new InputStreamReader(in, StandardCharsets.US_ASCII))) {
            lines = r.lines().filter(l -> !l.isBlank()).toList();
        }
        try (InputStream in = CarddataFixtureTest.class.getResourceAsStream("/fixtures/carddata.txt")) {
            gateway = InMemoryCardFileGateway.load(in);
        }
    }

    @Test
    void everyFixtureRecordRoundTripsThroughTheCopybookLayout() {
        assertEquals(50, lines.size());
        assertEquals(50, gateway.size());
        for (String line : lines) {
            assertEquals(CardRecord.RECORD_LENGTH, line.length());
            assertEquals(line, CardRecord.parse(line).toFixedWidth());
        }
    }

    @Test
    void everyFixtureCardIsDisplayedFromSearchScreen() {
        Cocrdslc program = new Cocrdslc(gateway, Clock.systemUTC());
        for (String line : lines) {
            CardRecord rec = CardRecord.parse(line);
            CarddemoCommarea c = CarddemoCommarea.initialized();
            c.setPgmReenter();
            ScreenSent s = assertInstanceOf(ScreenSent.class, program.execute(CocrdslcRequest.of(c, Dfhaid.DFHENTER,
                    CcrdslaInput.of(CobolText.zoned(rec.cardAcctId(), 11), rec.cardNum()))));
            assertEquals(rec.cardEmbossedName(), s.map().data(CcrdslaField.CRDNAME), rec.cardNum());
            assertEquals(rec.cardExpiraionDate().substring(5, 7), s.map().data(CcrdslaField.EXPMON));
            assertEquals(rec.cardExpiraionDate().substring(0, 4), s.map().data(CcrdslaField.EXPYEAR));
            assertEquals(rec.cardActiveStatus(), s.map().data(CcrdslaField.CRDSTCD));
            assertEquals(CobolText.pic(Cocrdslc.FOUND_CARDS_FOR_ACCOUNT, 40), s.map().data(CcrdslaField.INFOMSG));
        }
    }

    @Test
    void everyFixtureCardIsDisplayedWhenSelectedFromCardList() {
        Cocrdslc program = new Cocrdslc(gateway, Clock.systemUTC());
        for (String line : lines) {
            CardRecord rec = CardRecord.parse(line);
            CarddemoCommarea c = CarddemoCommarea.initialized();
            c.cdemoFromProgram = "COCRDLIC";
            c.cdemoLastMapset = "COCRDLI";
            c.acctId(rec.cardAcctId());
            c.cardNum(Long.parseLong(rec.cardNum()));
            ScreenSent s = assertInstanceOf(ScreenSent.class,
                    program.execute(CocrdslcRequest.of(c, Dfhaid.DFHENTER, null)));
            assertEquals(CobolText.zoned(rec.cardAcctId(), 11), s.map().data(CcrdslaField.ACCTSID));
            assertEquals(rec.cardEmbossedName(), s.map().data(CcrdslaField.CRDNAME));
        }
    }
}
