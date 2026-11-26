package com.carddemo.programs.cbact02c;

import com.carddemo.programs.cbact02c.copybook.CardRecord;
import com.carddemo.programs.cbact02c.io.FixedWidthCardFile;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Runs CBACT02C over the repository's sample card master unload (app/data/ASCII/carddata.txt). */
class Cbact02cFixtureTest {

    static final Path CARDDATA = Path.of("../../../app/data/ASCII/carddata.txt");

    @Test
    void displaysAllFiftySampleCardsInKeyOrder() throws IOException {
        assertTrue(Files.isRegularFile(CARDDATA), "sample data missing: " + CARDDATA.toAbsolutePath());
        List<String> input = Files.readAllLines(CARDDATA, StandardCharsets.ISO_8859_1);
        List<String> expectedRecords = new ArrayList<>(input);
        expectedRecords.sort(Comparator.comparing(r -> r.substring(0, 16)));

        List<String> sysout = new ArrayList<>();
        int rc = new Cbact02c(new FixedWidthCardFile(CARDDATA), sysout::add).run();

        assertEquals(0, rc);
        assertEquals(52, sysout.size());
        assertEquals("START OF EXECUTION OF PROGRAM CBACT02C", sysout.get(0));
        assertEquals("END OF EXECUTION OF PROGRAM CBACT02C", sysout.get(51));
        assertEquals(expectedRecords, sysout.subList(1, 51));
    }

    @Test
    void everySampleRecordDecodesAndRoundTripsPerCvact02y() throws IOException {
        List<String> input = Files.readAllLines(CARDDATA, StandardCharsets.ISO_8859_1);
        assertEquals(50, input.size());
        for (String line : input) {
            assertEquals(CardRecord.RECORD_LENGTH, line.length());
            CardRecord card = CardRecord.fromFixedWidth(line);
            assertEquals(line, card.toFixedWidth());
            assertTrue(card.cardActiveStatus().equals("Y") || card.cardActiveStatus().equals("N"));
            assertTrue(card.cardExpiraionDate().matches("\\d{4}-\\d{2}-\\d{2}"));
        }
    }

    @Test
    void firstSampleRecordFieldsMatchCopybookOffsets() throws IOException {
        String line = Files.readAllLines(CARDDATA, StandardCharsets.ISO_8859_1).get(0);
        CardRecord card = CardRecord.fromFixedWidth(line);

        assertEquals(line.substring(0, 16), card.cardNum());
        assertEquals(Long.parseLong(line.substring(16, 27)), card.cardAcctId());
        assertEquals(Integer.parseInt(line.substring(27, 30)), card.cardCvvCd());
        assertEquals(line.substring(30, 80), card.cardEmbossedName());
        assertEquals(line.substring(80, 90), card.cardExpiraionDate());
        assertEquals(line.substring(90, 91), card.cardActiveStatus());
        assertEquals(" ".repeat(59), card.filler());
    }
}
