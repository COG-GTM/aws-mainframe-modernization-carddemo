package com.carddemo.programs.cocrdupc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cocrdupc.copybook.AidKey;
import com.carddemo.programs.cocrdupc.copybook.CardRecord;
import com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Runs the program against the CARDDAT sample extract app/data/ASCII/carddata.txt. */
class CardDataFixtureTest {

    static final Path CARDDATA = Path.of("../../../app/data/ASCII/carddata.txt");

    InMemoryCardFileGateway file;
    CocrdupcService service;
    List<String> lines;

    @BeforeEach
    void load() throws Exception {
        lines = Files.readAllLines(CARDDATA, StandardCharsets.US_ASCII);
        file = InMemoryCardFileGateway.fromFixedWidthFile(CARDDATA);
        service = new CocrdupcService(file, () -> LocalDateTime.of(2026, 11, 26, 9, 0));
    }

    @Test
    void everySampleRecordRoundTripsThroughCvact02y() {
        assertEquals(50, file.size());
        for (String line : lines) {
            CardRecord r = CardRecord.parse(line);
            assertEquals(Cobol.alnum(line, CardRecord.LENGTH), r.toFixedWidth());
            assertTrue(r.cardExpiraionDate().matches("\\d{4}-\\d{2}-\\d{2}"));
        }
    }

    @Test
    void everySampleCardCanBeFetchedForUpdate() {
        String start = service.process(new CocrdupcRequest(null, AidKey.ENTER, null)).commarea();
        for (String line : lines) {
            CardRecord r = CardRecord.parse(line);
            CocrdupcResponse resp = service.process(new CocrdupcRequest(start, AidKey.ENTER,
                    CcrdupaInput.search(Cobol.numeric(r.cardAcctId(), 11), r.cardNum())));
            assertTrue(resp.programArea().isShowDetails(), r.cardNum());
            assertEquals(Cobol.upper(r.cardEmbossedName()).strip(), resp.screen().text(Field.CRDNAME));
            assertEquals(r.cardExpiraionDate().substring(5, 7), resp.screen().text(Field.EXPMON));
            assertEquals(r.cardExpiraionDate().substring(0, 4), resp.screen().text(Field.EXPYEAR));
            assertEquals(r.cardActiveStatus(), resp.screen().text(Field.CRDSTCD));
        }
    }

    @Test
    void updateFirstSampleCardEndToEnd() {
        CardRecord r = CardRecord.parse(lines.get(0));
        String acct = Cobol.numeric(r.cardAcctId(), 11);
        String day = r.cardExpiraionDate().substring(8, 10);
        CocrdupcResponse step = service.process(new CocrdupcRequest(null, AidKey.ENTER, null));
        step = service.process(new CocrdupcRequest(step.commarea(), AidKey.ENTER,
                CcrdupaInput.search(acct, r.cardNum())));
        CcrdupaInput change = new CcrdupaInput(acct, r.cardNum(), "ANIYA VON SMITH", "N", "06", "2030", day);
        step = service.process(new CocrdupcRequest(step.commarea(), AidKey.ENTER, change));
        assertEquals("Changes validated.Press F5 to save", step.screen().text(Field.INFOMSG));
        step = service.process(new CocrdupcRequest(step.commarea(), AidKey.PF5, change));
        assertEquals("Changes committed to database", step.screen().text(Field.INFOMSG));

        CardRecord saved = file.get(r.cardNum()).orElseThrow();
        assertEquals("ANIYA VON SMITH", saved.cardEmbossedName().strip());
        assertEquals("2030-06-" + day, saved.cardExpiraionDate());
        assertEquals("N", saved.cardActiveStatus());
        assertEquals(r.cardAcctId(), saved.cardAcctId());
    }
}
