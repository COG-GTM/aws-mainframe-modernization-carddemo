package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.Dfhaid;
import com.carddemo.programs.cotrtupc.copybook.TtypUpdateRecord;
import com.carddemo.programs.cotrtupc.gateway.InMemoryTransactionTypeGateway;
import com.carddemo.programs.cotrtupc.support.CobolText;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Fixture-based tests over app/data/ASCII/trantype.txt and trancatg.txt (copied into test resources). */
class CotrtupcFixtureTest {

    private static Reader resource(String name) {
        return new InputStreamReader(Objects.requireNonNull(
                CotrtupcFixtureTest.class.getResourceAsStream("/fixtures/" + name)), StandardCharsets.US_ASCII);
    }

    private static Terminal loaded(boolean withCategories) throws IOException {
        InMemoryTransactionTypeGateway db = new InMemoryTransactionTypeGateway().loadTranTypeFile(resource("trantype.txt"));
        if (withCategories) {
            db.loadTranCatFile(resource("trancatg.txt"));
        }
        return new Terminal(db).start();
    }

    @Test
    void fixtureRecordsHaveTheSixtyByteLayout() throws IOException {
        try (var br = new java.io.BufferedReader(resource("trantype.txt"))) {
            String line = br.readLine();
            TtypUpdateRecord r = TtypUpdateRecord.parse(line);
            assertEquals(TtypUpdateRecord.LENGTH, line.length());
            assertEquals("01", r.ttupUpdateTtypType());
            assertEquals(CobolText.pic("Purchase", 50), r.ttupUpdateTtypTypeDesc());
            assertEquals("00000000", r.filler());
        }
    }

    @ParameterizedTest
    @CsvSource({"1,01,Purchase", "02,02,Payment", "3,03,Credit", "04,04,Authorization", "05,05,Refund",
        "6,06,Reversal", "07,07,Adjustment"})
    void everyFixtureTypeCanBeDisplayed(String typed, String key, String desc) throws IOException {
        Terminal t = loaded(false);
        t.show(typed);
        assertEquals('S', t.state());
        assertEquals(key, t.last.screen().trtypcd.data);
        assertEquals(CobolText.pic(desc, 50), t.last.screen().trtydsc.data);
    }

    @Test
    void fixtureTypesWithCategoriesCannotBeDeleted() throws IOException {
        Terminal t = loaded(true);
        t.show("01");
        t.press(Dfhaid.DFHPF4, "01", "Purchase");
        t.press(Dfhaid.DFHPF4, "01", "Purchase");
        assertEquals('8', t.state());
        assertEquals("Please delete associated child records first:SQLCODE : -532:",
                t.errmsgRaw().substring(0, 60));
        assertEquals(7, t.db.rows().size());
    }

    @Test
    void fixtureTypeUpdateRoundTrip() throws IOException {
        Terminal t = loaded(false);
        t.show("07");
        t.press(Dfhaid.DFHENTER, "07", "Manual Adjustment");
        t.pressResending(Dfhaid.DFHPF5);
        assertEquals("Changes committed to database", t.info());

        Terminal again = new Terminal(t.db).start();
        again.show("07");
        assertEquals(CobolText.pic("Manual Adjustment", 50), again.last.screen().trtydsc.data);
    }
}
