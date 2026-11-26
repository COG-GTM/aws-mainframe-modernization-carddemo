package com.carddemo.programs.cbpaup0c;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.carddemo.programs.cbpaup0c.copybook.CobolCodec;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthDetails;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthSummary;
import com.carddemo.programs.cbpaup0c.ims.HdUnloadFile;
import com.carddemo.programs.cbpaup0c.ims.InMemoryPautDb;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Differential test against the original COBOL: src/test/cobol builds CBPAUP0C with GnuCOBOL 3.1.2 (EXEC DLI
 * replaced by CALLs to an in-memory DL/I stub), runs it on the shipped DBPAUTP0 unload, and records the SYSOUT
 * and the surviving database. The Java port must reproduce both exactly.
 */
class GoldenFixtureTest {

    static final String FIXTURE = "/fixtures/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat";

    @ParameterizedTest(name = "{0}")
    @CsvSource(delimiter = '|', ignoreLeadingAndTrailingWhitespace = false, value = {
            "jcl|2026-11-26|00,00001,00001,Y",
            "exp15|2023-11-20|15,00002,00003,N",
            "defaults|2023-11-20|XX,     ,     ,N",
            "chkp|2023-11-20|05,00000,00000,N",
            "zeroparm|2023-11-20|7A,0    ,0    ,N",
            "emptysysin|2024-01-02|''"
    })
    void matchesGnuCobolRunOnSampleUnload(String scenario, String date, String sysin) throws IOException {
        HdUnloadFile unload = loadFixture();
        List<String> sysout = new ArrayList<>();
        Clock clock = Clock.fixed(LocalDate.parse(date).atTime(10, 0).toInstant(ZoneOffset.UTC), ZoneOffset.UTC);

        int rc = new Cbpaup0c(unload.database(), sysin == null ? "" : sysin, clock, sysout::add).run();

        assertEquals(0, rc);
        assertEquals(golden(scenario + ".out"), sysout);
        assertEquals(golden(scenario + ".state"), renderState(unload.database()));
    }

    static HdUnloadFile loadFixture() throws IOException {
        try (InputStream in = GoldenFixtureTest.class.getResourceAsStream(FIXTURE)) {
            return HdUnloadFile.read(in, CobolCodec.EBCDIC);
        }
    }

    private static List<String> golden(String name) throws IOException {
        try (InputStream in = GoldenFixtureTest.class.getResourceAsStream("/golden/" + name)) {
            String text = new String(in.readAllBytes(), StandardCharsets.ISO_8859_1).replace("\r\n", "\n");
            return text.isEmpty() ? List.of() : List.of(text.substring(0, text.length() - 1).split("\n", -1));
        }
    }

    /** Same layout as DUMP-STATE in src/test/cobol/DLISTUB.cbl (edited pictures -9(11), -9(5), -9(9)). */
    static List<String> renderState(InMemoryPautDb db) {
        List<String> lines = new ArrayList<>();
        for (InMemoryPautDb.Root root : db.roots()) {
            if (root.summary().deleted()) {
                continue;
            }
            long acct = PendingAuthSummary.fromBytes(root.summary().data(), db.charset()).accountId();
            lines.add(("S " + edit(acct, 11) + " " + edit(0, 5) + " " + edit(0, 9)).stripTrailing());
            for (InMemoryPautDb.Segment d : root.details()) {
                if (!d.deleted()) {
                    PendingAuthDetails det = PendingAuthDetails.fromBytes(d.data(), db.charset());
                    lines.add("D " + edit(acct, 11) + " " + edit(det.authDate9c(), 5) + " " + edit(det.authTime9c(), 9));
                }
            }
        }
        return lines;
    }

    private static String edit(long v, int digits) {
        String s = Long.toString(Math.abs(v));
        return (v < 0 ? "-" : " ") + "0".repeat(Math.max(0, digits - s.length())) + s;
    }
}
