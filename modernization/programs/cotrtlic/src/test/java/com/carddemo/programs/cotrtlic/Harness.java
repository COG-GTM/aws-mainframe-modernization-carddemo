package com.carddemo.programs.cotrtlic;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import com.carddemo.programs.cotrtlic.copybook.CotrtliMap;
import com.carddemo.programs.cotrtlic.copybook.Dfhaid;
import com.carddemo.programs.cotrtlic.gateway.InMemoryDsntiac;
import com.carddemo.programs.cotrtlic.gateway.InMemorySyncpoint;
import com.carddemo.programs.cotrtlic.gateway.InMemoryTransactionTypeTable;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.function.Consumer;

/** Drives COTRTLIC like a 3270 terminal: each step sends back the last screen with the user's edits. */
final class Harness {

    static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);

    final InMemoryTransactionTypeTable table = new InMemoryTransactionTypeTable();
    final InMemorySyncpoint syncpoint = new InMemorySyncpoint();
    final CotrtlicService service = new CotrtlicService(table, new InMemoryDsntiac(), syncpoint, CLOCK);
    CicsResponse.SendMapAndReturn last;

    /** Loads app/data/ASCII/trantype.txt (TRAN-TYPE X(2), TRAN-TYPE-DESC X(50), FILLER X(8)). */
    Harness withFixture() {
        try (InputStream in = Harness.class.getResourceAsStream("/trantype.txt");
                BufferedReader r = new BufferedReader(new InputStreamReader(in, StandardCharsets.US_ASCII))) {
            String line;
            while ((line = r.readLine()) != null) {
                line = line.replace("\r", "");
                if (line.length() >= 52) {
                    table.put(line.substring(0, 2), line.substring(2, 52).stripTrailing());
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return this;
    }

    Harness withRows(int from, int to) {
        for (int i = from; i <= to; i++) {
            table.put(String.format("%02d", i), "Type " + i);
        }
        return this;
    }

    CicsResponse send(CicsRequest req) {
        CicsResponse resp = service.handle(req);
        if (resp instanceof CicsResponse.SendMapAndReturn s) {
            last = s;
        }
        return resp;
    }

    CicsResponse.SendMapAndReturn start() {
        return assertInstanceOf(CicsResponse.SendMapAndReturn.class, send(CicsRequest.firstEntry(Dfhaid.DFHENTER)));
    }

    /** Presses {@code aid} on the current screen after applying {@code edits}. */
    CicsResponse press(Dfhaid aid, Consumer<CotrtliMap> edits) {
        CotrtliMap terminal = last.map().copy();
        edits.accept(terminal);
        return send(new CicsRequest(aid, last.commarea().copy(), terminal));
    }

    CicsResponse press(Dfhaid aid) {
        return press(aid, m -> { });
    }

    CicsResponse.SendMapAndReturn pressForMap(Dfhaid aid, Consumer<CotrtliMap> edits) {
        return assertInstanceOf(CicsResponse.SendMapAndReturn.class, press(aid, edits));
    }

    CicsResponse.SendMapAndReturn pressForMap(Dfhaid aid) {
        return pressForMap(aid, m -> { });
    }

    String errmsg() {
        return last.map().errmsg.data().stripTrailing();
    }

    String infomsg() {
        return last.map().infomsg.data().strip();
    }

    String rowType(int i) {
        return last.map().row(i).trttyp.data();
    }

    String rowDesc(int i) {
        return last.map().row(i).trtypd.data().stripTrailing();
    }
}
