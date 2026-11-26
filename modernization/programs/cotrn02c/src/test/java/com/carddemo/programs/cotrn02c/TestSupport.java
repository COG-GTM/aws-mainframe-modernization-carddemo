package com.carddemo.programs.cotrn02c;

import com.carddemo.programs.cotrn02c.copybook.CardDemoCommarea;
import com.carddemo.programs.cotrn02c.copybook.CardXrefRecord;
import com.carddemo.programs.cotrn02c.copybook.Dfhaid;
import com.carddemo.programs.cotrn02c.gateway.Csutldtc;
import com.carddemo.programs.cotrn02c.gateway.InMemoryCardXref;
import com.carddemo.programs.cotrn02c.gateway.InMemoryTransactFile;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

/** Shared fixtures: in-memory files, fixed clock (2026-11-26 09:05:07 UTC), commarea builders. */
abstract class TestSupport {

    static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T09:05:07Z"), ZoneOffset.UTC);
    static final String CARD = "0500024453765740";
    static final String ACCT = "00000000050";

    InMemoryTransactFile transact = new InMemoryTransactFile();
    InMemoryCardXref xref = new InMemoryCardXref();
    final List<String> displayed = new ArrayList<>();

    TestSupport() {
        xref.put(new CardXrefRecord(CARD, 50, 50));
    }

    Cotrn02cService service() {
        return new Cotrn02cService(transact, xref, new Csutldtc(), CLOCK, displayed::add);
    }

    static Stream<String> fixture(String name) {
        try (InputStream in = TestSupport.class.getResourceAsStream("/fixtures/" + name)) {
            return new String(in.readAllBytes(), StandardCharsets.US_ASCII).lines().toList().stream();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** COMMAREA as it arrives on re-entry (CDEMO-PGM-REENTER set). */
    static String reenterCommarea(String fromProgram) {
        CardDemoCommarea c = CardDemoCommarea.fromRecord("");
        c.setFromTranId("CT00");
        c.setFromProgram(fromProgram);
        c.setUserId("USER0001");
        c.setUserType('U');
        c.setPgmReenter();
        return c.toRecord();
    }

    static String firstEntryCommarea(String selected) {
        CardDemoCommarea c = CardDemoCommarea.fromRecord("");
        c.setFromTranId("CT00");
        c.setFromProgram("COTRN00C");
        c.setPgmContext(0);
        c.setCt02TrnSelected(selected);
        return c.toRecord();
    }

    static Cotrn2aRequest.Builder valid() {
        return Cotrn2aRequest.builder()
                .acctId(ACCT)
                .typeCd("01").catCd("0001").source("POS TERM").desc("Purchase at Test Store")
                .amount("+00000012.50").origDate("2022-06-10").procDate("2022-06-11")
                .merchantId("800000000").merchantName("Test Store").merchantCity("Sydney").merchantZip("2000")
                .confirm("Y");
    }

    CicsOutcome enter(Cotrn2aRequest req) {
        return service().execute(reenterCommarea("COTRN00C"), Dfhaid.DFHENTER, req);
    }

    Cotrn2aResponse screen(CicsOutcome o) {
        return assertInstanceOf(CicsOutcome.SendMap.class, o).screen();
    }
}
