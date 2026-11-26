package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.Cobol;
import com.carddemo.programs.cotrn01c.copybook.Dfhaid;
import com.carddemo.programs.cotrn01c.copybook.TranRecord;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Loads app/data/ASCII/dailytran.txt (CVTRA05Y layout) into the in-memory TRANSACT file. */
class Cotrn01cFixtureTest {

    private static final Path DATA = Path.of("../../../app/data/ASCII/dailytran.txt");
    private static List<String> lines;
    private static InMemoryTransactFileGateway gateway;

    @BeforeAll
    static void load() throws IOException {
        lines = Files.readAllLines(DATA, StandardCharsets.US_ASCII);
        gateway = new InMemoryTransactFileGateway(
                TranRecordFileReader.readAll(Files.newBufferedReader(DATA, StandardCharsets.US_ASCII)));
    }

    private static TrnViewScreen view(String id) {
        Cotrn01cCommarea c = new Cotrn01cCommarea();
        c.fromProgram("COTRN00C");
        c.pgmContext(Cotrn01cCommarea.PgmContext.REENTER);
        Cotrn01cService service = new Cotrn01cService(gateway,
                Clock.fixed(Instant.parse("2026-11-26T00:00:00Z"), ZoneOffset.UTC), s -> { });
        CicsOutcome.Return r = assertInstanceOf(CicsOutcome.Return.class,
                service.execute(new CicsRequest(c.toFixedString(), Dfhaid.DFHENTER, TrnViewRequest.ofTranId(id))));
        return r.screen();
    }

    @Test
    void allRecordsLoadAndRoundTrip() {
        assertEquals(300, lines.size());
        assertEquals(300, gateway.size());
        for (String line : lines) {
            assertEquals(Cobol.moveAlnum(line, 350), TranRecord.parse(line).format());
        }
    }

    @Test
    void firstRecord_purchase() {
        TrnViewScreen s = view("0000000000683580");
        assertEquals("0000000000683580", s.trnid());
        assertEquals("4859452612877065", s.cardnum());
        assertEquals("01", s.ttypcd());
        assertEquals("0001", s.tcatcd());
        assertEquals("POS TERM  ", s.trnsrc());
        assertEquals("+00000504.77", s.trnamt());
        assertEquals(Cobol.moveAlnum("Purchase at Abshire-Lowe", 60), s.tdesc());
        assertEquals("2022-06-10", s.torigdt());
        assertEquals(Cobol.spaces(10), s.tprocdt());
        assertEquals("800000000", s.mid());
        assertEquals(Cobol.moveAlnum("Abshire-Lowe", 30), s.mname());
        assertEquals(Cobol.moveAlnum("North Enoshaven", 25), s.mcity());
        assertEquals("72112     ", s.mzip());
        assertEquals(Cobol.spaces(78), s.errmsg());
    }

    @Test
    void secondRecord_returnHasNegativeAmount() {
        TrnViewScreen s = view("0000000001774260");
        assertEquals("03", s.ttypcd());
        assertEquals("OPERATOR  ", s.trnsrc());
        assertEquals("-00000919.00", s.trnamt());
        assertEquals(Cobol.moveAlnum("Nitzsche, Nicolas and Lowe", 30), s.mname());
    }

    @Test
    void everyRecord_viewMatchesSourceFields() {
        for (String line : lines) {
            TranRecord r = TranRecord.parse(line);
            TrnViewScreen s = view(r.tranId());
            assertEquals(r.tranId(), s.trnid());
            assertEquals(Cobol.editPlus8Dot2(r.tranAmt()), s.trnamt());
            assertEquals(r.tranAmt().signum() < 0 ? '-' : '+', s.trnamt().charAt(0));
            assertEquals(new BigDecimal(s.trnamt().substring(1)).abs(), r.tranAmt().abs());
            assertEquals(r.tranOrigTs().substring(0, 10), s.torigdt());
            assertTrue(s.errmsg().isBlank());
        }
    }
}
