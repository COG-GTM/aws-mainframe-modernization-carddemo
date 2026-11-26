package com.carddemo.programs.cocrdlic;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.cocrdlic.CardListResponse.SendMapAndReturn;
import com.carddemo.programs.cocrdlic.copybook.CardListMap;
import com.carddemo.programs.cocrdlic.copybook.CardRecord;
import com.carddemo.programs.cocrdlic.copybook.EibAid;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Runs COCRDLIC against the repository's CARDDAT sample extract (app/data/ASCII/carddata.txt). */
class CarddataFixtureTest {

    static final Path CARDDATA = Path.of("../../../app/data/ASCII/carddata.txt");

    InMemoryCardFileGateway gateway;
    CocrdlicService service;

    @BeforeEach
    void load() throws Exception {
        gateway = InMemoryCardFileGateway.fromFixedWidthFile(CARDDATA);
        service = new CocrdlicService(gateway, Clock.fixed(Instant.parse("2026-01-02T03:04:05Z"), ZoneOffset.UTC));
    }

    static List<String> rows(CardListMap m) {
        List<String> out = new ArrayList<>();
        for (int r = 1; r <= 7; r++) {
            if (!m.crdnum(r).text().isEmpty()) {
                out.add(m.crdnum(r).text());
            }
        }
        return out;
    }

    @Test
    void fixtureParsesEveryRecordAndRoundTrips() throws Exception {
        List<String> lines = Files.readAllLines(CARDDATA, StandardCharsets.US_ASCII);
        assertEquals(50, gateway.size());
        for (String line : lines) {
            assertEquals(line, CardRecord.parse(line).toFixedWidth());
        }
    }

    @Test
    void firstPageIsFirstSevenKeysInCardNumberOrder() {
        SendMapAndReturn r = (SendMapAndReturn) service.execute(CardListRequest.initial());
        List<String> expected = gateway.allInKeyOrder().stream().limit(7).map(CardRecord::cardNum).toList();
        assertEquals(expected, rows(r.screen()));
        CardRecord firstCard = gateway.allInKeyOrder().get(0);
        assertEquals(firstCard.cardAcctIdDisplay(), r.screen().acctno(1).text());
        assertEquals(String.valueOf(firstCard.cardActiveStatus()), r.screen().crdsts(1).text());
    }

    @Test
    void pagingForwardVisitsEveryCardExactlyOnce() {
        SendMapAndReturn r = (SendMapAndReturn) service.execute(CardListRequest.initial());
        List<String> seen = new ArrayList<>(rows(r.screen()));
        int pages = 1;
        while (r.programState().isNextPageExists()) {
            r = (SendMapAndReturn) service.execute(
                    CardListRequest.reenter(r.commareaImage(), EibAid.PF8, CardListScreenInput.empty()));
            seen.addAll(rows(r.screen()));
            pages++;
        }
        assertEquals(8, pages);
        assertEquals("8  ", r.screen().pageno.data());
        assertEquals(gateway.allInKeyOrder().stream().map(CardRecord::cardNum).toList(), seen);
        assertEquals(CocrdlicService.MSG_NO_MORE_RECORDS, r.screen().errmsg.text());
    }

    @Test
    void accountFilterReturnsOnlyThatAccountsCards() {
        CardRecord target = gateway.allInKeyOrder().get(10);
        SendMapAndReturn p1 = (SendMapAndReturn) service.execute(CardListRequest.initial());
        SendMapAndReturn r = (SendMapAndReturn) service.execute(CardListRequest.reenter(p1.commareaImage(),
                EibAid.ENTER, CardListScreenInput.filters(target.cardAcctIdDisplay(), null)));
        List<String> expected = gateway.allInKeyOrder().stream()
                .filter(c -> c.cardAcctId() == target.cardAcctId()).map(CardRecord::cardNum).limit(7).toList();
        assertFalse(expected.isEmpty());
        assertEquals(expected, rows(r.screen()));
    }

    @Test
    void cardFilterThenSelectRoutesToDetail() {
        CardRecord target = gateway.allInKeyOrder().get(33);
        SendMapAndReturn p1 = (SendMapAndReturn) service.execute(CardListRequest.initial());
        SendMapAndReturn filtered = (SendMapAndReturn) service.execute(CardListRequest.reenter(p1.commareaImage(),
                EibAid.ENTER, CardListScreenInput.filters(null, target.cardNum())));
        assertEquals(List.of(target.cardNum()), rows(filtered.screen()));
        CardListResponse.Xctl x = (CardListResponse.Xctl) service.execute(CardListRequest.reenter(
                filtered.commareaImage(), EibAid.ENTER,
                CardListScreenInput.filters(null, target.cardNum()).withSelection(1, "S")));
        assertEquals("COCRDSLC", x.program());
        assertEquals(Long.parseLong(target.cardNum()), x.commarea().getCardNum());
        assertEquals(target.cardAcctId(), x.commarea().getAcctId());
    }
}
