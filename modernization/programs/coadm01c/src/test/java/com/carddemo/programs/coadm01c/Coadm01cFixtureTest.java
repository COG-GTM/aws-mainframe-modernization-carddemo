package com.carddemo.programs.coadm01c;

import com.carddemo.programs.coadm01c.copybook.CardDemoCommarea;
import com.carddemo.programs.coadm01c.copybook.CardDemoCommarea.Field;
import com.carddemo.programs.coadm01c.copybook.Coadm1aInput;
import com.carddemo.programs.coadm01c.copybook.Dfhaid;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

/**
 * Builds the COMMAREA from the first records of the sample ASCII data (app/data/ASCII) and checks that a full
 * admin-menu round trip (first entry -> ENTER option 1) carries customer/account/card info through byte for byte.
 */
class Coadm01cFixtureTest {

    static final Path DATA = Path.of("../../../app/data/ASCII");

    static String firstLine(String file) throws IOException {
        try (var lines = Files.lines(DATA.resolve(file))) {
            return lines.findFirst().orElseThrow();
        }
    }

    @Test
    void commareaBuiltFromSampleDataRoundTripsThroughAdminMenu() throws IOException {
        String cust = firstLine("custdata.txt");   // CVCUS01Y: CUST-ID 9(09), names X(25) x3
        String acct = firstLine("acctdata.txt");   // CVACT01Y: ACCT-ID 9(11), ACCT-ACTIVE-STATUS X(01)
        String card = firstLine("carddata.txt");   // CVACT02Y: CARD-NUM X(16)

        CardDemoCommarea c = CardDemoCommarea.fromRecord("");
        c.setFromTranid("CC00");
        c.setFromProgram("COSGN00C");
        c.setUserId("ADMIN001");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmContext(CardDemoCommarea.PgmContext.ENTER);
        c.setCustId(Long.parseLong(cust.substring(0, 9)));
        c.setCustFname(cust.substring(9, 34));
        c.setCustMname(cust.substring(34, 59));
        c.setCustLname(cust.substring(59, 84));
        c.setAcctId(Long.parseLong(acct.substring(0, 11)));
        c.setAcctStatus(acct.substring(11, 12));
        c.setCardNum(Long.parseLong(card.substring(0, 16)));

        assertEquals(1L, c.custId());
        assertEquals(1L, c.acctId());
        assertEquals(card.substring(0, 16), c.cardNum());

        Coadm01cService service = new Coadm01cService(InMemoryProgramControlGateway.allCardDemoPrograms(),
                Clock.fixed(Instant.parse("2022-07-19T23:12:32Z"), ZoneOffset.UTC));

        var first = assertInstanceOf(Coadm01cOutcome.Returned.class,
                service.execute(Coadm01cRequest.of(c, Dfhaid.DFHENTER, null)));
        var second = assertInstanceOf(Coadm01cOutcome.Transferred.class,
                service.execute(Coadm01cRequest.of(first.commarea(), Dfhaid.DFHENTER, Coadm1aInput.keyed("1"))));

        assertEquals("COUSR00C", second.program());
        String in = c.toRecord();
        String out = second.commarea().toRecord();
        assertEquals("CA00COADM01C", out.substring(0, 12));
        assertEquals(in.substring(12, Field.PGM_CONTEXT.offset), out.substring(12, Field.PGM_CONTEXT.offset));
        assertEquals('0', second.commarea().pgmContext());
        assertEquals(in.substring(Field.CUST_ID.offset), out.substring(Field.CUST_ID.offset));
        assertEquals(cust.substring(9, 34), second.commarea().custFname());
    }
}
