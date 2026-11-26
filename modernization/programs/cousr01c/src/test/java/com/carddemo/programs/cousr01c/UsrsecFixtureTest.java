package com.carddemo.programs.cousr01c;

import com.carddemo.programs.cousr01c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr01c.copybook.Dfhaid;
import com.carddemo.programs.cousr01c.copybook.SecUserData;
import com.carddemo.programs.cousr01c.dto.CicsRequest;
import com.carddemo.programs.cousr01c.dto.CicsResponse;
import com.carddemo.programs.cousr01c.dto.UserAddScreenInput;
import com.carddemo.programs.cousr01c.gateway.InMemoryUserSecRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

/** Uses the sample USRSEC unload app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS (10 x 80-byte Cp037 records). */
class UsrsecFixtureTest {

    private static final String FIXTURE = "/fixtures/AWS.M2.CARDDEMO.USRSEC.PS";

    private InMemoryUserSecRepository repo;
    private Cousr01cService service;
    private byte[] original;

    @BeforeEach
    void load() throws IOException {
        try (InputStream in = getClass().getResourceAsStream(FIXTURE)) {
            original = in.readAllBytes();
        }
        try (InputStream in = getClass().getResourceAsStream(FIXTURE)) {
            repo = InMemoryUserSecRepository.loadEbcdic(in);
        }
        service = new Cousr01cService(repo,
                Clock.fixed(Instant.parse("2026-01-02T03:04:05Z"), ZoneOffset.UTC));
    }

    private String enter(UserAddScreenInput input) {
        CardDemoCommarea c = new CardDemoCommarea();
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        CicsResponse r = service.handle(new CicsRequest(c, Dfhaid.DFHENTER, input));
        return assertInstanceOf(CicsResponse.SendAndReturn.class, r).screen().errmsgText();
    }

    @Test
    void fixtureDecodesToTenUsers() {
        assertEquals(10, repo.size());
        SecUserData admin = repo.find("ADMIN001").orElseThrow();
        assertEquals("MARGARET", admin.secUsrFname().strip());
        assertEquals("GOLD", admin.secUsrLname().strip());
        assertEquals("A", admin.secUsrType());
        assertEquals("U", repo.find("USER0005").orElseThrow().secUsrType());
    }

    @Test
    void fixtureRoundTripsByteForByte() {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        List<SecUserData> all = repo.all();
        all.forEach(u -> out.writeBytes(u.toEbcdic()));
        assertArrayEquals(original, out.toByteArray());
    }

    @Test
    void existingFixtureUserIsRejected() {
        assertEquals("User ID already exist...",
                enter(new UserAddScreenInput("NEW", "PERSON", "ADMIN001", "PASSWORD", "A")));
        assertEquals("MARGARET", repo.find("ADMIN001").orElseThrow().secUsrFname().strip());
        assertEquals(10, repo.size());
    }

    @Test
    void newUserIsAddedInKeyOrder() {
        assertEquals("User USER0006 has been added ...",
                enter(new UserAddScreenInput("ALEX", "TEST", "USER0006", "PASSWORD", "U")));
        assertEquals(11, repo.size());
        assertEquals("USER0006", repo.all().get(10).secUsrId());
        byte[] rec = repo.find("USER0006").orElseThrow().toEbcdic();
        assertEquals(SecUserData.LENGTH, rec.length);
    }
}
