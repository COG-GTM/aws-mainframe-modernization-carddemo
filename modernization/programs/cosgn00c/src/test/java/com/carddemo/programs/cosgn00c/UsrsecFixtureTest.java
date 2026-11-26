package com.carddemo.programs.cosgn00c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cosgn00c.copybook.SecUserData;

import java.io.IOException;
import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * Loads the USRSEC seed records from app/jcl/DUSRSECJ.jcl (the IEBGENER SYSUT1 in-stream data that
 * is REPROed into the USRSEC KSDS) and signs on as every seeded user.
 */
class UsrsecFixtureTest {

    private static final Path JCL = Path.of("..", "..", "..", "app", "jcl", "DUSRSECJ.jcl");

    private static List<String> seedRecords;
    private static InMemoryUserSecurityRepository repo;

    @BeforeAll
    static void load() throws IOException {
        List<String> lines = Files.readAllLines(JCL);
        int start = lines.indexOf("//SYSUT1   DD *") + 1;
        int end = lines.subList(start, lines.size()).indexOf("/*") + start;
        seedRecords = lines.subList(start, end);
        repo = InMemoryUserSecurityRepository.load(new StringReader(String.join("\n", seedRecords)));
    }

    private SignonService service() {
        return new SignonService(repo, CicsSystemInfo.of("CARDDEMO", "CICS"),
                Clock.fixed(Instant.parse("2026-11-26T00:00:00Z"), ZoneOffset.UTC));
    }

    @Test
    void seedFileHasFiveAdminsAndFiveUsers() {
        assertEquals(10, repo.size());
        List<String> types = seedRecords.stream().map(SecUserData::fromRecord).map(SecUserData::type)
                .collect(Collectors.toList());
        assertEquals(5, types.stream().filter("A"::equals).count());
        assertEquals(5, types.stream().filter("U"::equals).count());
    }

    @Test
    void everySeededUserSignsOnAndIsRoutedByType() {
        for (String line : seedRecords) {
            SecUserData user = SecUserData.fromRecord(line);
            SignonOutcome out = service().run(SignonRequest.enter(user.usrId().toLowerCase(Locale.ROOT), user.pwd().toLowerCase(Locale.ROOT)));

            SignonOutcome.ControlTransferred x = assertInstanceOf(SignonOutcome.ControlTransferred.class, out,
                    user.usrId());
            String expected = "A".equals(user.type()) ? "COADM01C" : "COMEN01C";
            assertEquals(expected, x.program(), user.usrId());
            assertEquals(user.usrId(), x.commarea().userId());
            assertEquals(user.type(), x.commarea().userType());
        }
    }

    @Test
    void seededUserWithWrongPasswordIsRejected() {
        SignonOutcome.ScreenSent s = assertInstanceOf(SignonOutcome.ScreenSent.class,
                service().run(SignonRequest.enter("USER0003", "NOTRIGHT")));
        assertTrue(s.map().errmsgO().startsWith("Wrong Password. Try again ..."));
    }
}
