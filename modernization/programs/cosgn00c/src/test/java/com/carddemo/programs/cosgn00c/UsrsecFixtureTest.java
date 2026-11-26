package com.carddemo.programs.cosgn00c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cosgn00c.copybook.SecUserData;

import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.Charset;
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
    void ebcdicFixedBlockSampleLoadsSameUsersAsJclSeed() throws IOException {
        Path ps = Path.of("..", "..", "..", "app", "data", "EBCDIC", "AWS.M2.CARDDEMO.USRSEC.PS");
        String fb = new String(Files.readAllBytes(ps), Charset.forName("IBM037"));
        InMemoryUserSecurityRepository fbRepo = InMemoryUserSecurityRepository.loadFixedBlock(new StringReader(fb));

        assertEquals(10, fbRepo.size());
        for (String line : seedRecords) {
            SecUserData user = SecUserData.fromRecord(line);
            assertEquals(user, fbRepo.read(user.usrId()).record(), user.usrId());
        }
    }

    @Test
    void fixedBlockLoadKeepsAlignmentWhenARecordContainsLineBreakCharacters() throws IOException {
        SecUserData first = SecUserData.of("USERA001", "AN\nN", "LEE\r", "PASSWORD", "U");
        SecUserData second = SecUserData.of("USERA002", "BOB", "RAY", "PASSWORD", "U");
        InMemoryUserSecurityRepository fbRepo = InMemoryUserSecurityRepository
                .loadFixedBlock(new StringReader(first.toRecord() + second.toRecord()));

        assertEquals(2, fbRepo.size());
        assertEquals(first, fbRepo.read("USERA001").record());
        assertEquals(second, fbRepo.read("USERA002").record());
    }

    @Test
    void fixedBlockLoadRejectsTruncatedTrailingRecord() {
        String admin = SecUserData.of("ADMIN009", "AL", "BEE", "PASSWORD", "A").toRecord();
        String truncated = admin.substring(0, 56);
        assertThrows(IOException.class, () -> InMemoryUserSecurityRepository
                .loadFixedBlock(new StringReader(admin.replace("ADMIN009", "ADMIN008") + truncated)));
    }

    @Test
    void duplicateUserIdInInputIsRejectedLikeReproIntoKsds() {
        String first = SecUserData.of("ADMIN001", "AL", "BEE", "PASSWORD", "A").toRecord();
        String dup = SecUserData.of("ADMIN001", "EVE", "X", "HIJACKED", "A").toRecord();
        assertThrows(IOException.class,
                () -> InMemoryUserSecurityRepository.load(new StringReader(first + "\n" + dup)));
        assertThrows(IOException.class,
                () -> InMemoryUserSecurityRepository.loadFixedBlock(new StringReader(first + dup)));
    }

    @Test
    void seededUserWithWrongPasswordIsRejected() {
        SignonOutcome.ScreenSent s = assertInstanceOf(SignonOutcome.ScreenSent.class,
                service().run(SignonRequest.enter("USER0003", "NOTRIGHT")));
        assertTrue(s.map().errmsgO().startsWith("Wrong Password. Try again ..."));
    }
}
