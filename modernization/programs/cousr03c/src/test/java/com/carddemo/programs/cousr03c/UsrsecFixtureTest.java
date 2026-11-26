package com.carddemo.programs.cousr03c;

import com.carddemo.programs.cousr03c.copybook.Dfhaid;
import com.carddemo.programs.cousr03c.copybook.SecUserData;
import com.carddemo.programs.cousr03c.dto.Cousr03cRequest;
import com.carddemo.programs.cousr03c.dto.Cousr03cResult;
import com.carddemo.programs.cousr03c.dto.ScreenInput;
import com.carddemo.programs.cousr03c.gateway.InMemoryUserSecurityFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;

import static com.carddemo.programs.cousr03c.dto.UserDeleteScreen.text;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

/** Runs COUSR03C against the repository's sample USRSEC dataset (app/data/EBCDIC, 80-byte EBCDIC records). */
class UsrsecFixtureTest {

    private static final Path USRSEC = Path.of("../../../app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS");
    private static final Charset EBCDIC = Charset.forName("IBM037");

    private byte[] dataset;
    private InMemoryUserSecurityFile usrsec;
    private Cousr03cService service;

    @BeforeEach
    void load() throws IOException {
        assumeTrue(Files.exists(USRSEC), "sample USRSEC dataset not available");
        dataset = Files.readAllBytes(USRSEC);
        usrsec = InMemoryUserSecurityFile.load(dataset, EBCDIC);
        service = new Cousr03cService(usrsec, Clock.fixed(Instant.parse("2022-07-19T23:12:35Z"), ZoneOffset.UTC), s -> { });
    }

    private Cousr03cResult press(Dfhaid aid, String userId) {
        Cousr03cCommarea c = new Cousr03cCommarea();
        c.fill((byte) ' ');
        c.setFromProgram("COADM01C");
        c.setPgmContext(1);
        return service.execute(new Cousr03cRequest(c.toBytes(), aid, ScreenInput.userId(userId)));
    }

    @Test
    void datasetHasTenUsers() {
        assertEquals(800, dataset.length);
        assertEquals(List.of("ADMIN001", "ADMIN002", "ADMIN003", "ADMIN004", "ADMIN005",
                "USER0001", "USER0002", "USER0003", "USER0004", "USER0005"), usrsec.keys());
        SecUserData admin = usrsec.find("ADMIN001").orElseThrow();
        assertEquals("A", admin.usrType());
        assertEquals("U", usrsec.find("USER0005").orElseThrow().usrType());
    }

    @Test
    void unloadReproducesDatasetBytes() {
        assertArrayEquals(dataset, usrsec.unload(EBCDIC));
    }

    @Test
    void enterShowsSampleAdmin() {
        Cousr03cResult r = press(Dfhaid.DFHENTER, "ADMIN001");
        assertEquals("MARGARET", text(r.lastScreen().fname()));
        assertEquals("GOLD", text(r.lastScreen().lname()));
        assertEquals("A", r.lastScreen().usrType());
        assertEquals("07/19/22", r.lastScreen().curDate());
        assertEquals("23:12:35", r.lastScreen().curTime());
    }

    @Test
    void pf5DeletesSampleUserAndRewritesDataset() {
        Cousr03cResult r = press(Dfhaid.DFHPF5, "USER0003");
        assertEquals("User USER0003 has been deleted ...", text(r.lastScreen().errMsg()));
        assertFalse(usrsec.find("USER0003").isPresent());
        byte[] after = usrsec.unload(EBCDIC);
        assertEquals(720, after.length);
        byte[] expected = new byte[720];
        System.arraycopy(dataset, 0, expected, 0, 7 * 80);
        System.arraycopy(dataset, 8 * 80, expected, 7 * 80, 2 * 80);
        assertArrayEquals(expected, after);
    }
}
