package com.carddemo.programs.cousr00c;

import static com.carddemo.programs.cousr00c.Fixtures.trim;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cousr00c.copybook.Dfhaid;
import com.carddemo.programs.cousr00c.copybook.SecUserData;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Drives COUSR00C against the repo's USRSEC sample unload. No USRSEC file exists under
 * app/data/ASCII, so the EBCDIC unload (10 x 80-byte records) is read in place, decoded as IBM037.
 */
class UsrsecFixtureTest {

    static final Path USRSEC = Path.of("../../../app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS");

    private static List<SecUserData> sample() throws IOException {
        return UserSecRecordReader.read(USRSEC, Cobol.EBCDIC);
    }

    @Test
    void sampleUnloadHasTenEightyByteRecords() throws IOException {
        assertEquals(800, Files.size(USRSEC));
        List<SecUserData> users = sample();
        assertEquals(10, users.size());
        assertEquals(5, users.stream().filter(u -> u.secUsrType().equals("A")).count());
        assertEquals(5, users.stream().filter(u -> u.secUsrType().equals("U")).count());
        users.forEach(u -> assertEquals(SecUserData.RECORD_LENGTH, u.toFixedWidth().length()));
    }

    @Test
    void firstEntryListsAllTenSampleUsersThenReportsBottom() throws IOException {
        InMemoryUserSecurityFile file = new InMemoryUserSecurityFile(sample());
        Cousr00cResponse r = new Cousr00cService(file, Fixtures.CLOCK).execute(Fixtures.firstEntry());
        Cousr0aScreen s = r.screen();
        List<String> ids = s.rows().stream().map(Cousr0aScreen.Row::usrid).toList();
        assertEquals(List.of("ADMIN001", "ADMIN002", "ADMIN003", "ADMIN004", "ADMIN005",
                "USER0001", "USER0002", "USER0003", "USER0004", "USER0005"), ids);
        assertEquals(List.of("A", "A", "A", "A", "A", "U", "U", "U", "U", "U"),
                s.rows().stream().map(Cousr0aScreen.Row::utype).toList());
        List<SecUserData> data = sample();
        for (int i = 0; i < 10; i++) {
            assertEquals(data.get(i).secUsrFname(), s.rows().get(i).fname());
            assertEquals(data.get(i).secUsrLname(), s.rows().get(i).lname());
        }
        assertEquals("You have reached the bottom of the page...", trim(s.errmsg()));
        assertEquals("00000001", s.pagenum());
        assertEquals("ADMIN001", r.commarea().cu00().getUsridFirst());
        assertEquals("USER0005", r.commarea().cu00().getUsridLast());
        assertTrue(r.commarea().cu00().isNextPageNo());
    }

    @Test
    void searchAndSelectFromSample() throws IOException {
        Cousr00cService service = new Cousr00cService(new InMemoryUserSecurityFile(sample()), Fixtures.CLOCK);
        Cousr00cResponse first = service.execute(Fixtures.firstEntry());
        Cousr00cResponse search = service.execute(Fixtures.next(first, Dfhaid.DFHENTER,
                Cousr0aInput.echo(first.screen()).withUsridin("USER")));
        assertEquals("USER0001", search.screen().rows().get(0).usrid());
        assertEquals("USER0005", search.screen().rows().get(4).usrid());
        assertEquals("        ", search.screen().rows().get(5).usrid());
        Cousr00cResponse del = service.execute(Fixtures.next(search, Dfhaid.DFHENTER,
                Cousr0aInput.echo(search.screen()).withSelection(2, "d")));
        assertEquals("COUSR03C", del.target());
        assertEquals("USER0002", del.commarea().cu00().getUsrSelected());
    }

    @Test
    void readerRejectsPartialRecords(@TempDir Path dir) throws IOException {
        Path bad = dir.resolve("bad.ps");
        Files.write(bad, new byte[81]);
        assertThrows(IOException.class, () -> UserSecRecordReader.read(bad, Cobol.EBCDIC));
    }
}
