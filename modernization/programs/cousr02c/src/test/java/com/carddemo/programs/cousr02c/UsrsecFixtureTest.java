package com.carddemo.programs.cousr02c;

import com.carddemo.programs.cousr02c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr02c.copybook.Dfhaid;
import com.carddemo.programs.cousr02c.copybook.Pic;
import com.carddemo.programs.cousr02c.copybook.SecUserData;
import com.carddemo.programs.cousr02c.gateway.InMemoryUserSecurityFile;
import com.carddemo.programs.cousr02c.gateway.UsrsecRecordReader;
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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

/** Runs the program against the repository's USRSEC sample unload (app/data/EBCDIC, EBCDIC cp037, LRECL 80). */
class UsrsecFixtureTest {

    private static final Path USRSEC = Path.of("../../../app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS");
    private static final Charset EBCDIC = Charset.forName("IBM037");

    private byte[] raw;
    private List<SecUserData> records;

    @BeforeEach
    void load() throws IOException {
        raw = Files.readAllBytes(USRSEC);
        records = UsrsecRecordReader.read(raw, EBCDIC);
    }

    @Test
    void fixtureParsesPerCopybookLayout() {
        assertEquals(10, records.size());
        assertEquals("ADMIN001", records.get(0).getSecUsrId());
        assertEquals("A", records.get(0).getSecUsrType());
        assertEquals("USER0005", records.get(9).getSecUsrId());
        assertEquals("U", records.get(9).getSecUsrType());
        assertEquals("PASSWORD", records.get(9).getSecUsrPwd());
        assertEquals(Pic.spaces(23), records.get(9).getSecUsrFiller());
        assertArrayEquals(raw, UsrsecRecordReader.write(records, EBCDIC));
    }

    @Test
    void fetchAndUpdateUserFromFixture() {
        InMemoryUserSecurityFile file = new InMemoryUserSecurityFile(records);
        Cousr02cService service = new Cousr02cService(file,
                Clock.fixed(Instant.parse("2022-07-19T23:12:34Z"), ZoneOffset.UTC));
        SecUserData before = file.find("USER0003").orElseThrow();

        CardDemoCommarea selected = CardDemoCommarea.blank();
        selected.setCdemoFromProgram("COUSR00C");
        selected.setCdemoCu02UsrSelected("USER0003");
        Cousr02cResponse first = service.execute(Cousr02cRequest.firstEntry(selected));
        ScreenOutput shown = first.finalScreen();
        assertEquals(before.getSecUsrFname(), shown.fname());
        assertEquals(before.getSecUsrLname(), shown.lname());
        assertEquals("07/19/22", shown.curdate());
        assertEquals("23:12:34", shown.curtime());

        Cousr02cResponse saved = service.execute(new Cousr02cRequest(first.outcome().commareaBytes(),
                Dfhaid.DFHPF5, new MapInput(shown.usrid(), shown.fname(), shown.lname(), shown.passwd(), "A")));

        assertEquals("User USER0003 has been updated ...", saved.finalScreen().message());
        SecUserData after = file.find("USER0003").orElseThrow();
        assertEquals("A", after.getSecUsrType());
        assertEquals(before.toRecord().substring(0, 56), after.toRecord().substring(0, 56));
        assertEquals(before.getSecUsrFiller(), after.getSecUsrFiller());
    }
}
