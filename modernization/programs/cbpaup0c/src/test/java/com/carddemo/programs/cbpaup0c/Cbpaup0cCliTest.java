package com.carddemo.programs.cbpaup0c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cbpaup0c.copybook.CobolCodec;
import com.carddemo.programs.cbpaup0c.ims.HdUnloadFile;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class Cbpaup0cCliTest {

    @TempDir
    Path tmp;

    @Test
    void runsJclScenarioAgainstUnloadFileAndWritesSurvivors() throws Exception {
        Path in = tmp.resolve("in.dat");
        Path outFile = tmp.resolve("out.dat");
        try (InputStream fixture = getClass().getResourceAsStream(GoldenFixtureTest.FIXTURE)) {
            Files.copy(fixture, in);
        }
        ByteArrayOutputStream sysout = new ByteArrayOutputStream();
        Clock clock = Clock.fixed(Instant.parse("2026-11-26T10:00:00Z"), ZoneOffset.UTC);

        int rc = Cbpaup0c.runCli(new String[] {in.toString(), outFile.toString()},
                new ByteArrayInputStream("00,00001,00001,Y\n".getBytes(StandardCharsets.UTF_8)),
                new PrintStream(sysout, true, StandardCharsets.UTF_8), clock);

        assertEquals(0, rc);
        assertTrue(sysout.toString(StandardCharsets.UTF_8).contains("# DETAILS REC DELETED :+00000202"));
        HdUnloadFile written;
        try (InputStream w = Files.newInputStream(outFile)) {
            written = HdUnloadFile.read(w, CobolCodec.EBCDIC);
        }
        assertEquals(1, written.database().roots().size());
        assertTrue(written.database().roots().get(0).details().isEmpty());
    }

    @Test
    void inPlaceRunReplacesInputAtomicallyWithOwnerOnlyFile() throws Exception {
        Path db = tmp.resolve("db.dat");
        try (InputStream fixture = getClass().getResourceAsStream(GoldenFixtureTest.FIXTURE)) {
            Files.copy(fixture, db);
        }
        Clock clock = Clock.fixed(Instant.parse("2026-11-26T10:00:00Z"), ZoneOffset.UTC);
        int rc = Cbpaup0c.runCli(new String[] {db.toString(), db.toString()},
                new ByteArrayInputStream("00,00001,00001,N\n".getBytes(StandardCharsets.UTF_8)),
                new PrintStream(new ByteArrayOutputStream(), true, StandardCharsets.UTF_8), clock);
        assertEquals(0, rc);
        try (InputStream w = Files.newInputStream(db)) {
            assertEquals(1, HdUnloadFile.read(w, CobolCodec.EBCDIC).database().roots().size());
        }
        try (var files = Files.list(tmp)) {
            assertEquals(1, files.count(), "temp file cleaned up");
        }
        if (db.getFileSystem().supportedFileAttributeViews().contains("posix")) {
            assertEquals(PosixFilePermissions.fromString("rw-------"), Files.getPosixFilePermissions(db));
        }
    }

    @Test
    void usageErrorReturns8() throws Exception {
        ByteArrayOutputStream sysout = new ByteArrayOutputStream();
        int rc = Cbpaup0c.runCli(new String[0], new ByteArrayInputStream(new byte[0]),
                new PrintStream(sysout, true, StandardCharsets.UTF_8), Clock.systemUTC());
        assertEquals(8, rc);
    }
}
