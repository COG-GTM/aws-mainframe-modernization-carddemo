package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.CobolCodec;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthDetails;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthSummary;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Runs the port against the sample IMS unload shipped with the sub-app (data/EBCDIC). */
class DbunldgsFixtureTest {

    static final String FIXTURE = "/fixtures/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat";

    static byte[] fixtureBytes() throws IOException {
        try (InputStream in = DbunldgsFixtureTest.class.getResourceAsStream(FIXTURE)) {
            assertNotNull(in, "fixture missing");
            return in.readAllBytes();
        }
    }

    static InMemoryPautDatabase fixtureDb() throws IOException {
        return ImsUnloadReader.load(fixtureBytes(), CobolCodec.EBCDIC);
    }

    @Test
    void unloadReaderFindsAllSegments() throws IOException {
        List<InMemoryPautDatabase.DbRecord> recs = fixtureDb().records();
        assertEquals(22, recs.size());
        assertEquals(202, recs.stream().mapToInt(r -> r.details().size()).sum());
        InMemoryPautDatabase.DbRecord last = recs.get(recs.size() - 1);
        assertFalse(PendingAuthSummary.isAcctIdNumeric(last.summary()));
        assertTrue(last.details().isEmpty());
    }

    @Test
    void sampleSegmentsDecodeAndRoundTripThroughCopybookRecords() throws IOException {
        for (InMemoryPautDatabase.DbRecord rec : fixtureDb().records()) {
            if (PendingAuthSummary.isAcctIdNumeric(rec.summary())) {
                PendingAuthSummary s = PendingAuthSummary.fromBytes(rec.summary(), CobolCodec.EBCDIC);
                assertArrayEquals(rec.summary(), s.toBytes(CobolCodec.EBCDIC));
            } else {
                assertThrows(com.carddemo.programs.dbunldgs.copybook.CobolDataException.class,
                        () -> PendingAuthSummary.fromBytes(rec.summary(), CobolCodec.EBCDIC));
            }
            for (byte[] d : rec.details()) {
                assertArrayEquals(d, PendingAuthDetails.fromBytes(d, CobolCodec.EBCDIC).toBytes(CobolCodec.EBCDIC));
            }
        }
    }

    @Test
    void firstSampleRecordFieldValues() throws IOException {
        InMemoryPautDatabase.DbRecord first = fixtureDb().records().get(0);
        PendingAuthSummary s = PendingAuthSummary.fromBytes(first.summary(), CobolCodec.EBCDIC);
        assertEquals(new BigDecimal("1"), s.paAcctId());
        assertEquals(1L, s.paCustId());
        assertEquals(List.of("  ", "  ", "  ", "  ", "00"), s.paAccountStatus());
        assertEquals(new BigDecimal("2022.00"), s.paCreditLimit());
        assertEquals(new BigDecimal("1020.00"), s.paCashLimit());

        PendingAuthDetails d = PendingAuthDetails.fromBytes(first.details().get(0), CobolCodec.EBCDIC);
        assertEquals(23300, 99999 - d.paAuthDate9c().intValueExact());
        assertEquals("231027", d.paAuthOrigDate());
        assertEquals("041252", d.paAuthOrigTime());
        assertEquals("9680294154603697", d.paCardNum());
        assertEquals("Amazon.com            ", d.paMerchantName());
        assertEquals("Wilmington   ", d.paMerchantCity());
        assertEquals("DE", d.paMerchantState());
        assertEquals(PendingAuthDetails.MatchStatus.PA_MATCH_PENDING, d.matchStatus().orElseThrow());
    }

    @Test
    void programUnloadsSampleDatabaseToGsamByteForByte() throws IOException {
        InMemoryPautDatabase db = fixtureDb();
        InMemoryGsamDataset pasfl = new InMemoryGsamDataset(PendingAuthSummary.LENGTH);
        InMemoryGsamDataset padfl = new InMemoryGsamDataset(PendingAuthDetails.LENGTH);
        List<String> out = new ArrayList<>();
        Dbunldgs p = new Dbunldgs(db, pasfl, padfl, out::add, Clock.systemUTC(), CobolCodec.EBCDIC);

        assertEquals(0, p.run());

        List<byte[]> expectedRoots = new ArrayList<>();
        List<byte[]> expectedChildren = new ArrayList<>();
        for (InMemoryPautDatabase.DbRecord r : db.records()) {
            if (PendingAuthSummary.isAcctIdNumeric(r.summary())) {
                expectedRoots.add(r.summary());
                expectedChildren.addAll(r.details());
            }
        }
        assertEquals(21, pasfl.records().size());
        assertEquals(202, padfl.records().size());
        for (int i = 0; i < expectedRoots.size(); i++) {
            assertArrayEquals(expectedRoots.get(i), pasfl.records().get(i), "root " + i);
        }
        for (int i = 0; i < expectedChildren.size(); i++) {
            assertArrayEquals(expectedChildren.get(i), padfl.records().get(i), "child " + i);
        }
        assertEquals(21, out.stream().filter("CHILD SEG FLAG GE : Y"::equals).count());
        assertEquals("CLOSING THE FILE", out.get(out.size() - 1));
        assertEquals(22 + 202, p.getWsNoSumryRead());
    }

    @Test
    void batchMainWritesFixedWidthGsamFiles(@TempDir Path dir) throws IOException {
        Path in = dir.resolve("DBPAUTP0.dat");
        Files.write(in, fixtureBytes());
        Path root = dir.resolve("PASFILOP");
        Path child = dir.resolve("PADFILOP");

        assertEquals(0, DbunldgsMain.run(new String[] {in.toString(), root.toString(), child.toString()}));

        byte[] rootBytes = Files.readAllBytes(root);
        byte[] childBytes = Files.readAllBytes(child);
        assertEquals(21 * 100, rootBytes.length);
        assertEquals(202 * 200, childBytes.length);
        byte[] firstRoot = fixtureDb().records().get(0).summary();
        assertArrayEquals(firstRoot, Arrays.copyOfRange(rootBytes, 0, 100));
    }

    @Test
    void batchMainRejectsBadArguments(@TempDir Path dir) {
        assertEquals(DbunldgsMain.RC_SETUP_ERROR, DbunldgsMain.run(new String[0]));
        assertEquals(DbunldgsMain.RC_SETUP_ERROR, DbunldgsMain.run(new String[] {
                dir.resolve("missing").toString(), dir.resolve("a").toString(), dir.resolve("b").toString()}));
    }

    @Test
    void unloadReaderRejectsTruncatedTail() throws IOException {
        byte[] data = fixtureBytes();
        byte[] withPartialRdw = Arrays.copyOf(data, data.length + 2);
        withPartialRdw[data.length] = 0x00;
        withPartialRdw[data.length + 1] = 0x50;
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
                () -> ImsUnloadReader.load(withPartialRdw, CobolCodec.EBCDIC));
        assertTrue(e.getMessage().startsWith("Truncated unload record"));
        assertThrows(IllegalArgumentException.class,
                () -> ImsUnloadReader.load(Arrays.copyOf(data, data.length - 1), CobolCodec.EBCDIC));
    }

    @Test
    void unloadReaderRejectsSegmentLongerThanRecord() {
        int len = ImsUnloadReader.SEG_DATA_OFFSET + 10;
        byte[] rec = new byte[len];
        rec[0] = 0;
        rec[1] = (byte) len;
        rec[ImsUnloadReader.SEG_LEN_OFFSET + 1] = (byte) PendingAuthSummary.LENGTH;
        CobolCodec.encodeAlpha("PAUTSUM0", rec, ImsUnloadReader.SEG_NAME_OFFSET, 8, CobolCodec.EBCDIC);
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
                () -> ImsUnloadReader.load(rec, CobolCodec.EBCDIC));
        assertTrue(e.getMessage().contains("exceeds unload record"));
    }

    @Test
    void batchMainRejectsAliasedDatasets(@TempDir Path dir) throws IOException {
        Path in = dir.resolve("DBPAUTP0.dat");
        Files.write(in, fixtureBytes());
        Path out = dir.resolve("OUT");
        assertEquals(DbunldgsMain.RC_SETUP_ERROR, DbunldgsMain.run(new String[] {
                in.toString(), dir.resolve("x/../DBPAUTP0.dat").toString(), out.toString()}));
        assertEquals(DbunldgsMain.RC_SETUP_ERROR, DbunldgsMain.run(new String[] {
                in.toString(), out.toString(), out.toString()}));
        assertArrayEquals(fixtureBytes(), Files.readAllBytes(in));
        assertFalse(Files.exists(out));
    }

    @Test
    void batchMainKeepsExistingSummaryWhenDetailCannotOpen(@TempDir Path dir) throws IOException {
        Path in = dir.resolve("DBPAUTP0.dat");
        Files.write(in, fixtureBytes());
        Path root = dir.resolve("PASFILOP");
        byte[] previous = {1, 2, 3, 4};
        Files.write(root, previous);
        assertEquals(DbunldgsMain.RC_SETUP_ERROR, DbunldgsMain.run(new String[] {
                in.toString(), root.toString(), dir.resolve("missing-dir/PADFILOP").toString()}));
        assertArrayEquals(previous, Files.readAllBytes(root));
    }

    @Test
    void batchMainOverwritesExistingOutputsAndCreatesThemOwnerOnly(@TempDir Path dir) throws IOException {
        Path in = dir.resolve("DBPAUTP0.dat");
        Files.write(in, fixtureBytes());
        Path root = dir.resolve("PASFILOP");
        Path child = dir.resolve("PADFILOP");
        Files.write(root, new byte[50_000]);
        assertEquals(0, DbunldgsMain.run(new String[] {in.toString(), root.toString(), child.toString()}));
        assertEquals(21 * 100, Files.size(root));
        if (child.getFileSystem().supportedFileAttributeViews().contains("posix")) {
            assertEquals("rw-------", java.nio.file.attribute.PosixFilePermissions.toString(
                    Files.getPosixFilePermissions(child)));
        }
    }

    @Test
    void batchMainRejectsInvalidPath(@TempDir Path dir) {
        assertEquals(DbunldgsMain.RC_SETUP_ERROR, DbunldgsMain.run(new String[] {
                "bad\u0000path", dir.resolve("a").toString(), dir.resolve("b").toString()}));
    }

    @Test
    void batchMainRejectsOutputSymlinkedToNotYetCreatedOutput(@TempDir Path dir) throws IOException {
        Path in = dir.resolve("DBPAUTP0.dat");
        Files.write(in, fixtureBytes());
        Path root = dir.resolve("PASFILOP");
        Path child = dir.resolve("PADFILOP");
        try {
            Files.createSymbolicLink(child, root);
        } catch (UnsupportedOperationException | IOException e) {
            return;
        }
        assertEquals(DbunldgsMain.RC_SETUP_ERROR, DbunldgsMain.run(new String[] {
                in.toString(), root.toString(), child.toString()}));
        assertEquals(0, Files.size(root));
    }

    @Test
    void batchMainRejectsOutputHeldByAnotherJob(@TempDir Path dir) throws IOException {
        Path in = dir.resolve("DBPAUTP0.dat");
        Files.write(in, fixtureBytes());
        Path root = dir.resolve("PASFILOP");
        byte[] previous = {9, 9, 9};
        Files.write(root, previous);
        try (java.nio.channels.FileChannel other = java.nio.channels.FileChannel.open(root,
                java.nio.file.StandardOpenOption.WRITE);
             java.nio.channels.FileLock held = other.lock()) {
            assertEquals(DbunldgsMain.RC_SETUP_ERROR, DbunldgsMain.run(new String[] {
                    in.toString(), root.toString(), dir.resolve("PADFILOP").toString()}));
        }
        assertArrayEquals(previous, Files.readAllBytes(root));
    }

    @Test
    void secondAllocationInSameJvmKeepsFirstHoldersLock(@TempDir Path dir) throws IOException {
        Path root = dir.resolve("PASFILOP");
        try (FixedWidthGsamFile first = new FixedWidthGsamFile(root, PendingAuthSummary.LENGTH)) {
            assertThrows(java.io.UncheckedIOException.class,
                    () -> new FixedWidthGsamFile(root, PendingAuthSummary.LENGTH));
            try (java.nio.channels.FileChannel probe = java.nio.channels.FileChannel.open(root,
                    java.nio.file.StandardOpenOption.WRITE)) {
                assertThrows(java.nio.channels.OverlappingFileLockException.class, probe::tryLock);
            }
        }
        try (FixedWidthGsamFile again = new FixedWidthGsamFile(root, PendingAuthSummary.LENGTH)) {
            assertNotNull(again);
        }
    }
}
