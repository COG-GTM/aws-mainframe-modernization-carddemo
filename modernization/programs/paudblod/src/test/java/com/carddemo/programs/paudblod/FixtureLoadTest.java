package com.carddemo.programs.paudblod;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.paudblod.copybook.CobolFields;
import com.carddemo.programs.paudblod.copybook.PautbPcb;
import com.carddemo.programs.paudblod.copybook.PendingAuthDetails;
import com.carddemo.programs.paudblod.copybook.PendingAuthSummary;
import com.carddemo.programs.paudblod.ims.InMemoryPautDatabase;
import com.carddemo.programs.paudblod.io.FixedWidthFileReader;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Loads the CardDemo sample IMS unload of DBPAUTP0
 * (app/app-authorization-ims-db2-mq/data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat, RECFM=VB,
 * segment data at offset 39 of each record) and replays it through PAUDBLOD in the
 * INFILE1/INFILE2 layout that PAUDBUNL writes.
 */
class FixtureLoadTest {

    private static final int SEGMENT_DATA_OFFSET = 39;
    private static final Map<String, List<byte[]>> SOURCE = new LinkedHashMap<>();
    private static final List<byte[]> ROOTS = new ArrayList<>();

    @TempDir
    Path tmp;

    @BeforeAll
    static void parseUnload() throws IOException {
        byte[] d;
        try (InputStream in = FixtureLoadTest.class.getResourceAsStream(
                "/fixtures/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat")) {
            d = in.readAllBytes();
        }
        String currentRoot = null;
        for (int i = 0; i < d.length; ) {
            int len = ((d[i] & 0xFF) << 8) | (d[i + 1] & 0xFF);
            int segCode = d[i + 4];
            if (segCode == 1) {
                byte[] seg = Arrays.copyOfRange(d, i + SEGMENT_DATA_OFFSET, i + SEGMENT_DATA_OFFSET + 100);
                ROOTS.add(seg);
                currentRoot = CobolFields.hex(seg, 0, 6);
                SOURCE.put(currentRoot, new ArrayList<>());
            } else if (segCode == 2) {
                SOURCE.get(currentRoot).add(
                        Arrays.copyOfRange(d, i + SEGMENT_DATA_OFFSET, i + SEGMENT_DATA_OFFSET + 200));
            }
            i += len;
        }
    }

    /** Writes INFILE1 (LRECL 100) and INFILE2 (LRECL 206) exactly as PAUDBUNL does. */
    private Path[] writeUnloadFiles() throws IOException {
        ByteArrayOutputStream f1 = new ByteArrayOutputStream();
        ByteArrayOutputStream f2 = new ByteArrayOutputStream();
        for (byte[] root : ROOTS) {
            f1.writeBytes(root);
            for (byte[] child : SOURCE.get(CobolFields.hex(root, 0, 6))) {
                f2.writeBytes(Arrays.copyOf(root, Paudblod.ROOT_SEG_KEY_LENGTH));
                f2.writeBytes(child);
            }
        }
        Path p1 = Files.write(tmp.resolve("PAUTDB.ROOT.FILEO"), f1.toByteArray());
        Path p2 = Files.write(tmp.resolve("PAUTDB.CHILD.FILEO"), f2.toByteArray());
        return new Path[] {p1, p2};
    }

    private int load(InMemoryPautDatabase db, Path[] files, List<String> out) {
        return new Paudblod(
                new FixedWidthFileReader(files[0], Paudblod.INFIL1_REC_LENGTH),
                new FixedWidthFileReader(files[1], Paudblod.INFIL2_REC_LENGTH),
                db, new PautbPcb("DBPAUTP0", "A", 2), Clock.systemUTC(), Paudblod.EBCDIC, out::add).run();
    }

    @Test
    void sampleUnloadContains22RootsAnd202Children() {
        assertEquals(22, ROOTS.size());
        // The last root has a non-numeric (spaces) ACCNTID and no children.
        assertEquals(List.of(), SOURCE.get("404040404040"));
        assertEquals(202, SOURCE.values().stream().mapToInt(List::size).sum());
        PendingAuthSummary first = PendingAuthSummary.fromBytes(ROOTS.get(0), Paudblod.EBCDIC);
        assertEquals(BigDecimal.ONE, first.paAcctId());
        assertEquals(1L, first.paCustId());
        PendingAuthDetails d = PendingAuthDetails.fromBytes(SOURCE.get("00000000001C").get(0), Paudblod.EBCDIC);
        assertEquals(new BigDecimal("76699"), d.paAuthDate9c());
        assertEquals("231027", d.paAuthOrigDate());
    }

    @Test
    void loadsEverySegmentByteForByte() throws IOException {
        InMemoryPautDatabase db = new InMemoryPautDatabase((byte) 0x40);
        List<String> out = new ArrayList<>();

        int rc = load(db, writeUnloadFiles(), out);

        assertEquals(0, rc);
        assertEquals(22, db.rootCount());
        assertEquals(202, db.childCount());
        assertEquals(22, out.stream().filter("ROOT INSERT SUCCESS    "::equals).count());
        assertEquals(202, out.stream().filter("CHILD SEGMENT INSERTED SUCCESS"::equals).count());
        assertEquals("CLOSING THE FILE", out.get(out.size() - 1));

        Map<String, List<byte[]>> loaded = db.childSegmentsByRoot();
        for (byte[] root : ROOTS) {
            String key = CobolFields.hex(root, 0, 6);
            assertTrue(db.rootSegments().stream().anyMatch(r -> Arrays.equals(r, root)), key);
            List<byte[]> expected = new ArrayList<>(SOURCE.get(key));
            expected.sort(Arrays::compareUnsigned);
            List<byte[]> actual = loaded.get(key);
            assertEquals(expected.size(), actual.size(), key);
            for (int i = 0; i < expected.size(); i++) {
                assertArrayEquals(expected.get(i), actual.get(i), key + "[" + i + "]");
            }
        }
    }

    @Test
    void reloadingTheSameFilesReportsDuplicatesAndStillEndsRcZero() throws IOException {
        InMemoryPautDatabase db = new InMemoryPautDatabase((byte) 0x40);
        Path[] files = writeUnloadFiles();
        load(db, files, new ArrayList<>());
        List<String> out = new ArrayList<>();

        int rc = load(db, files, out);

        assertEquals(0, rc);
        assertEquals(22, out.stream().filter("ROOT SEGMENT ALREADY IN DB"::equals).count());
        assertEquals(202, out.stream().filter("CHILD SEGMENT ALREADY IN DB"::equals).count());
        assertEquals(202, db.childCount());
    }

    @Test
    void missingInputFileAbendsWithReturnCode16() {
        List<String> out = new ArrayList<>();
        int rc = load(new InMemoryPautDatabase((byte) 0x40),
                new Path[] {tmp.resolve("missing1"), tmp.resolve("missing2")}, out);

        assertEquals(16, rc);
        assertTrue(out.contains("ERROR IN OPENING INFILE1:35"));
    }

    @Test
    void shortTrailingRecordIsReportedAsReadError() throws IOException {
        Path[] files = writeUnloadFiles();
        byte[] f1 = Files.readAllBytes(files[0]);
        Files.write(files[0], Arrays.copyOf(f1, f1.length + 10));
        List<String> out = new ArrayList<>();

        int rc = load(new InMemoryPautDatabase((byte) 0x40), files, out);

        assertEquals(0, rc);
        assertEquals(1, out.stream().filter("ERROR READING ROOT SEG INFILE"::equals).count());
    }
}
