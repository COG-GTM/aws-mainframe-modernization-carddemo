package com.carddemo.programs.cbpaup0c.ims;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.carddemo.programs.cbpaup0c.copybook.CobolCodec;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthSummary;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import org.junit.jupiter.api.Test;

class HdUnloadFileTest {

    static byte[] fixtureBytes() throws IOException {
        try (InputStream in = HdUnloadFileTest.class.getResourceAsStream("/fixtures/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat")) {
            return in.readAllBytes();
        }
    }

    @Test
    void readsAllSampleSegments() throws IOException {
        HdUnloadFile f = HdUnloadFile.read(new ByteArrayInputStream(fixtureBytes()), CobolCodec.EBCDIC);
        assertEquals(22, f.database().roots().size());
        assertEquals(202, f.database().roots().stream().mapToInt(r -> r.details().size()).sum());
    }

    @Test
    void unchangedDatabaseWritesBackIdenticalBytes() throws IOException {
        byte[] original = fixtureBytes();
        HdUnloadFile f = HdUnloadFile.read(new ByteArrayInputStream(original), CobolCodec.EBCDIC);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        f.write(out);
        assertArrayEquals(original, out.toByteArray());
    }

    @Test
    void deletedSegmentsAreOmitted() throws IOException {
        HdUnloadFile f = HdUnloadFile.read(new ByteArrayInputStream(fixtureBytes()), CobolCodec.EBCDIC);
        InMemoryPautDb db = f.database();
        db.getNextSummary();
        int firstRootDetails = db.roots().get(0).details().size();
        db.deleteSummary(null);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        f.write(out);
        HdUnloadFile again = HdUnloadFile.read(new ByteArrayInputStream(out.toByteArray()), CobolCodec.EBCDIC);
        assertEquals(21, again.database().roots().size());
        assertEquals(202 - firstRootDetails, again.database().roots().stream().mapToInt(r -> r.details().size()).sum());
    }

    @Test
    void damagedSegmentRecordIsRejectedInsteadOfReparentingItsChildren() throws IOException {
        byte[] bytes = fixtureBytes();
        bytes[88 + 9] = 99; // first PAUTSUM0 data record (after the 88-byte header): segment length 100 -> 99
        IOException e = assertThrows(IOException.class,
                () -> HdUnloadFile.read(new ByteArrayInputStream(bytes), CobolCodec.EBCDIC));
        assertEquals("Damaged PAUTSUM0 record at offset 88: segment length 99, record length 140", e.getMessage());
        bytes[88 + 9] = (byte) 150; // longer than the record
        assertThrows(IOException.class, () -> HdUnloadFile.read(new ByteArrayInputStream(bytes), CobolCodec.EBCDIC));
    }

    @Test
    void segmentRecordWithDamagedNameIsRejected() throws IOException {
        byte[] bytes = fixtureBytes();
        bytes[88 + 10] = 0x00; // 'P' of PAUTSUM0 in the first data record
        IOException e = assertThrows(IOException.class,
                () -> HdUnloadFile.read(new ByteArrayInputStream(bytes), CobolCodec.EBCDIC));
        assertEquals("Damaged segment record at offset 88: unknown segment code 1", e.getMessage());
    }

    @Test
    void invalidRdwIsRejected() {
        assertThrows(IOException.class,
                () -> HdUnloadFile.read(new ByteArrayInputStream(new byte[] {0x00, 0x02, 0x00, 0x00}), CobolCodec.EBCDIC));
    }

    @Test
    void segmentsAddedProgrammaticallyCannotBeWrittenBack() throws IOException {
        HdUnloadFile f = HdUnloadFile.read(new ByteArrayInputStream(new byte[0]), CobolCodec.EBCDIC);
        f.database().addSummary(new PendingAuthSummary());
        assertThrows(IllegalStateException.class, () -> f.write(new ByteArrayOutputStream()));
    }
}
