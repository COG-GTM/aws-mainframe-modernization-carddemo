package com.carddemo.programs.paudbunl.ims;

import com.carddemo.programs.paudbunl.TestSegments;
import com.carddemo.programs.paudbunl.support.CobolData;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ImsUnloadFileTest {

    private static byte[] record(String segName, byte[] data) {
        byte[] r = new byte[ImsUnloadFile.SEGMENT_DATA_OFFSET + data.length + 1];
        CobolData.encodeBinary(r.length, r, 0, 2);
        CobolData.encodeBinary(data.length, r, ImsUnloadFile.SEGMENT_LENGTH_OFFSET, 2);
        CobolData.encodeAlpha(segName, r, ImsUnloadFile.SEGMENT_NAME_OFFSET, 8, CobolData.EBCDIC);
        System.arraycopy(data, 0, r, ImsUnloadFile.SEGMENT_DATA_OFFSET, data.length);
        return r;
    }

    @Test
    void parsesRootsAndChildrenAndSkipsOtherRecords() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        byte[] header = new byte[88];
        CobolData.encodeBinary(88, header, 0, 2);
        out.write(header);
        out.write(record("PAUTSUM0", TestSegments.summaryBytes(1)));
        out.write(record("PAUTDTL1", TestSegments.detailsBytes(1, 1)));
        out.write(record("PAUTDTL1", TestSegments.detailsBytes(2, 2)));
        out.write(record("PAUTSUM0", TestSegments.summaryBytes(2)));
        out.write(header);
        InMemoryPendingAuthDatabase db = ImsUnloadFile.load(out.toByteArray());
        assertEquals(2, db.rootCount());
        assertEquals(2, db.childCount());
    }

    @Test
    void rejectsCorruptLengths() {
        assertThrows(IllegalArgumentException.class, () -> ImsUnloadFile.load(new byte[] {0x00, 0x50, 0x00}));
        assertThrows(IllegalArgumentException.class, () -> ImsUnloadFile.load(new byte[] {0x00}));
    }
}
