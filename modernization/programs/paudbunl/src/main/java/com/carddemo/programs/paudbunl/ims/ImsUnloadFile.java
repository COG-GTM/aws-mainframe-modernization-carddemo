package com.carddemo.programs.paudbunl.ims;

import com.carddemo.programs.paudbunl.support.CobolData;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

/**
 * Loads an IMS HD unload dataset of DBPAUTP0 (e.g. app/app-authorization-ims-db2-mq/data/EBCDIC/
 * AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat) into an {@link InMemoryPendingAuthDatabase}.
 *
 * <p>Record layout as found in the sample dataset: a 2-byte big-endian record length, the
 * segment data length at offset 8 (2 bytes), the EBCDIC segment name at offset 10 (8 bytes) and
 * the segment data at offset 39. Header/trailer records (other segment names) are skipped.
 */
public final class ImsUnloadFile {

    static final int SEGMENT_LENGTH_OFFSET = 8;
    static final int SEGMENT_NAME_OFFSET = 10;
    static final int SEGMENT_DATA_OFFSET = 39;

    private ImsUnloadFile() {
    }

    public static InMemoryPendingAuthDatabase load(Path path) throws IOException {
        return load(Files.readAllBytes(path));
    }

    public static InMemoryPendingAuthDatabase load(byte[] data) {
        InMemoryPendingAuthDatabase db = new InMemoryPendingAuthDatabase();
        int pos = 0;
        while (pos < data.length) {
            if (pos + 2 > data.length) {
                throw new IllegalArgumentException("Truncated record length at offset " + pos);
            }
            int recLen = (int) CobolData.decodeBinary(data, pos, 2) & 0xFFFF;
            if (recLen < 2 || pos + recLen > data.length) {
                throw new IllegalArgumentException("Invalid record length " + recLen + " at offset " + pos);
            }
            if (recLen > SEGMENT_DATA_OFFSET) {
                String name = CobolData.decodeAlpha(data, pos + SEGMENT_NAME_OFFSET, 8, CobolData.EBCDIC);
                int segLen = (int) CobolData.decodeBinary(data, pos + SEGMENT_LENGTH_OFFSET, 2) & 0xFFFF;
                if (InMemoryPendingAuthDatabase.ROOT_SEGMENT.equals(name)
                        || InMemoryPendingAuthDatabase.CHILD_SEGMENT.equals(name)) {
                    int start = pos + SEGMENT_DATA_OFFSET;
                    if (start + segLen > pos + recLen) {
                        throw new IllegalArgumentException("Segment " + name + " overruns record at offset " + pos);
                    }
                    byte[] seg = Arrays.copyOfRange(data, start, start + segLen);
                    if (InMemoryPendingAuthDatabase.ROOT_SEGMENT.equals(name)) {
                        db.addRoot(seg);
                    } else {
                        db.addChild(seg);
                    }
                }
            }
            pos += recLen;
        }
        return db;
    }
}
