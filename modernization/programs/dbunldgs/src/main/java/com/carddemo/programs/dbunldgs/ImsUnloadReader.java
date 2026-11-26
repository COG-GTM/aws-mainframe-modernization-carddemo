package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.CobolCodec;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Loads an IMS HD unload file (variable-length records with a 4-byte RDW, as in
 * app/app-authorization-ims-db2-mq/data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat) into an
 * {@link InMemoryPautDatabase}. Segment records carry the segment length at offset 8, the segment
 * name at offset 10 and the segment data at offset 39; header/trailer records are skipped.
 */
public final class ImsUnloadReader {

    static final int SEG_LEN_OFFSET = 8;
    static final int SEG_NAME_OFFSET = 10;
    static final int SEG_DATA_OFFSET = 39;

    private ImsUnloadReader() {
    }

    public static InMemoryPautDatabase load(Path path, Charset charset) throws IOException {
        return load(Files.readAllBytes(path), charset);
    }

    public static InMemoryPautDatabase load(byte[] data, Charset charset) {
        InMemoryPautDatabase db = new InMemoryPautDatabase(charset);
        byte[] root = null;
        List<byte[]> children = new ArrayList<>();
        int pos = 0;
        while (pos + 4 <= data.length) {
            int len = ((data[pos] & 0xFF) << 8) | (data[pos + 1] & 0xFF);
            if (len < 4 || pos + len > data.length) {
                throw new IllegalArgumentException("Corrupt unload record at offset " + pos);
            }
            if (len >= SEG_DATA_OFFSET) {
                String name = CobolCodec.decodeAlpha(data, pos + SEG_NAME_OFFSET, 8, charset);
                int segLen = ((data[pos + SEG_LEN_OFFSET] & 0xFF) << 8) | (data[pos + SEG_LEN_OFFSET + 1] & 0xFF);
                if ("PAUTSUM0".equals(name) || "PAUTDTL1".equals(name)) {
                    if (SEG_DATA_OFFSET + segLen > len) {
                        throw new IllegalArgumentException("Segment " + name + " length " + segLen
                                + " exceeds unload record at offset " + pos);
                    }
                    byte[] seg = new byte[segLen];
                    System.arraycopy(data, pos + SEG_DATA_OFFSET, seg, 0, segLen);
                    if ("PAUTSUM0".equals(name)) {
                        if (root != null) {
                            db.addRecord(root, children);
                        }
                        root = seg;
                        children = new ArrayList<>();
                    } else {
                        if (root == null) {
                            throw new IllegalArgumentException("PAUTDTL1 without parent at offset " + pos);
                        }
                        children.add(seg);
                    }
                }
            }
            pos += len;
        }
        if (pos != data.length) {
            throw new IllegalArgumentException("Truncated unload record at offset " + pos);
        }
        if (root != null) {
            db.addRecord(root, children);
        }
        return db;
    }
}
