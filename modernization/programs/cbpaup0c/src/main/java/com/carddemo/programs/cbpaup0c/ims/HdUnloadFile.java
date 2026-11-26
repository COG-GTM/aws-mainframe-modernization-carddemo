package com.carddemo.programs.cbpaup0c.ims;

import com.carddemo.programs.cbpaup0c.copybook.PendingAuthDetails;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthSummary;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Reader/writer for the variable-length (RECFM=VB) HD unload of DBPAUTP0 shipped as
 * app/app-authorization-ims-db2-mq/data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat.
 *
 * <p>Each record starts with a 4-byte RDW. Segment records carry the segment length at offset 8, the segment
 * name at offset 10 and the copybook bytes at offset {@value #SEGMENT_DATA_OFFSET}. Non-segment records
 * (unload header/statistics) are kept and written back unchanged.
 */
public final class HdUnloadFile {

    static final int SEGMENT_DATA_OFFSET = 39;

    private final InMemoryPautDb database;
    private final List<byte[]> leadingRecords;
    private final List<byte[]> trailingRecords;

    private HdUnloadFile(InMemoryPautDb database, List<byte[]> leading, List<byte[]> trailing) {
        this.database = database;
        this.leadingRecords = leading;
        this.trailingRecords = trailing;
    }

    public InMemoryPautDb database() {
        return database;
    }

    public static HdUnloadFile read(InputStream in, Charset charset) throws IOException {
        byte[] all = in.readAllBytes();
        InMemoryPautDb db = new InMemoryPautDb(charset);
        List<byte[]> leading = new ArrayList<>();
        List<byte[]> trailing = new ArrayList<>();
        boolean seenSegment = false;
        int pos = 0;
        while (pos < all.length) {
            if (pos + 4 > all.length) {
                throw new IOException("Truncated RDW at offset " + pos);
            }
            int len = ((all[pos] & 0xFF) << 8) | (all[pos + 1] & 0xFF);
            if (len < 4 || pos + len > all.length) {
                throw new IOException("Invalid RDW length " + len + " at offset " + pos);
            }
            byte[] rec = Arrays.copyOfRange(all, pos, pos + len);
            pos += len;
            String name = len >= 18 ? new String(rec, 10, 8, charset) : "";
            int segLen = len >= 10 ? ((rec[8] & 0xFF) << 8) | (rec[9] & 0xFF) : 0;
            boolean fits = len >= SEGMENT_DATA_OFFSET + segLen;
            // Byte 4 is the segment code in data records and X'00' in the header/statistics records.
            boolean dataRecord = len >= 18 && rec[4] != 0;
            boolean namedSegment = PendingAuthSummary.SEGMENT_NAME.equals(name)
                    || PendingAuthDetails.SEGMENT_NAME.equals(name);
            int expectedLen = PendingAuthSummary.SEGMENT_NAME.equals(name)
                    ? PendingAuthSummary.LENGTH : PendingAuthDetails.LENGTH;
            if (dataRecord && namedSegment && !(segLen == expectedLen && fits)) {
                throw new IOException("Damaged " + name + " record at offset " + (pos - len)
                        + ": segment length " + segLen + ", record length " + len);
            }
            if (PendingAuthSummary.SEGMENT_NAME.equals(name) && segLen == PendingAuthSummary.LENGTH && fits) {
                db.addSummary(Arrays.copyOfRange(rec, SEGMENT_DATA_OFFSET, SEGMENT_DATA_OFFSET + segLen), rec);
                seenSegment = true;
                trailing.clear();
            } else if (PendingAuthDetails.SEGMENT_NAME.equals(name) && segLen == PendingAuthDetails.LENGTH && fits) {
                db.addDetail(Arrays.copyOfRange(rec, SEGMENT_DATA_OFFSET, SEGMENT_DATA_OFFSET + segLen), rec);
                seenSegment = true;
                trailing.clear();
            } else if (seenSegment) {
                trailing.add(rec);
            } else {
                leading.add(rec);
            }
        }
        return new HdUnloadFile(db, leading, trailing);
    }

    /** Writes the header records, every surviving segment (original unload record) and the trailer records. */
    public void write(OutputStream out) throws IOException {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        for (byte[] r : leadingRecords) {
            buf.write(r);
        }
        for (InMemoryPautDb.Root root : database.roots()) {
            if (root.summary().deleted()) {
                continue;
            }
            buf.write(requireSource(root.summary()));
            for (InMemoryPautDb.Segment d : root.details()) {
                if (!d.deleted()) {
                    buf.write(requireSource(d));
                }
            }
        }
        for (byte[] r : trailingRecords) {
            buf.write(r);
        }
        out.write(buf.toByteArray());
    }

    private static byte[] requireSource(InMemoryPautDb.Segment s) {
        byte[] src = s.sourceRecord();
        if (src == null) {
            throw new IllegalStateException("Segment was not loaded from an unload file; cannot write it back");
        }
        return src;
    }
}
