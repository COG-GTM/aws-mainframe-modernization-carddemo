package com.carddemo.programs.copaus1c.inmemory;

import com.carddemo.programs.copaus1c.copybook.Cobol;
import com.carddemo.programs.copaus1c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus1c.copybook.PendingAuthSummary;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

/**
 * Loads an IMS HD unload of DBPAUTP0 (e.g. AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat) into an
 * {@link InMemoryPendingAuthDb}. Each segment record carries a 2-byte segment length immediately before the
 * 8-byte EBCDIC segment name; the segment image starts 29 bytes after the start of the name. A root whose
 * ACCNTID is not valid packed decimal (e.g. the space-filled dummy root in the sample) is skipped with its children.
 */
public final class ImsUnloadReader {

    static final int DATA_OFFSET_FROM_NAME = 29;
    private static final byte[] SUM_NAME = "PAUTSUM0".getBytes(Cobol.EBCDIC);
    private static final byte[] DTL_NAME = "PAUTDTL1".getBytes(Cobol.EBCDIC);

    private ImsUnloadReader() {
    }

    public static InMemoryPendingAuthDb load(InputStream in) throws IOException {
        return load(in.readAllBytes());
    }

    public static InMemoryPendingAuthDb load(byte[] data) {
        InMemoryPendingAuthDb db = new InMemoryPendingAuthDb();
        Long currentAcct = null;
        for (int i = 2; i + 8 <= data.length; i++) {
            boolean sum = matches(data, i, SUM_NAME, PendingAuthSummary.LENGTH);
            boolean dtl = !sum && matches(data, i, DTL_NAME, PendingAuthDetails.LENGTH);
            if (!sum && !dtl) {
                continue;
            }
            int start = i + DATA_OFFSET_FROM_NAME;
            int len = sum ? PendingAuthSummary.LENGTH : PendingAuthDetails.LENGTH;
            if (start + len > data.length) {
                throw new IllegalArgumentException("Truncated IMS unload: segment at offset " + i + " needs "
                        + len + " bytes but only " + Math.max(0, data.length - start) + " remain");
            }
            byte[] seg = Arrays.copyOfRange(data, start, start + len);
            if (sum && !Cobol.isValidPacked(seg, 0, 6)) {
                currentAcct = null;
            } else if (sum) {
                PendingAuthSummary s = PendingAuthSummary.fromEbcdic(seg);
                db.addSummary(s);
                currentAcct = s.getAcctId();
            } else if (currentAcct != null) {
                db.addDetail(currentAcct, PendingAuthDetails.fromEbcdic(seg));
            }
            i = start + len - 1;
        }
        for (int i = Math.max(2, data.length - 7); i < data.length; i++) {
            if (endsInsideName(data, i, SUM_NAME, PendingAuthSummary.LENGTH)
                    || endsInsideName(data, i, DTL_NAME, PendingAuthDetails.LENGTH)) {
                throw new IllegalArgumentException("Truncated IMS unload: segment name at offset " + i
                        + " is cut off");
            }
        }
        return db;
    }

    private static boolean endsInsideName(byte[] data, int at, byte[] name, int segLen) {
        for (int k = 0; at + k < data.length; k++) {
            if (data[at + k] != name[k]) {
                return false;
            }
        }
        return (((data[at - 2] & 0xFF) << 8) | (data[at - 1] & 0xFF)) == segLen;
    }

    private static boolean matches(byte[] data, int at, byte[] name, int segLen) {
        for (int k = 0; k < 8; k++) {
            if (data[at + k] != name[k]) {
                return false;
            }
        }
        int declared = ((data[at - 2] & 0xFF) << 8) | (data[at - 1] & 0xFF);
        return declared == segLen;
    }
}
