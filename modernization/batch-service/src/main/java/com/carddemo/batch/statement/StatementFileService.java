package com.carddemo.batch.statement;

import com.carddemo.common.record.CobolRecord;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * {@code CBSTM03B}, the subprogram CBSTM03A calls for all its file access ({@code LK-M03B-AREA}): sequential
 * reads of TRNXFILE and XREFFILE ({@code 'R'}) and keyed reads of CUSTFILE and ACCTFILE ({@code 'K'}), each
 * answering with the two-character file status. Open and close always succeed on the in-memory files
 * ({@code '00'}); the {@code 'W'} / {@code 'Z'} operations are declared in the COBOL but not implemented.
 */
public final class StatementFileService {

    public enum Dd { TRNXFILE, XREFFILE, CUSTFILE, ACCTFILE }

    /** {@code LK-M03B-RC} and the record moved to {@code LK-M03B-FLDT} (null when none was read). */
    public record Result(String status, CobolRecord record) {
        public boolean ok() {
            return "00".equals(status) || "04".equals(status);
        }
    }

    private final Iterator<CobolRecord> trnx;
    private final Iterator<CobolRecord> xref;
    private final Map<String, CobolRecord> customers = new HashMap<>();
    private final Map<String, CobolRecord> accounts = new HashMap<>();

    /** Files in key order; customer keys are FD-CUST-ID (first 9 bytes), account keys FD-ACCT-ID (first 11). */
    public StatementFileService(List<CobolRecord> trnxFile, List<CobolRecord> xrefFile,
                                List<CobolRecord> custFile, List<CobolRecord> acctFile) {
        this.trnx = trnxFile.iterator();
        this.xref = xrefFile.iterator();
        custFile.forEach(r -> customers.put(key(r, 9), r));
        acctFile.forEach(r -> accounts.put(key(r, 11), r));
    }

    /** {@code 'R'}: next record of a sequential file, status {@code '10'} at end of file. */
    public Result read(Dd dd) {
        Iterator<CobolRecord> file = switch (dd) {
            case TRNXFILE -> trnx;
            case XREFFILE -> xref;
            default -> throw new IllegalArgumentException(dd + " is read by key");
        };
        return file.hasNext() ? new Result("00", file.next()) : new Result("10", null);
    }

    /** {@code 'K'}: {@code MOVE LK-M03B-KEY (1:LK-M03B-KEY-LN) TO} the record key, then READ; {@code '23'} when absent. */
    public Result readKey(Dd dd, String key) {
        Map<String, CobolRecord> file = switch (dd) {
            case CUSTFILE -> customers;
            case ACCTFILE -> accounts;
            default -> throw new IllegalArgumentException(dd + " is read sequentially");
        };
        CobolRecord record = file.get(key);
        return record == null ? new Result("23", null) : new Result("00", record);
    }

    private static String key(CobolRecord record, int length) {
        return record.codePage().decodeText(record.toBytes(), 0, length);
    }
}
