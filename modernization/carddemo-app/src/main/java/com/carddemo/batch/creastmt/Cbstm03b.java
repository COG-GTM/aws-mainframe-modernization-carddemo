package com.carddemo.batch.creastmt;

import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.customer.CustomerRecord;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * {@code CBSTM03B}: the file service CBSTM03A calls for its four input KSDSs, keyed like the COBOL by DD name and
 * operation code ({@code LK-M03B-OPER}). The return code is the DD's FILE STATUS after the operation:
 * <ul>
 * <li>{@code TRNXFILE} (TRXFL, sequential): {@code O}, {@code R}, {@code C}.</li>
 * <li>{@code XREFFILE} (CARDXREF, sequential): {@code O}, {@code R}, {@code C}.</li>
 * <li>{@code CUSTFILE} (CUSTDATA, random): {@code O}, {@code K} (key = the first {@code keyLength} key bytes, 9),
 * {@code C}.</li>
 * <li>{@code ACCTFILE} (ACCTDATA, random): {@code O}, {@code K} (11 key bytes), {@code C}.</li>
 * </ul>
 * An operation a DD does not implement ({@code W}/{@code Z} anywhere, {@code K} on a sequential DD, {@code R} on a
 * random one) does no I/O and returns the DD's last FILE STATUS, as the COBOL falls through to {@code n900-EXIT}
 * ({@code MOVE <dd>-STATUS TO LK-M03B-RC}); before the first I/O that status is spaces. Rules doc CBSTM03B.md.
 */
public final class Cbstm03b {

    public static final String PROGRAM = "CBSTM03B";
    public static final String TRNXFILE = "TRNXFILE";
    public static final String XREFFILE = "XREFFILE";
    public static final String CUSTFILE = "CUSTFILE";
    public static final String ACCTFILE = "ACCTFILE";
    static final String NO_STATUS = "  ";

    /** {@code LK-M03B-OPER} and its level-88 names. */
    public enum Operation {
        OPEN('O'), CLOSE('C'), READ('R'), READ_KEY('K'), WRITE('W'), REWRITE('Z');

        private final char code;

        Operation(char code) {
            this.code = code;
        }

        public char code() {
            return code;
        }

        public static Operation of(char code) {
            return Arrays.stream(values()).filter(o -> o.code == code).findFirst()
                    .orElseThrow(() -> new IllegalArgumentException("unknown CBSTM03B operation '" + code + "'"));
        }
    }

    /** {@code LK-M03B-RC} and the record read ({@code LK-M03B-FLDT}), empty when nothing was read. */
    public record Response(String returnCode, Optional<FixedWidthRecord> record) {

        public boolean is(String code) {
            return returnCode.equals(code);
        }
    }

    private final KsdsInput trnxfile;
    private final KsdsInput xreffile;
    private final KeyedDataset<Integer, CustomerRecord> custfile;
    private final KeyedDataset<Long, AccountRecord> acctfile;
    private final RecordEncoding encoding;
    private final Map<String, String> status = new HashMap<>();

    public Cbstm03b(KsdsInput trnxfile, KsdsInput xreffile, KeyedDataset<Integer, CustomerRecord> custfile,
                    KeyedDataset<Long, AccountRecord> acctfile, RecordEncoding encoding) {
        this.trnxfile = trnxfile;
        this.xreffile = xreffile;
        this.custfile = custfile;
        this.acctfile = acctfile;
        this.encoding = encoding;
    }

    /** Operations without a key ({@code O}, {@code R}, {@code C}). */
    public Response call(String dd, Operation operation) {
        return call(dd, operation, "", 0);
    }

    /**
     * One {@code CALL 'CBSTM03B'}: {@code dd} selects the file ({@code EVALUATE LK-M03B-DD}), {@code key} and
     * {@code keyLength} are {@code LK-M03B-KEY} / {@code LK-M03B-KEY-LN} for {@code K}.
     */
    public Response call(String dd, Operation operation, String key, int keyLength) {
        return switch (dd) {
            case TRNXFILE -> sequential(TRNXFILE, trnxfile, operation);
            case XREFFILE -> sequential(XREFFILE, xreffile, operation);
            case CUSTFILE -> keyed(CUSTFILE, custfile, CustomerRecord.MAPPER, Integer::valueOf, operation, key,
                    keyLength);
            case ACCTFILE -> keyed(ACCTFILE, acctfile, AccountRecord.MAPPER, Long::valueOf, operation, key,
                    keyLength);
            // WHEN OTHER GO TO 9999-GOBACK: the COBOL returns without touching the area; no caller does this.
            default -> throw new IllegalArgumentException(PROGRAM + ": no file for DD '" + dd + "'");
        };
    }

    private Response sequential(String dd, KsdsInput file, Operation operation) {
        Optional<FixedWidthRecord> record = Optional.empty();
        try {
            switch (operation) {
                case OPEN -> {
                    file.open();
                    status.put(dd, FileStatus.SUCCESS.code());
                }
                case READ -> {
                    record = file.readNext();
                    status.put(dd, (record.isPresent() ? FileStatus.SUCCESS : FileStatus.END_OF_FILE).code());
                }
                case CLOSE -> {
                    file.close();
                    status.put(dd, FileStatus.SUCCESS.code());
                }
                default -> {
                }
            }
        } catch (FileStatusException e) {
            status.put(dd, e.status().code());
        }
        return new Response(status.getOrDefault(dd, NO_STATUS), record);
    }

    private <K, D extends Record> Response keyed(String dd, KeyedDataset<K, D> file, CopybookRecordMapper<D> mapper,
                                                 Function<String, K> parse, Operation operation, String key,
                                                 int keyLength) {
        Optional<FixedWidthRecord> record = Optional.empty();
        try {
            switch (operation) {
                case OPEN -> {
                    file.open();
                    status.put(dd, FileStatus.SUCCESS.code());
                }
                case READ_KEY -> {
                    // MOVE LK-M03B-KEY (1:LK-M03B-KEY-LN) TO FD-<dd>-ID; a key that is not all digits matches no
                    // record of the numeric-keyed KSDS.
                    String image = key.length() > keyLength ? key.substring(0, keyLength) : key;
                    Optional<D> found = image.isEmpty() || !image.chars().allMatch(Character::isDigit)
                            ? Optional.empty() : file.read(parse.apply(image));
                    record = found.map(d -> mapper.toRecord(d, encoding));
                    status.put(dd, (found.isPresent() ? FileStatus.SUCCESS : FileStatus.RECORD_NOT_FOUND).code());
                }
                case CLOSE -> {
                    file.close();
                    status.put(dd, FileStatus.SUCCESS.code());
                }
                default -> {
                }
            }
        } catch (FileStatusException e) {
            status.put(dd, e.status().code());
        }
        return new Response(status.getOrDefault(dd, NO_STATUS), record);
    }
}
