package com.carddemo.batch.exchange;

import com.carddemo.common.codec.Copybook;
import com.carddemo.common.codec.Field;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Record-level port of CBEXPORT/CBIMPORT over the {@code CVEXPORT} layout (500 bytes): the export record is
 * {@code INITIALIZE}d (spaces; the binary sequence number zero), stamped with type, timestamp, sequence,
 * branch {@code 0001} and region {@code NORTH}, and filled by the {@link ExportRecordType#moves()}; import moves the
 * same fields back into a fresh dataset record whose FILLER is spaces.
 */
public final class ExportRecordCodec {

    public static final RecordLayout LAYOUT = Copybook.layout("CVEXPORT");
    public static final String BRANCH_ID = "0001";
    public static final String REGION_CODE = "NORTH";
    /** {@code ERROR-OUTPUT-RECORD PIC X(132)} of CBIMPORT. */
    public static final int ERROR_RECORD_LENGTH = 132;

    private static final Field REC_TYPE = LAYOUT.field("EXPORT-REC-TYPE");
    private static final Field TIMESTAMP = LAYOUT.field("EXPORT-TIMESTAMP");
    private static final Field SEQUENCE = LAYOUT.field("EXPORT-SEQUENCE-NUM");
    private static final Field BRANCH = LAYOUT.field("EXPORT-BRANCH-ID");
    private static final Field REGION = LAYOUT.field("EXPORT-REGION-CODE");
    private static final DateTimeFormatter EXPORT_TS = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    private static final DateTimeFormatter CURRENT_DATE = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

    private ExportRecordCodec() {
    }

    /**
     * {@code WS-FORMATTED-TIMESTAMP} of CBEXPORT {@code 1050-GENERATE-TIMESTAMP}: {@code YYYY-MM-DD HH:MM:SS.hh},
     * hundredths from the clock.
     */
    public static String timestamp(LocalDateTime now) {
        return now.format(EXPORT_TS) + "." + String.format("%02d", now.getNano() / 10_000_000);
    }

    /**
     * CBEXPORT {@code 2200-CREATE-CUSTOMER-EXP-REC}, {@code 3200-CREATE-ACCOUNT-EXP-REC},
     * {@code 4200-CREATE-XREF-EXPORT-RECORD}, {@code 5200-CREATE-TRAN-EXP-REC}, {@code 5700-CREATE-CARD-EXPORT-RECORD}:
     * header fields, then the type's field moves.
     */
    public static FixedWidthRecord export(ExportRecordType type, FixedWidthRecord source, String timestamp,
                                          long sequence) {
        FixedWidthRecord out = FixedWidthRecord.spaces(LAYOUT, source.encoding());
        out.moveString(REC_TYPE, type.code());
        out.moveString(TIMESTAMP, timestamp);
        out.setLong(SEQUENCE, sequence);
        out.moveString(BRANCH, BRANCH_ID);
        out.moveString(REGION, REGION_CODE);
        for (ExportRecordType.Move m : type.moves()) {
            move(source, m.datasetField(), out, m.exportField());
        }
        return out;
    }

    /**
     * CBIMPORT {@code 2300-PROCESS-CUSTOMER-RECORD}, {@code 2400-PROCESS-ACCOUNT-RECORD},
     * {@code 2500-PROCESS-XREF-RECORD}, {@code 2600-PROCESS-TRAN-RECORD}, {@code 2650-PROCESS-CARD-RECORD}: the
     * type's field moves back into the dataset layout.
     */
    public static FixedWidthRecord importRecord(ExportRecordType type, FixedWidthRecord export) {
        FixedWidthRecord out = FixedWidthRecord.spaces(type.datasetLayout(), export.encoding());
        for (ExportRecordType.Move m : type.moves()) {
            move(export, m.exportField(), out, m.datasetField());
        }
        return out;
    }

    public static String recordType(FixedWidthRecord export) {
        return export.getString(REC_TYPE);
    }

    public static long sequence(FixedWidthRecord export) {
        return export.getLong(SEQUENCE);
    }

    /**
     * {@code WS-ERROR-RECORD} of CBIMPORT {@code 2700-PROCESS-UNKNOWN-RECORD} / {@code 2750-WRITE-ERROR}: {@code
     * FUNCTION CURRENT-DATE} (21 characters in
     * a 26-byte field), record type, sequence ({@code PIC 9(07)}, high-order digits truncated) and message, pipe
     * separated, written into the 132-byte error record.
     */
    public static FixedWidthRecord error(FixedWidthRecord export, String message, ZonedDateTime now) {
        int offsetMinutes = now.getOffset().getTotalSeconds() / 60;
        String currentDate = now.format(CURRENT_DATE) + String.format("%02d%s%02d%02d", now.getNano() / 10_000_000,
                offsetMinutes < 0 ? "-" : "+", Math.abs(offsetMinutes) / 60, Math.abs(offsetMinutes) % 60);
        String line = pad(currentDate, 26) + "|" + pad(recordType(export), 1) + "|"
                + String.format("%07d", sequence(export) % 10_000_000) + "|" + pad(message, 50);
        RecordEncoding encoding = export.encoding();
        return new FixedWidthRecord(encoding.encode(pad(line, ERROR_RECORD_LENGTH)), encoding);
    }

    /** {@code MOVE from TO to} between two items of the same class (both numeric or both alphanumeric). */
    private static void move(FixedWidthRecord from, Field f, FixedWidthRecord to, Field t) {
        if (t.isNumeric()) {
            to.moveDecimal(t, from.getDecimal(f));
        } else {
            to.moveString(t, from.getString(f));
        }
    }

    private static String pad(String s, int width) {
        return s.length() >= width ? s.substring(0, width) : s + " ".repeat(width - s.length());
    }
}
