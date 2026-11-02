package com.carddemo.batch.tranrept;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordLayout;
import java.time.LocalDate;
import java.util.Arrays;

/**
 * {@code WS-DATEPARM-RECORD} of CBTRN03C: {@code WS-START-DATE X(10)}, one filler byte,
 * {@code WS-END-DATE X(10)}, read from the 80-byte DATEPARM record. The dates are compared with
 * {@code TRAN-PROC-TS(1:10)} as bytes of the record's code page, not as dates: the COBOL does not
 * validate them.
 */
public record DateParm(byte[] start, byte[] end, String startText, String endText) {

    private static final RecordLayout.Builder B = RecordLayout.builder("CBTRN03C", "FD-DATEPARM-REC");
    public static final AlphanumericField RECORD = B.alpha("FD-DATEPARM-REC", "X(80)");
    public static final RecordLayout LAYOUT = B.build(80);

    /** The record as CBTRN03C reads it; {@code text} is the record's character content. */
    public static DateParm of(String text, CodePage codePage) {
        CobolRecord record = CobolRecord.initialized(LAYOUT, codePage).set(RECORD, text);
        return of(record);
    }

    public static DateParm of(CobolRecord record) {
        byte[] bytes = record.toBytes();
        String text = record.get(RECORD);
        return new DateParm(Arrays.copyOfRange(bytes, 0, 10), Arrays.copyOfRange(bytes, 11, 21),
                text.substring(0, 10), text.substring(11, 21));
    }

    /** The record CORPT00C writes into {@code //STEP10R.DATEPARM DD *}: {@code start + ' ' + end}. */
    public static String text(LocalDate start, LocalDate end) {
        return start + " " + end;
    }

    /** {@code TRAN-PROC-TS(1:10) >= WS-START-DATE AND <= WS-END-DATE} on raw record bytes at {@code offset}. */
    public boolean includes(byte[] record, int offset) {
        return Arrays.compareUnsigned(record, offset, offset + 10, start, 0, 10) >= 0
                && Arrays.compareUnsigned(record, offset, offset + 10, end, 0, 10) <= 0;
    }
}
