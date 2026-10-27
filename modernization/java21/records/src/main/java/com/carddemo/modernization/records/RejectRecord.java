package com.carddemo.modernization.records;

import com.carddemo.modernization.records.codec.RecordCodec;
import com.carddemo.modernization.records.codec.RecordFields;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import java.nio.charset.Charset;

/**
 * 430-byte CBTRN02C layout. Transaction data stays opaque because rejected fields may be invalid; trailer is populated at app/cbl/CBTRN02C.cbl:446-448.
 * @param transactionData app/cbl/CBTRN02C.cbl:177, PIC X(350)
 * @param reasonCode app/cbl/CBTRN02C.cbl:181, PIC 9(4)
 * @param reasonDescription app/cbl/CBTRN02C.cbl:182, PIC X(76)
 */
public record RejectRecord(
        String transactionData,
        String reasonCode,
        String reasonDescription) {
    public static final RecordCodec<RejectRecord> CODEC = new RecordCodec<>() {
        public int length() { return 430; }
        public RejectRecord decode(byte[] bytes, Charset charset) {
            RecordFields fields = new RecordFields(bytes, length(), charset);
            return new RejectRecord(
                    fields.text(350),
                    fields.unsigned(4),
                    fields.text(76));
        }
        public byte[] encode(RejectRecord record, Charset charset) {
            RecordFields fields = new RecordFields(null, length(), charset);
            fields.text(record.transactionData(), 350);
            fields.unsigned(record.reasonCode(), 4);
            fields.text(record.reasonDescription(), 76);
            return fields.bytes();
        }
    };
}
