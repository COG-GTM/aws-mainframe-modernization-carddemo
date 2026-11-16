package com.carddemo.batch.model;

import com.carddemo.batch.io.FieldReader;
import com.carddemo.batch.io.FieldWriter;
import com.carddemo.batch.io.FixedWidthCodec;

import java.math.BigDecimal;

/**
 * {@code TRNX-RECORD} from copybook COSTM01 (350 bytes): the transaction file re-keyed by card number then
 * transaction id ({@code TRNX-KEY}) for statement generation (CBSTM03A/B).
 */
public record StatementTransactionRecord(
        TrnxKey trnxKey,
        String trnxTypeCd,         // PIC X(02)
        int trnxCatCd,             // PIC 9(04)
        String trnxSource,         // PIC X(10)
        String trnxDesc,           // PIC X(100)
        BigDecimal trnxAmt,        // PIC S9(09)V99
        long trnxMerchantId,       // PIC 9(09)
        String trnxMerchantName,   // PIC X(50)
        String trnxMerchantCity,   // PIC X(50)
        String trnxMerchantZip,    // PIC X(10)
        String trnxOrigTs,         // PIC X(26)
        String trnxProcTs,         // PIC X(26)
        String filler) {           // PIC X(20)

    public static final int LENGTH = 350;

    /** {@code TRNX-KEY} (32 bytes); ordered like the VSAM key bytes. */
    public record TrnxKey(
            String trnxCardNum,   // PIC X(16)
            String trnxId)        // PIC X(16)
            implements Comparable<TrnxKey> {

        public static final int LENGTH = 32;

        static TrnxKey read(FieldReader r) {
            return new TrnxKey(r.text(16), r.text(16));
        }

        FieldWriter write(FieldWriter w) {
            return w.text(trnxCardNum, 16).text(trnxId, 16);
        }

        public String encode() {
            return write(new FieldWriter(LENGTH)).toString();
        }

        @Override
        public int compareTo(TrnxKey o) {
            return encode().compareTo(o.encode());
        }
    }

    public static final FixedWidthCodec<StatementTransactionRecord> CODEC = FixedWidthCodec.of(LENGTH,
            r -> new StatementTransactionRecord(TrnxKey.read(r), r.text(2), r.unsignedInt(4), r.text(10),
                    r.text(100), r.signedDecimal(11, 2), r.unsignedLong(9), r.text(50), r.text(50), r.text(10),
                    r.text(26), r.text(26), r.text(20)),
            t -> t.trnxKey.write(new FieldWriter(LENGTH))
                    .text(t.trnxTypeCd, 2)
                    .unsigned(t.trnxCatCd, 4)
                    .text(t.trnxSource, 10)
                    .text(t.trnxDesc, 100)
                    .signedDecimal(t.trnxAmt, 11, 2)
                    .unsigned(t.trnxMerchantId, 9)
                    .text(t.trnxMerchantName, 50)
                    .text(t.trnxMerchantCity, 50)
                    .text(t.trnxMerchantZip, 10)
                    .text(t.trnxOrigTs, 26)
                    .text(t.trnxProcTs, 26)
                    .text(t.filler, 20)
                    .toString());

    /** Re-keys a CVTRA05Y transaction for statement processing. */
    public static StatementTransactionRecord from(TransactionRecord t) {
        return new StatementTransactionRecord(new TrnxKey(t.tranCardNum(), t.tranId()), t.tranTypeCd(),
                t.tranCatCd(), t.tranSource(), t.tranDesc(), t.tranAmt(), t.tranMerchantId(), t.tranMerchantName(),
                t.tranMerchantCity(), t.tranMerchantZip(), t.tranOrigTs(), t.tranProcTs(), t.filler());
    }
}
