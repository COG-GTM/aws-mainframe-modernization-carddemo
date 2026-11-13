package com.carddemo.common.copybook;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.cobol.Ebcdic;
import com.carddemo.common.cobol.Usage;

/**
 * Layout-aware code page conversion: DISPLAY bytes (text, zoned decimals with their overpunch, FILLER) are
 * translated, COMP/COMP-3/COMP-5 bytes are copied untouched. This is what a correct EBCDIC to ASCII file
 * transfer must do; a plain text-mode transfer corrupts binary and packed fields.
 */
public final class RecordTranscoder {

    private RecordTranscoder() {
    }

    public static byte[] transcode(byte[] record, RecordLayout layout, DataEncoding from, DataEncoding to) {
        byte[] out = record.clone();
        if (from == to) {
            return out;
        }
        boolean[] binary = new boolean[record.length];
        for (CopybookField f : layout.primaryElementaryFields()) {
            if (f.usage() != Usage.DISPLAY) {
                for (int offset : RecordView.offsets(f)) {
                    for (int i = offset; i < Math.min(offset + f.length(), record.length); i++) {
                        binary[i] = true;
                    }
                }
            }
        }
        int i = 0;
        while (i < record.length) {
            if (binary[i]) {
                i++;
                continue;
            }
            int start = i;
            while (i < record.length && !binary[i]) {
                i++;
            }
            byte[] converted = from == DataEncoding.EBCDIC
                    ? Ebcdic.toAscii(record, start, i - start)
                    : Ebcdic.toEbcdic(record, start, i - start);
            System.arraycopy(converted, 0, out, start, converted.length);
        }
        return out;
    }
}
