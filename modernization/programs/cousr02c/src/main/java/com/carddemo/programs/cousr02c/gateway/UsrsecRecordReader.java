package com.carddemo.programs.cousr02c.gateway;

import com.carddemo.programs.cousr02c.copybook.SecUserData;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

/**
 * Fixed-width reader for the USRSEC sequential unload (LRECL=80, RECFM=FB, see app/jcl/DUSRSECJ.jcl) laid out per
 * CSUSR01Y. Accepts either raw FB data (no record separators, e.g. app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS with
 * charset IBM037) or newline-delimited text, whose lines are space-padded to 80.
 */
public final class UsrsecRecordReader {

    private UsrsecRecordReader() {
    }

    public static List<SecUserData> read(byte[] data, Charset charset) {
        String text = new String(data, charset);
        List<SecUserData> out = new ArrayList<>();
        if (text.indexOf('\n') >= 0) {
            for (String line : text.split("\r?\n")) {
                if (!line.isEmpty()) {
                    out.add(SecUserData.fromRecord(line));
                }
            }
            return out;
        }
        if (text.length() % SecUserData.RECORD_LENGTH != 0) {
            throw new IllegalArgumentException("FB data length " + text.length() + " is not a multiple of 80");
        }
        for (int p = 0; p < text.length(); p += SecUserData.RECORD_LENGTH) {
            out.add(SecUserData.fromRecord(text.substring(p, p + SecUserData.RECORD_LENGTH)));
        }
        return out;
    }

    public static byte[] write(List<SecUserData> records, Charset charset) {
        StringBuilder sb = new StringBuilder();
        records.forEach(r -> sb.append(r.toRecord()));
        return sb.toString().getBytes(charset);
    }
}
