package com.carddemo.programs.cousr02c.gateway;

import com.carddemo.programs.cousr02c.copybook.SecUserData;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;
import java.util.ArrayList;
import java.util.List;

/**
 * Fixed-width reader/writer for the USRSEC sequential unload (LRECL=80, RECFM=FB, see app/jcl/DUSRSECJ.jcl) laid
 * out per CSUSR01Y. {@link #read} takes raw FB data with no record separators (e.g.
 * app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS with charset IBM037); {@link #readLines} takes newline-delimited text
 * whose lines are space-padded to 80.
 */
public final class UsrsecRecordReader {

    private UsrsecRecordReader() {
    }

    public static List<SecUserData> read(byte[] data, Charset charset) {
        if (data.length % SecUserData.RECORD_LENGTH != 0) {
            throw new IllegalArgumentException("FB data length " + data.length + " is not a multiple of 80");
        }
        List<SecUserData> out = new ArrayList<>();
        for (int p = 0; p < data.length; p += SecUserData.RECORD_LENGTH) {
            String rec = decode(data, p, SecUserData.RECORD_LENGTH, charset);
            if (rec.length() != SecUserData.RECORD_LENGTH) {
                throw new IllegalArgumentException("charset " + charset + " is not single-byte");
            }
            out.add(SecUserData.fromRecord(rec));
        }
        return out;
    }

    public static List<SecUserData> readLines(byte[] data, Charset charset) {
        List<SecUserData> out = new ArrayList<>();
        for (String line : decode(data, 0, data.length, charset).split("\r?\n")) {
            if (!line.isEmpty()) {
                out.add(SecUserData.fromRecord(line));
            }
        }
        return out;
    }

    private static String decode(byte[] data, int offset, int length, Charset charset) {
        try {
            return charset.newDecoder()
                    .onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT)
                    .decode(ByteBuffer.wrap(data, offset, length))
                    .toString();
        } catch (CharacterCodingException e) {
            throw new IllegalArgumentException("USRSEC data at offset " + offset + " is not valid " + charset, e);
        }
    }

    public static byte[] write(List<SecUserData> records, Charset charset) {
        CharsetEncoder encoder = charset.newEncoder()
                .onMalformedInput(CodingErrorAction.REPORT)
                .onUnmappableCharacter(CodingErrorAction.REPORT);
        ByteBuffer out = ByteBuffer.allocate(records.size() * SecUserData.RECORD_LENGTH);
        for (SecUserData r : records) {
            ByteBuffer rec;
            try {
                rec = encoder.encode(CharBuffer.wrap(r.toRecord()));
            } catch (CharacterCodingException e) {
                throw new IllegalArgumentException("record " + r.getSecUsrId() + " is not encodable in " + charset, e);
            }
            if (rec.remaining() != SecUserData.RECORD_LENGTH) {
                throw new IllegalArgumentException("record " + r.getSecUsrId() + " encodes to " + rec.remaining()
                        + " bytes, not 80, in " + charset);
            }
            out.put(rec);
        }
        return out.array();
    }
}
