package com.carddemo.programs.cousr01c.copybook;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;

/** CSUSR01Y - SEC-USER-DATA, the 80-byte USRSEC VSAM KSDS record (key SEC-USR-ID). */
public record SecUserData(
        String secUsrId,      // PIC X(08)
        String secUsrFname,   // PIC X(20)
        String secUsrLname,   // PIC X(20)
        String secUsrPwd,     // PIC X(08)
        String secUsrType,    // PIC X(01)
        String secUsrFiller   // PIC X(23)
) {

    public static final int LENGTH = 80;
    public static final int KEY_LENGTH = 8;
    public static final Charset EBCDIC = Charset.forName("Cp037");

    public SecUserData {
        secUsrId = Pic.x(secUsrId, 8);
        secUsrFname = Pic.x(secUsrFname, 20);
        secUsrLname = Pic.x(secUsrLname, 20);
        secUsrPwd = Pic.x(secUsrPwd, 8);
        secUsrType = Pic.x(secUsrType, 1);
        secUsrFiller = Pic.x(secUsrFiller, 23);
    }

    public static SecUserData fromFixedWidth(String raw) {
        String s = Pic.x(raw, LENGTH);
        return new SecUserData(s.substring(0, 8), s.substring(8, 28), s.substring(28, 48),
                s.substring(48, 56), s.substring(56, 57), s.substring(57, 80));
    }

    public static SecUserData fromEbcdic(byte[] record) {
        return fromFixedWidth(new String(record, EBCDIC));
    }

    public String toFixedWidth() {
        return secUsrId + secUsrFname + secUsrLname + secUsrPwd + secUsrType + secUsrFiller;
    }

    /** Encodes the record as 80 Cp037 bytes; characters with no EBCDIC code point are rejected. */
    public byte[] toEbcdic() {
        return encodeEbcdic(toFixedWidth(), LENGTH);
    }

    static byte[] encodeEbcdic(String s, int expectedLength) {
        try {
            ByteBuffer buf = EBCDIC.newEncoder()
                    .onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT)
                    .encode(CharBuffer.wrap(s));
            byte[] out = new byte[buf.remaining()];
            buf.get(out);
            if (out.length != expectedLength) {
                throw new IllegalArgumentException(
                        "EBCDIC record is " + out.length + " bytes, expected " + expectedLength);
            }
            return out;
        } catch (CharacterCodingException e) {
            throw new IllegalArgumentException("Value has characters not representable in Cp037", e);
        }
    }

    /** Compares two keys by their Cp037 byte values, i.e. VSAM KSDS key sequence. */
    public static int compareEbcdicKeys(String a, String b) {
        byte[] x = encodeEbcdic(a, a.length());
        byte[] y = encodeEbcdic(b, b.length());
        return java.util.Arrays.compareUnsigned(x, y);
    }
}
