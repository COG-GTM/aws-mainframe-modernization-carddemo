package com.carddemo.programs.copaus1c.copybook;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HexFormat;

/**
 * PA-AUTHORIZATION-KEY (CIPAUDTY): 8 raw bytes made of PA-AUTH-DATE-9C PIC S9(05) COMP-3 (3 bytes) and
 * PA-AUTH-TIME-9C PIC S9(09) COMP-3 (5 bytes). The same bytes travel through PIC X(08) items such as
 * CDEMO-CPVD-PAU-SELECTED and WS-AUTH-KEY, and form the IMS key field PAUT9CTS.
 */
public final class AuthorizationKey implements Comparable<AuthorizationKey> {

    public static final int LENGTH = 8;
    public static final AuthorizationKey SPACES = new AuthorizationKey(filled((byte) 0x40));
    public static final AuthorizationKey LOW_VALUES = new AuthorizationKey(filled((byte) 0x00));

    private final byte[] bytes;

    private AuthorizationKey(byte[] bytes) {
        this.bytes = bytes;
    }

    public static AuthorizationKey ofBytes(byte[] raw) {
        if (raw.length != LENGTH) {
            throw new IllegalArgumentException("authorization key must be 8 bytes");
        }
        return new AuthorizationKey(Arrays.copyOf(raw, LENGTH));
    }

    public static AuthorizationKey of(int authDate9c, long authTime9c) {
        byte[] b = new byte[LENGTH];
        Cobol.writePacked(b, 0, 3, 0, BigDecimal.valueOf(authDate9c));
        Cobol.writePacked(b, 3, 5, 0, BigDecimal.valueOf(authTime9c));
        return new AuthorizationKey(b);
    }

    private static byte[] filled(byte v) {
        byte[] b = new byte[LENGTH];
        Arrays.fill(b, v);
        return b;
    }

    /** PA-AUTH-DATE-9C PIC S9(05) COMP-3. */
    public int authDate9c() {
        return Cobol.readPacked(bytes, 0, 3, 0).intValue();
    }

    /** PA-AUTH-TIME-9C PIC S9(09) COMP-3. */
    public long authTime9c() {
        return Cobol.readPacked(bytes, 3, 5, 0).longValue();
    }

    public byte[] bytes() {
        return Arrays.copyOf(bytes, LENGTH);
    }

    public boolean isSpaces() {
        return equals(SPACES);
    }

    public boolean isLowValues() {
        return equals(LOW_VALUES);
    }

    @Override
    public int compareTo(AuthorizationKey o) {
        return Arrays.compareUnsigned(bytes, o.bytes);
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof AuthorizationKey k && Arrays.equals(bytes, k.bytes);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(bytes);
    }

    @Override
    public String toString() {
        return "AuthorizationKey[" + HexFormat.of().formatHex(bytes) + "]";
    }
}
