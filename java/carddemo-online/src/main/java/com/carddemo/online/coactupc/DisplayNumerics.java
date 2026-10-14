package com.carddemo.online.coactupc;

import com.carddemo.runtime.cobol.Ebcdic;
import com.carddemo.runtime.cobol.ZonedDecimal;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Numeric value of a PIC 9 / S9 DISPLAY item as IBM Enterprise COBOL sees it when the item is used in a
 * comparison or MOVE. Valid zoned data decodes normally. Invalid data (spaces, LOW-VALUES, letters - common
 * here because the numeric items REDEFINE PIC X screen fields) is taken digit-by-digit from the low nibble of
 * each EBCDIC byte (what PACK does); a nibble above 9 is a data exception (S0C7/ASRA), raised as
 * {@link DataException} so the program can route it to its HANDLE ABEND label.
 */
final class DisplayNumerics {

    /** S0C7 data exception (program check) on invalid packed digits. */
    static final class DataException extends RuntimeException {
        DataException(String raw) {
            super("Data exception on '" + raw.replace('\u0000', '.') + "'");
        }
    }

    private DisplayNumerics() {
    }

    static BigDecimal value(String raw, int scale, boolean signed) {
        if (ZonedDecimal.isNumeric(raw, signed)) {
            return ZonedDecimal.decode(raw, scale);
        }
        byte[] ebcdic = Ebcdic.encode(raw);
        StringBuilder digits = new StringBuilder();
        for (byte b : ebcdic) {
            int nibble = b & 0x0F;
            if (nibble > 9) {
                throw new DataException(raw);
            }
            digits.append((char) ('0' + nibble));
        }
        boolean negative = false;
        if (signed && ebcdic.length > 0) {
            int zone = (ebcdic[ebcdic.length - 1] & 0xF0) >> 4;
            negative = zone == 0xB || zone == 0xD;
        }
        BigInteger unscaled = digits.isEmpty() ? BigInteger.ZERO : new BigInteger(digits.toString());
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    static long longValue(String raw) {
        return value(raw, 0, false).longValueExact();
    }
}
