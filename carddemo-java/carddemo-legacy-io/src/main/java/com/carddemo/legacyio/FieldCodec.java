package com.carddemo.legacyio;

import com.carddemo.legacyio.copybook.Picture;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.ByteBuffer;
import java.util.Arrays;

/** Converts one elementary field between its stored bytes and a Java value. */
final class FieldCodec {

    private FieldCodec() {
    }

    /**
     * Decodes a field: {@link String} for alphanumeric and edited fields, {@link BigDecimal} with the picture's scale
     * for numeric fields, {@link InvalidFieldData} for numeric bytes that do not form a valid number.
     */
    static Object decode(FieldDefinition field, Encoding encoding, byte[] record, int base) {
        int from = base + field.offset();
        if (!field.isNumeric()) {
            return encoding.decode(record, from, field.length());
        }
        byte[] bytes = Arrays.copyOfRange(record, from, from + field.length());
        BigInteger unscaled = switch (field.usage()) {
            case DISPLAY -> decodeZoned(bytes, field.picture(), encoding);
            case PACKED_DECIMAL -> decodePacked(bytes, field.picture());
            case BINARY -> field.picture().signed() ? new BigInteger(bytes) : new BigInteger(1, bytes);
        };
        return unscaled == null ? new InvalidFieldData(bytes) : new BigDecimal(unscaled, field.picture().scale());
    }

    static void encode(FieldDefinition field, Encoding encoding, Object value, byte[] record, int base) {
        int from = base + field.offset();
        if (value instanceof InvalidFieldData invalid) {
            byte[] raw = invalid.raw();
            if (raw.length != field.length()) {
                throw new IllegalArgumentException(field.name() + ": raw value has " + raw.length + " bytes, field has "
                        + field.length());
            }
            System.arraycopy(raw, 0, record, from, raw.length);
            return;
        }
        if (!field.isNumeric()) {
            encodeText(field, encoding, value, record, from);
            return;
        }
        BigInteger unscaled = unscaled(field, value);
        byte[] bytes = switch (field.usage()) {
            case DISPLAY -> encodeZoned(unscaled, field.picture(), encoding);
            case PACKED_DECIMAL -> encodePacked(unscaled, field.picture(), field.length());
            case BINARY -> encodeBinary(unscaled, field.length());
        };
        System.arraycopy(bytes, 0, record, from, bytes.length);
    }

    private static void encodeText(FieldDefinition field, Encoding encoding, Object value, byte[] record, int from) {
        if (!(value instanceof CharSequence text)) {
            throw new IllegalArgumentException(field.name() + " is alphanumeric; expected a String, got " + typeOf(value));
        }
        byte[] bytes = encoding.encode(text);
        if (bytes.length > field.length()) {
            throw new IllegalArgumentException(field.name() + " holds " + field.length() + " bytes; value has "
                    + bytes.length + ": '" + text + "'");
        }
        System.arraycopy(bytes, 0, record, from, bytes.length);
        Arrays.fill(record, from + bytes.length, from + field.length(), encoding.space());
    }

    private static BigInteger unscaled(FieldDefinition field, Object value) {
        Picture picture = field.picture();
        BigDecimal decimal = switch (value) {
            case BigDecimal d -> d;
            case BigInteger i -> new BigDecimal(i);
            case Long l -> BigDecimal.valueOf(l);
            case Integer i -> BigDecimal.valueOf(i);
            case Short s -> BigDecimal.valueOf(s);
            case null, default -> throw new IllegalArgumentException(field.name() + " is numeric; expected a "
                    + "BigDecimal, BigInteger, Long or Integer, got " + typeOf(value));
        };
        BigDecimal scaled;
        try {
            scaled = decimal.setScale(picture.scale(), RoundingMode.UNNECESSARY);
        } catch (ArithmeticException e) {
            throw new IllegalArgumentException(field.name() + " (PIC " + picture.source() + ") cannot hold " + decimal
                    + " without rounding", e);
        }
        BigInteger unscaled = scaled.unscaledValue();
        if (unscaled.signum() < 0 && !picture.signed()) {
            throw new IllegalArgumentException(field.name() + " (PIC " + picture.source() + ") is unsigned; got " + decimal);
        }
        if (unscaled.abs().toString().length() > picture.digits()) {
            throw new IllegalArgumentException(field.name() + " (PIC " + picture.source() + ") cannot hold " + decimal);
        }
        return unscaled;
    }

    private static BigInteger decodeZoned(byte[] bytes, Picture picture, Encoding encoding) {
        StringBuilder digits = new StringBuilder(bytes.length + 1);
        for (int i = 0; i < bytes.length - 1; i++) {
            int digit = encoding.plainDigit(bytes[i]);
            if (digit < 0) {
                return null;
            }
            digits.append((char) ('0' + digit));
        }
        int last = encoding.zonedLast(bytes[bytes.length - 1]);
        if (last < 0 || (!picture.signed() && (last & 0x200) == 0)) {
            return null;
        }
        digits.append((char) ('0' + (last & 0x0F)));
        BigInteger value = new BigInteger(digits.toString());
        return (last & 0x100) != 0 ? value.negate() : value;
    }

    private static byte[] encodeZoned(BigInteger unscaled, Picture picture, Encoding encoding) {
        String digits = leftPad(unscaled.abs().toString(), picture.digits());
        byte[] bytes = new byte[digits.length()];
        for (int i = 0; i < bytes.length - 1; i++) {
            bytes[i] = encoding.plainDigitByte(digits.charAt(i) - '0');
        }
        bytes[bytes.length - 1] = encoding.zonedLastByte(digits.charAt(bytes.length - 1) - '0', picture.signed(),
                unscaled.signum() < 0);
        return bytes;
    }

    private static BigInteger decodePacked(byte[] bytes, Picture picture) {
        StringBuilder digits = new StringBuilder(bytes.length * 2);
        for (int i = 0; i < bytes.length; i++) {
            int high = (bytes[i] >> 4) & 0x0F;
            int low = bytes[i] & 0x0F;
            if (high > 9) {
                return null;
            }
            digits.append((char) ('0' + high));
            if (i < bytes.length - 1) {
                if (low > 9) {
                    return null;
                }
                digits.append((char) ('0' + low));
            } else {
                boolean valid = picture.signed() ? low >= 0x0A : low == 0x0F;
                if (!valid) {
                    return null;
                }
                BigInteger value = new BigInteger(digits.toString());
                return low == 0x0D || low == 0x0B ? value.negate() : value;
            }
        }
        return null;
    }

    private static byte[] encodePacked(BigInteger unscaled, Picture picture, int length) {
        String digits = leftPad(unscaled.abs().toString(), length * 2 - 1);
        byte[] bytes = new byte[length];
        for (int i = 0; i < length; i++) {
            int high = digits.charAt(i * 2) - '0';
            int low = i < length - 1 ? digits.charAt(i * 2 + 1) - '0'
                    : !picture.signed() ? 0x0F : unscaled.signum() < 0 ? 0x0D : 0x0C;
            bytes[i] = (byte) ((high << 4) | low);
        }
        return bytes;
    }

    private static byte[] encodeBinary(BigInteger unscaled, int length) {
        long value = unscaled.longValueExact();
        ByteBuffer buffer = ByteBuffer.allocate(8).putLong(value);
        return Arrays.copyOfRange(buffer.array(), 8 - length, 8);
    }

    private static String leftPad(String digits, int width) {
        return digits.length() >= width ? digits : "0".repeat(width - digits.length()) + digits;
    }

    private static String typeOf(Object value) {
        return value == null ? "null" : value.getClass().getSimpleName();
    }
}
