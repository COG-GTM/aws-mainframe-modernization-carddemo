package com.carddemo.legacyio;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

/**
 * Converts records between bytes and {@link Record} values for one {@link LegacyEncoding}.
 *
 * <p>Signed zoned-decimal fields carry the sign as an overpunch on the last digit, as z/OS stores them and GnuCOBOL
 * writes them with {@code -fsign=EBCDIC}: {@code {}, {@code A}-{@code I} for +0..+9 and {@code }}, {@code J}-{@code R}
 * for -0..-9 (bytes C0-C9 / D0-D9 in EBCDIC). A plain digit is read as positive. Packed decimal uses sign nibble C/D
 * for signed and F for unsigned fields; binary fields are big-endian.
 */
public final class RecordCodec {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    private final LegacyEncoding encoding;

    public RecordCodec(LegacyEncoding encoding) {
        this.encoding = encoding;
    }

    public LegacyEncoding encoding() {
        return encoding;
    }

    public Record decode(RecordLayout layout, byte[] data) {
        if (data.length != layout.length()) {
            throw new IllegalArgumentException(
                    "record of %d bytes does not match %s".formatted(data.length, layout));
        }
        List<FieldDefinition> fields = layout.fields();
        Object[] values = new Object[fields.size()];
        byte[][] storedNumbers = new byte[fields.size()][];
        for (int i = 0; i < values.length; i++) {
            FieldDefinition field = fields.get(i);
            values[i] = decodeField(field, data);
            if (field.isNumeric()) {
                storedNumbers[i] = Arrays.copyOfRange(data, field.offset(), field.offset() + field.length());
            }
        }
        return new Record(layout, values, storedNumbers);
    }

    public byte[] encode(Record record) {
        RecordLayout layout = record.layout();
        byte[] data = new byte[layout.length()];
        List<FieldDefinition> fields = layout.fields();
        for (int i = 0; i < fields.size(); i++) {
            byte[] stored = record.storedNumber(i);
            if (stored != null) {
                System.arraycopy(stored, 0, data, fields.get(i).offset(), stored.length);
            } else {
                encodeField(fields.get(i), record.get(i), data);
            }
        }
        return data;
    }

    private Object decodeField(FieldDefinition field, byte[] data) {
        if (!field.isNumeric()) {
            return encoding.decode(data, field.offset(), field.length());
        }
        BigDecimal value = switch (field.usage()) {
            case DISPLAY -> decodeZoned(field, data);
            case PACKED_DECIMAL -> decodePacked(field, data);
            case BINARY -> decodeBinary(field, data);
        };
        return value == null ? new InvalidValue(Arrays.copyOfRange(data, field.offset(), field.offset() + field.length()))
                : value;
    }

    private BigDecimal decodeZoned(FieldDefinition field, byte[] data) {
        String text = encoding.decode(data, field.offset(), field.length());
        StringBuilder digits = new StringBuilder(text.length() + 1);
        boolean negative = false;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c >= '0' && c <= '9') {
                digits.append(c);
            } else if (i == text.length() - 1 && field.picture().signed() && POSITIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + POSITIVE_OVERPUNCH.indexOf(c)));
            } else if (i == text.length() - 1 && field.picture().signed() && NEGATIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + NEGATIVE_OVERPUNCH.indexOf(c)));
                negative = true;
            } else {
                return null;
            }
        }
        BigInteger unscaled = new BigInteger(digits.toString());
        if (negative && unscaled.signum() == 0) {
            return null;
        }
        return new BigDecimal(negative ? unscaled.negate() : unscaled, field.picture().scale());
    }

    private static BigDecimal decodePacked(FieldDefinition field, byte[] data) {
        Picture picture = field.picture();
        int nibbles = field.length() * 2;
        BigInteger unscaled = BigInteger.ZERO;
        for (int n = 0; n < nibbles - 1; n++) {
            int nibble = nibble(data, field.offset(), n);
            boolean padding = n < nibbles - 1 - picture.digits();
            if (nibble > 9 || padding && nibble != 0) {
                return null;
            }
            unscaled = unscaled.multiply(BigInteger.TEN).add(BigInteger.valueOf(nibble));
        }
        int sign = nibble(data, field.offset(), nibbles - 1);
        boolean expected = picture.signed() ? sign == 0xC || sign == 0xD && unscaled.signum() != 0 : sign == 0xF;
        if (!expected) {
            return null;
        }
        return new BigDecimal(sign == 0xD ? unscaled.negate() : unscaled, picture.scale());
    }

    private static BigDecimal decodeBinary(FieldDefinition field, byte[] data) {
        byte[] bytes = Arrays.copyOfRange(data, field.offset(), field.offset() + field.length());
        BigInteger unscaled = field.picture().signed() ? new BigInteger(bytes) : new BigInteger(1, bytes);
        if (unscaled.abs().toString().length() > field.picture().digits()) {
            return null;
        }
        return new BigDecimal(unscaled, field.picture().scale());
    }

    private void encodeField(FieldDefinition field, Object value, byte[] data) {
        if (value instanceof InvalidValue invalid) {
            System.arraycopy(invalid.raw(), 0, data, field.offset(), field.length());
            return;
        }
        if (!field.isNumeric()) {
            encoding.encode((String) value, data, field.offset());
            return;
        }
        BigInteger unscaled = ((BigDecimal) value).setScale(field.picture().scale()).unscaledValue();
        switch (field.usage()) {
            case DISPLAY -> encodeZoned(field, unscaled, data);
            case PACKED_DECIMAL -> encodePacked(field, unscaled, data);
            case BINARY -> encodeBinary(field, unscaled, data);
        }
    }

    private void encodeZoned(FieldDefinition field, BigInteger unscaled, byte[] data) {
        String digits = paddedDigits(field, unscaled);
        if (field.picture().signed()) {
            int last = digits.charAt(digits.length() - 1) - '0';
            String overpunch = unscaled.signum() < 0 ? NEGATIVE_OVERPUNCH : POSITIVE_OVERPUNCH;
            digits = digits.substring(0, digits.length() - 1) + overpunch.charAt(last);
        }
        encoding.encode(digits, data, field.offset());
    }

    private static void encodePacked(FieldDefinition field, BigInteger unscaled, byte[] data) {
        String digits = paddedDigits(field, unscaled);
        int nibbles = field.length() * 2;
        String all = "0".repeat(nibbles - 1 - digits.length()) + digits;
        int sign = !field.picture().signed() ? 0xF : unscaled.signum() < 0 ? 0xD : 0xC;
        for (int b = 0; b < field.length(); b++) {
            int high = all.charAt(2 * b) - '0';
            int low = 2 * b + 1 < all.length() ? all.charAt(2 * b + 1) - '0' : sign;
            data[field.offset() + b] = (byte) (high << 4 | low);
        }
    }

    private static void encodeBinary(FieldDefinition field, BigInteger unscaled, byte[] data) {
        paddedDigits(field, unscaled);
        byte[] bytes = unscaled.toByteArray();
        byte fill = unscaled.signum() < 0 ? (byte) 0xFF : 0;
        int length = field.length();
        if (bytes.length > length && !(bytes.length == length + 1 && bytes[0] == 0)) {
            throw new IllegalArgumentException(unscaled + " does not fit " + field.name());
        }
        for (int i = 0; i < length; i++) {
            int source = bytes.length - length + i;
            data[field.offset() + i] = source < 0 ? fill : bytes[source];
        }
    }

    private static String paddedDigits(FieldDefinition field, BigInteger unscaled) {
        String digits = unscaled.abs().toString();
        int width = field.picture().digits();
        if (digits.length() > width) {
            throw new IllegalArgumentException(unscaled + " has more than " + width + " digits for " + field.name());
        }
        return "0".repeat(width - digits.length()) + digits;
    }

    private static int nibble(byte[] data, int offset, int index) {
        int b = data[offset + index / 2] & 0xFF;
        return index % 2 == 0 ? b >> 4 : b & 0x0F;
    }
}
