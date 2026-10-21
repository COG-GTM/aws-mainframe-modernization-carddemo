namespace CardDemo.Cbact01c.Cobol;

/// <summary>Encoder for COMP-3 (packed decimal) fields, e.g. PIC S9(10)V99 COMP-3 = 7 bytes.</summary>
public static class PackedDecimal
{
    public static int ByteLength(int totalDigits) => totalDigits / 2 + 1;

    public static byte[] Encode(decimal value, int integerDigits, int scale)
    {
        int totalDigits = integerDigits + scale;
        int length = ByteLength(totalDigits);
        string digits = ZonedDecimal.Digits(value, integerDigits, scale)
                                    .PadLeft(length * 2 - 1, '0');
        var nibbles = new int[length * 2];
        for (int i = 0; i < digits.Length; i++) nibbles[i] = digits[i] - '0';
        nibbles[^1] = decimal.IsNegative(value) ? 0xD : 0xC;

        var bytes = new byte[length];
        for (int i = 0; i < length; i++)
            bytes[i] = (byte)((nibbles[2 * i] << 4) | nibbles[2 * i + 1]);
        return bytes;
    }
}
