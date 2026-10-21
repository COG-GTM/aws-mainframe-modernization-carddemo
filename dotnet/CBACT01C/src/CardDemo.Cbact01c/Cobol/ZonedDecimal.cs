namespace CardDemo.Cbact01c.Cobol;

/// <summary>
/// Codec for signed zoned-decimal (USAGE DISPLAY) fields such as PIC S9(10)V99,
/// using the EBCDIC "overpunch" sign convention found in the CardDemo data
/// ('{' = +0, 'A'..'I' = +1..+9, '}' = -0, 'J'..'R' = -1..-9).
/// </summary>
public static class ZonedDecimal
{
    private const string PositiveOverpunch = "{ABCDEFGHI";
    private const string NegativeOverpunch = "}JKLMNOPQR";

    public static decimal Decode(ReadOnlySpan<char> text, int scale)
    {
        if (text.IsEmpty)
            throw new FormatException("Empty zoned decimal field.");

        decimal magnitude = 0m;
        for (int i = 0; i < text.Length - 1; i++)
            magnitude = magnitude * 10 + Digit(text[i], text);

        char last = text[^1];
        bool negative;
        int lastDigit;
        if (last is >= '0' and <= '9') { negative = false; lastDigit = last - '0'; }
        else if (PositiveOverpunch.IndexOf(last) is var p and >= 0) { negative = false; lastDigit = p; }
        else if (NegativeOverpunch.IndexOf(last) is var n and >= 0) { negative = true; lastDigit = n; }
        else throw new FormatException($"Invalid zoned sign byte '{last}' in '{text}'.");

        magnitude = magnitude * 10 + lastDigit;
        // decimal(lo, mid, hi, isNegative, scale) keeps the sign of -0, like COBOL does.
        int[] bits = decimal.GetBits(magnitude);
        return new decimal(bits[0], bits[1], bits[2], negative, (byte)scale);
    }

    /// <summary>Encodes with an overpunched sign on the last digit (MOVE semantics).</summary>
    public static string Encode(decimal value, int integerDigits, int scale)
    {
        string digits = Digits(value, integerDigits, scale);
        int last = digits[^1] - '0';
        char sign = decimal.IsNegative(value) ? NegativeOverpunch[last] : PositiveOverpunch[last];
        return string.Concat(digits.AsSpan(0, digits.Length - 1), sign.ToString());
    }

    /// <summary>
    /// DISPLAY of a signed numeric item: all digits, no decimal point, trailing '+'/'-'
    /// (GnuCOBOL -std=ibm behaviour).
    /// </summary>
    public static string Display(decimal value, int integerDigits, int scale) =>
        Digits(value, integerDigits, scale) + (decimal.IsNegative(value) ? "-" : "+");

    /// <summary>Unscaled, zero-padded, truncated to the PIC size (COBOL high-order truncation).</summary>
    internal static string Digits(decimal value, int integerDigits, int scale)
    {
        decimal unscaled = decimal.Truncate(Math.Abs(value) * Pow10(scale));
        decimal modulus = Pow10(integerDigits + scale);
        unscaled %= modulus;
        return unscaled.ToString("0", System.Globalization.CultureInfo.InvariantCulture)
                       .PadLeft(integerDigits + scale, '0');
    }

    private static decimal Pow10(int n)
    {
        decimal r = 1m;
        for (int i = 0; i < n; i++) r *= 10m;
        return r;
    }

    private static int Digit(char c, ReadOnlySpan<char> text) =>
        c is >= '0' and <= '9' ? c - '0'
            : throw new FormatException($"Non-numeric byte '{c}' in zoned field '{text}'.");
}
