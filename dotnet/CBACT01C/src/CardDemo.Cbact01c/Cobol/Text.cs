using System.Text;

namespace CardDemo.Cbact01c.Cobol;

public static class Text
{
    /// <summary>Single-byte code page so every record byte maps 1:1 to a char.</summary>
    public static readonly Encoding Latin1 = Encoding.Latin1;

    /// <summary>MOVE alphanumeric: left-justify, pad with spaces, truncate on the right.</summary>
    public static string PicX(string? value, int length) =>
        (value ?? string.Empty).PadRight(length)[..length];
}
