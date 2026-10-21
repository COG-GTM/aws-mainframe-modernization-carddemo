using CardDemo.Cbact01c.Cobol;
using CardDemo.Cbact01c.Copybooks;

namespace CardDemo.Cbact01c.Tests;

public class CodecTests
{
    [Theory]
    [InlineData("00000001940{", "194.00")]
    [InlineData("00000001234N", "-123.45")]
    [InlineData("99999999999I", "9999999999.99")]
    [InlineData("000000012340", "123.40")]
    public void ZonedDecimal_DecodesOverpunchedSign(string zoned, string expected) =>
        Assert.Equal(decimal.Parse(expected), ZonedDecimal.Decode(zoned, 2));

    [Fact]
    public void ZonedDecimal_PreservesNegativeZero()
    {
        decimal negZero = ZonedDecimal.Decode("00000000000}", 2);
        Assert.Equal(0m, negZero);
        Assert.Equal("00000000000}", ZonedDecimal.Encode(negZero, 10, 2));
        Assert.Equal("000000000000-", ZonedDecimal.Display(negZero, 10, 2));
    }

    [Theory]
    [InlineData("2525.00", "0000000252500C")]
    [InlineData("-2500.00", "0000000250000D")]
    [InlineData("0", "0000000000000C")]
    public void PackedDecimal_EncodesComp3(string value, string hex) =>
        Assert.Equal(hex, Convert.ToHexString(PackedDecimal.Encode(decimal.Parse(value), 10, 2)));

    [Fact]
    public void CobDatFt_ConvertsIsoDateToYyyymmdd()
    {
        var rec = new DateConversionRecord { Type = "2", OutType = "2", InpDate = "2025-05-20".PadRight(20) };
        CobDatFt.Call(rec);
        Assert.Equal("20250520  ", rec.OutDate[..10]);
    }

    [Fact]
    public void AccountRecord_RoundTripsThe300ByteImage()
    {
        string line = File.ReadLines(TestPaths.TestData("acctdata")).First();
        Assert.Equal(line.PadRight(300), AccountRecord.Parse(line).ToImage());
    }
}
