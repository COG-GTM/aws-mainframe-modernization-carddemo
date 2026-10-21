using CardDemo.Cbact01c.Cobol;

namespace CardDemo.Cbact01c.Copybooks;

/// <summary>
/// CODATECN.cpy - 01 CODATECN-REC, the parameter block passed to the COBDATFT
/// assembler date-format routine. Lives in WORKING-STORAGE, so values persist
/// between calls (initially SPACES).
/// </summary>
public sealed class DateConversionRecord
{
    /// <summary>CODATECN-TYPE PIC X: '1' = YYYYMMDD in, '2' = YYYY-MM-DD in.</summary>
    public string Type { get; set; } = " ";
    /// <summary>CODATECN-INP-DATE PIC X(20).</summary>
    public string InpDate { get; set; } = new(' ', 20);
    /// <summary>CODATECN-OUTTYPE PIC X: '1' = YYYY-MM-DD out, '2' = YYYYMMDD out.</summary>
    public string OutType { get; set; } = " ";
    /// <summary>CODATECN-0UT-DATE PIC X(20) (sic - zero, not letter O).</summary>
    public string OutDate { get; set; } = new(' ', 20);
    /// <summary>CODATECN-ERROR-MSG PIC X(38).</summary>
    public string ErrorMsg { get; set; } = new(' ', 38);
}

/// <summary>
/// Port of app/asm/COBDATFT.asm. Only overwrites the bytes the assembler
/// MVCs touch; everything else in the output area is left as it was.
/// </summary>
public static class CobDatFt
{
    public static void Call(DateConversionRecord rec)
    {
        string inp = Text.PicX(rec.InpDate, 20);
        char[] outDate = Text.PicX(rec.OutDate, 20).ToCharArray();

        switch (rec.Type)
        {
            case "1" when inp[4] != '-' && rec.OutType != "2":
                inp.CopyTo(0, outDate, 0, 4);
                outDate[4] = '-';
                inp.CopyTo(4, outDate, 5, 2);
                outDate[7] = '-';
                inp.CopyTo(6, outDate, 8, 2);
                break;
            case "2" when rec.OutType != "1":
                inp.CopyTo(0, outDate, 0, 4);
                inp.CopyTo(5, outDate, 4, 2);
                inp.CopyTo(8, outDate, 6, 2);
                break;
            default:
                rec.ErrorMsg = Text.PicX("INVALID INPUT", 38);
                return;
        }
        rec.OutDate = new string(outDate);
    }
}
