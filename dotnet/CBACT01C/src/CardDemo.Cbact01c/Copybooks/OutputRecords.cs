using CardDemo.Cbact01c.Cobol;

namespace CardDemo.Cbact01c.Copybooks;

/// <summary>FD OUT-FILE / 01 OUT-ACCT-REC - fixed 107-byte record.</summary>
public sealed class OutAccountRecord
{
    public const int Length = 107;

    public long AcctId { get; set; }                 // OUT-ACCT-ID                PIC 9(11)
    public string ActiveStatus { get; set; } = " ";  // OUT-ACCT-ACTIVE-STATUS     PIC X(01)
    public decimal CurrBal { get; set; }             // OUT-ACCT-CURR-BAL          PIC S9(10)V99
    public decimal CreditLimit { get; set; }         // OUT-ACCT-CREDIT-LIMIT      PIC S9(10)V99
    public decimal CashCreditLimit { get; set; }     // OUT-ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
    public string OpenDate { get; set; } = "";       // OUT-ACCT-OPEN-DATE         PIC X(10)
    public string ExpirationDate { get; set; } = ""; // OUT-ACCT-EXPIRAION-DATE    PIC X(10)
    public string ReissueDate { get; set; } = "";    // OUT-ACCT-REISSUE-DATE      PIC X(10)
    public decimal CurrCycCredit { get; set; }       // OUT-ACCT-CURR-CYC-CREDIT   PIC S9(10)V99

    /// <summary>
    /// OUT-ACCT-CURR-CYC-DEBIT PIC S9(10)V99 COMP-3 (7 bytes).
    /// <c>null</c> = never assigned: the FD buffer still holds LOW-VALUES.
    /// The legacy program only assigns it when the input debit is zero, so
    /// otherwise the value from the previous record is written again.
    /// </summary>
    public decimal? CurrCycDebit { get; set; }
    public string GroupId { get; set; } = "";        // OUT-ACCT-GROUP-ID          PIC X(10)

    public byte[] ToBytes()
    {
        var buf = new List<byte>(Length);
        buf.AddRange(Text.Latin1.GetBytes(string.Concat(
            AcctId.ToString("D11"),
            Text.PicX(ActiveStatus, 1),
            ZonedDecimal.Encode(CurrBal, 10, 2),
            ZonedDecimal.Encode(CreditLimit, 10, 2),
            ZonedDecimal.Encode(CashCreditLimit, 10, 2),
            Text.PicX(OpenDate, 10),
            Text.PicX(ExpirationDate, 10),
            Text.PicX(ReissueDate, 10),
            ZonedDecimal.Encode(CurrCycCredit, 10, 2))));
        buf.AddRange(CurrCycDebit is { } d ? PackedDecimal.Encode(d, 10, 2) : new byte[7]);
        buf.AddRange(Text.Latin1.GetBytes(Text.PicX(GroupId, 10)));
        return buf.ToArray();
    }
}

/// <summary>05 ARR-ACCT-BAL OCCURS 5 TIMES.</summary>
public sealed class ArrayBalance
{
    /// <summary>
    /// ARR-ACCT-CURR-BAL PIC S9(10)V99. <c>null</c> = INITIALIZEd and never
    /// MOVEd to: GnuCOBOL writes plain '0' digits (no overpunch) in that case.
    /// </summary>
    public decimal? CurrBal { get; set; }
    /// <summary>ARR-ACCT-CURR-CYC-DEBIT PIC S9(10)V99 COMP-3.</summary>
    public decimal CurrCycDebit { get; set; }
}

/// <summary>FD ARRY-FILE / 01 ARR-ARRAY-REC - fixed 110-byte record.</summary>
public sealed class ArrayRecord
{
    public const int Length = 110;
    public const int Occurs = 5;

    public long AcctId { get; set; }                                   // ARR-ACCT-ID PIC 9(11)
    public ArrayBalance[] Balances { get; } = NewBalances();           // ARR-ACCT-BAL OCCURS 5
    public string Filler { get; set; } = "    ";                       // ARR-FILLER  PIC X(04)

    /// <summary>INITIALIZE ARR-ARRAY-REC.</summary>
    public void Initialize()
    {
        AcctId = 0;
        for (int i = 0; i < Occurs; i++) Balances[i] = new ArrayBalance();
        Filler = "    ";
    }

    public byte[] ToBytes()
    {
        var buf = new List<byte>(Length);
        buf.AddRange(Text.Latin1.GetBytes(AcctId.ToString("D11")));
        foreach (var b in Balances)
        {
            string bal = b.CurrBal is { } v ? ZonedDecimal.Encode(v, 10, 2) : new string('0', 12);
            buf.AddRange(Text.Latin1.GetBytes(bal));
            buf.AddRange(PackedDecimal.Encode(b.CurrCycDebit, 10, 2));
        }
        buf.AddRange(Text.Latin1.GetBytes(Text.PicX(Filler, 4)));
        return buf.ToArray();
    }

    private static ArrayBalance[] NewBalances() =>
        Enumerable.Range(0, Occurs).Select(_ => new ArrayBalance()).ToArray();
}

/// <summary>WORKING-STORAGE 01 VBRC-REC1 - 12 bytes, written as a variable-length record.</summary>
public sealed class VbrcRecord1
{
    public const int Length = 12;
    public long AcctId { get; set; }                 // VB1-ACCT-ID            PIC 9(11)
    public string ActiveStatus { get; set; } = " ";  // VB1-ACCT-ACTIVE-STATUS PIC X(01)

    public string ToImage() => AcctId.ToString("D11") + Text.PicX(ActiveStatus, 1);
}

/// <summary>WORKING-STORAGE 01 VBRC-REC2 - 39 bytes, written as a variable-length record.</summary>
public sealed class VbrcRecord2
{
    public const int Length = 39;
    public long AcctId { get; set; }                 // VB2-ACCT-ID           PIC 9(11)
    public decimal CurrBal { get; set; }             // VB2-ACCT-CURR-BAL     PIC S9(10)V99
    public decimal CreditLimit { get; set; }         // VB2-ACCT-CREDIT-LIMIT PIC S9(10)V99
    public string ReissueYyyy { get; set; } = "    ";// VB2-ACCT-REISSUE-YYYY PIC X(04)

    public string ToImage() => string.Concat(
        AcctId.ToString("D11"),
        ZonedDecimal.Encode(CurrBal, 10, 2),
        ZonedDecimal.Encode(CreditLimit, 10, 2),
        Text.PicX(ReissueYyyy, 4));
}
