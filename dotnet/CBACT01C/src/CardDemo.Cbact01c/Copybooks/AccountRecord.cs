using CardDemo.Cbact01c.Cobol;

namespace CardDemo.Cbact01c.Copybooks;

/// <summary>
/// CVACT01Y.cpy - 01 ACCOUNT-RECORD (RECLN 300). The record layout of the ACCTFILE KSDS.
/// </summary>
public sealed class AccountRecord
{
    public const int Length = 300;

    /// <summary>ACCT-ID PIC 9(11) - KSDS primary key.</summary>
    public long AcctId { get; set; }
    /// <summary>ACCT-ACTIVE-STATUS PIC X(01).</summary>
    public string ActiveStatus { get; set; } = " ";
    /// <summary>ACCT-CURR-BAL PIC S9(10)V99.</summary>
    public decimal CurrBal { get; set; }
    /// <summary>ACCT-CREDIT-LIMIT PIC S9(10)V99.</summary>
    public decimal CreditLimit { get; set; }
    /// <summary>ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99.</summary>
    public decimal CashCreditLimit { get; set; }
    /// <summary>ACCT-OPEN-DATE PIC X(10) (YYYY-MM-DD).</summary>
    public string OpenDate { get; set; } = "";
    /// <summary>ACCT-EXPIRAION-DATE PIC X(10) (sic - the copybook spells it this way).</summary>
    public string ExpirationDate { get; set; } = "";
    /// <summary>ACCT-REISSUE-DATE PIC X(10) (YYYY-MM-DD).</summary>
    public string ReissueDate { get; set; } = "";
    /// <summary>ACCT-CURR-CYC-CREDIT PIC S9(10)V99.</summary>
    public decimal CurrCycCredit { get; set; }
    /// <summary>ACCT-CURR-CYC-DEBIT PIC S9(10)V99.</summary>
    public decimal CurrCycDebit { get; set; }
    /// <summary>ACCT-ADDR-ZIP PIC X(10).</summary>
    public string AddrZip { get; set; } = "";
    /// <summary>ACCT-GROUP-ID PIC X(10).</summary>
    public string GroupId { get; set; } = "";
    /// <summary>FILLER PIC X(178) - carried so the 300-byte image round-trips.</summary>
    public string Filler { get; set; } = "";

    /// <summary>Key exactly as stored in the record (FD-ACCT-ID), used for KSDS ordering.</summary>
    public static string KeyOf(string image) => Text.PicX(image, Length)[..11];

    public static AccountRecord Parse(string image)
    {
        string r = Text.PicX(image, Length);
        return new AccountRecord
        {
            AcctId = long.Parse(r.AsSpan(0, 11), System.Globalization.CultureInfo.InvariantCulture),
            ActiveStatus = r.Substring(11, 1),
            CurrBal = ZonedDecimal.Decode(r.AsSpan(12, 12), 2),
            CreditLimit = ZonedDecimal.Decode(r.AsSpan(24, 12), 2),
            CashCreditLimit = ZonedDecimal.Decode(r.AsSpan(36, 12), 2),
            OpenDate = r.Substring(48, 10),
            ExpirationDate = r.Substring(58, 10),
            ReissueDate = r.Substring(68, 10),
            CurrCycCredit = ZonedDecimal.Decode(r.AsSpan(78, 12), 2),
            CurrCycDebit = ZonedDecimal.Decode(r.AsSpan(90, 12), 2),
            AddrZip = r.Substring(102, 10),
            GroupId = r.Substring(112, 10),
            Filler = r.Substring(122, 178),
        };
    }

    /// <summary>The 300-byte record image, as shown by DISPLAY ACCOUNT-RECORD.</summary>
    public string ToImage() => string.Concat(
        AcctId.ToString("D11"),
        Text.PicX(ActiveStatus, 1),
        ZonedDecimal.Encode(CurrBal, 10, 2),
        ZonedDecimal.Encode(CreditLimit, 10, 2),
        ZonedDecimal.Encode(CashCreditLimit, 10, 2),
        Text.PicX(OpenDate, 10),
        Text.PicX(ExpirationDate, 10),
        Text.PicX(ReissueDate, 10),
        ZonedDecimal.Encode(CurrCycCredit, 10, 2),
        ZonedDecimal.Encode(CurrCycDebit, 10, 2),
        Text.PicX(AddrZip, 10),
        Text.PicX(GroupId, 10),
        Text.PicX(Filler, 178));
}
