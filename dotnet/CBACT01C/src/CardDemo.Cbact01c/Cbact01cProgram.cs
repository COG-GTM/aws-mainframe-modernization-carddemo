using CardDemo.Cbact01c.Cobol;
using CardDemo.Cbact01c.Copybooks;

namespace CardDemo.Cbact01c;

/// <summary>DD-name to path mapping (ACCTFILE, OUTFILE, ARRYFILE, VBRCFILE).</summary>
public sealed record Cbact01cFiles(string AcctFile, string OutFile, string ArryFile, string VbrcFile);

/// <summary>
/// C# port of app/cbl/CBACT01C.cbl. Each method mirrors one COBOL paragraph.
/// </summary>
public sealed class Cbact01cProgram
{
    private const int AbendCode = 999;

    private readonly Cbact01cFiles _files;
    private readonly TextWriter _sysout;

    // FILE SECTION record areas (persist between records, like FD buffers).
    private readonly OutAccountRecord _outAcctRec = new();
    private readonly ArrayRecord _arrArrayRec = new();

    // WORKING-STORAGE.
    private readonly DateConversionRecord _codatecnRec = new();
    private AccountRecord _accountRecord = new();
    private VbrcRecord1 _vbrcRec1 = new();
    private readonly VbrcRecord2 _vbrcRec2 = new();
    private string _wsAcctReissueYyyy = "    ";
    private bool _endOfFile;

    private IEnumerator<string>? _acctFile;
    private Stream? _outFile, _arryFile, _vbrcFile;

    public Cbact01cProgram(Cbact01cFiles files, TextWriter sysout)
    {
        _files = files;
        _sysout = sysout;
    }

    /// <summary>Runs the batch step. Returns the process return code (0, or 999 on ABEND).</summary>
    public int Run()
    {
        try
        {
            Display("START OF EXECUTION OF PROGRAM CBACT01C");
            AcctFileOpen0000();
            OutFileOpen2000();
            ArrFileOpen3000();
            VbrFileOpen4000();

            while (!_endOfFile)
            {
                AcctFileGetNext1000();
                if (!_endOfFile)
                    Display(_accountRecord.ToImage());
            }

            AcctFileClose9000();
            Display("END OF EXECUTION OF PROGRAM CBACT01C");
            return 0;
        }
        catch (AbendException ex)
        {
            return ex.Code;
        }
        finally
        {
            _outFile?.Dispose();
            _arryFile?.Dispose();
            _vbrcFile?.Dispose();
            _sysout.Flush();
        }
    }

    // 1000-ACCTFILE-GET-NEXT
    private void AcctFileGetNext1000()
    {
        if (!_acctFile!.MoveNext())
        {
            _endOfFile = true; // status '10' -> APPL-EOF
            return;
        }

        _accountRecord = AccountRecord.Parse(_acctFile.Current);
        _arrArrayRec.Initialize();
        DisplayAcctRecord1100();
        PopulAcctRecord1300();
        WriteAcctRecord1350();
        PopulArrayRecord1400();
        WriteArryRecord1450();
        _vbrcRec1 = new VbrcRecord1(); // INITIALIZE VBRC-REC1
        PopulVbrcRecord1500();
        WriteVb1Record1550();
        WriteVb2Record1575();
    }

    // 1100-DISPLAY-ACCT-RECORD
    private void DisplayAcctRecord1100()
    {
        var a = _accountRecord;
        Display("ACCT-ID                 :" + a.AcctId.ToString("D11"));
        Display("ACCT-ACTIVE-STATUS      :" + a.ActiveStatus);
        Display("ACCT-CURR-BAL           :" + ZonedDecimal.Display(a.CurrBal, 10, 2));
        Display("ACCT-CREDIT-LIMIT       :" + ZonedDecimal.Display(a.CreditLimit, 10, 2));
        Display("ACCT-CASH-CREDIT-LIMIT  :" + ZonedDecimal.Display(a.CashCreditLimit, 10, 2));
        Display("ACCT-OPEN-DATE          :" + a.OpenDate);
        Display("ACCT-EXPIRAION-DATE     :" + a.ExpirationDate);
        Display("ACCT-REISSUE-DATE       :" + a.ReissueDate);
        Display("ACCT-CURR-CYC-CREDIT    :" + ZonedDecimal.Display(a.CurrCycCredit, 10, 2));
        Display("ACCT-CURR-CYC-DEBIT     :" + ZonedDecimal.Display(a.CurrCycDebit, 10, 2));
        Display("ACCT-GROUP-ID           :" + a.GroupId);
        Display("-------------------------------------------------");
    }

    // 1300-POPUL-ACCT-RECORD
    private void PopulAcctRecord1300()
    {
        var a = _accountRecord;
        var o = _outAcctRec;
        o.AcctId = a.AcctId;
        o.ActiveStatus = a.ActiveStatus;
        o.CurrBal = a.CurrBal;
        o.CreditLimit = a.CreditLimit;
        o.CashCreditLimit = a.CashCreditLimit;
        o.OpenDate = a.OpenDate;
        o.ExpirationDate = a.ExpirationDate;

        _codatecnRec.InpDate = Text.PicX(a.ReissueDate, 20);
        _wsAcctReissueYyyy = Text.PicX(a.ReissueDate, 10)[..4]; // WS-REISSUE-DATE REDEFINES
        _codatecnRec.Type = "2";
        _codatecnRec.OutType = "2";
        CobDatFt.Call(_codatecnRec); // CALL 'COBDATFT' USING CODATECN-REC
        o.ReissueDate = Text.PicX(_codatecnRec.OutDate, 10);

        o.CurrCycCredit = a.CurrCycCredit;
        if (a.CurrCycDebit == 0m)
            o.CurrCycDebit = 2525.00m;
        o.GroupId = a.GroupId;
    }

    // 1350-WRITE-ACCT-RECORD
    private void WriteAcctRecord1350() =>
        WriteRecord(_outFile!, _outAcctRec.ToBytes());

    // 1400-POPUL-ARRAY-RECORD
    private void PopulArrayRecord1400()
    {
        var r = _arrArrayRec;
        r.AcctId = _accountRecord.AcctId;
        r.Balances[0].CurrBal = _accountRecord.CurrBal;
        r.Balances[0].CurrCycDebit = 1005.00m;
        r.Balances[1].CurrBal = _accountRecord.CurrBal;
        r.Balances[1].CurrCycDebit = 1525.00m;
        r.Balances[2].CurrBal = -1025.00m;
        r.Balances[2].CurrCycDebit = -2500.00m;
    }

    // 1450-WRITE-ARRY-RECORD
    private void WriteArryRecord1450() =>
        WriteRecord(_arryFile!, _arrArrayRec.ToBytes());

    // 1500-POPUL-VBRC-RECORD
    private void PopulVbrcRecord1500()
    {
        _vbrcRec1.AcctId = _accountRecord.AcctId;
        _vbrcRec2.AcctId = _accountRecord.AcctId;
        _vbrcRec1.ActiveStatus = _accountRecord.ActiveStatus;
        _vbrcRec2.CurrBal = _accountRecord.CurrBal;
        _vbrcRec2.CreditLimit = _accountRecord.CreditLimit;
        _vbrcRec2.ReissueYyyy = _wsAcctReissueYyyy;
        Display("VBRC-REC1:" + _vbrcRec1.ToImage());
        Display("VBRC-REC2:" + _vbrcRec2.ToImage());
    }

    // 1550-WRITE-VB1-RECORD  (WS-RECD-LEN = 12)
    private void WriteVb1Record1550() =>
        WriteVariableRecord(_vbrcFile!, Text.Latin1.GetBytes(_vbrcRec1.ToImage()));

    // 1575-WRITE-VB2-RECORD  (WS-RECD-LEN = 39)
    private void WriteVb2Record1575() =>
        WriteVariableRecord(_vbrcFile!, Text.Latin1.GetBytes(_vbrcRec2.ToImage()));

    // 0000-ACCTFILE-OPEN
    private void AcctFileOpen0000()
    {
        if (!File.Exists(_files.AcctFile))
        {
            Display("ERROR OPENING ACCTFILE");
            DisplayIoStatus9910("35");
            AbendProgram9999();
        }
        List<string> records;
        try
        {
            records = ReadKsdsSequential(_files.AcctFile);
        }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException)
        {
            Display("ERROR OPENING ACCTFILE");
            DisplayIoStatus9910(ex is UnauthorizedAccessException ? "37" : "30");
            AbendProgram9999();
            throw; // unreachable
        }
        _acctFile = records.GetEnumerator();
    }

    // 2000-OUTFILE-OPEN
    private void OutFileOpen2000() => _outFile = OpenOutput(_files.OutFile, "ERROR OPENING OUTFILE");

    // 3000-ARRFILE-OPEN
    private void ArrFileOpen3000() => _arryFile = OpenOutput(_files.ArryFile, "ERROR OPENING ARRAYFILE");

    // 4000-VBRFILE-OPEN
    private void VbrFileOpen4000() => _vbrcFile = OpenOutput(_files.VbrcFile, "ERROR OPENING VBRC FILE");

    // 9000-ACCTFILE-CLOSE
    private void AcctFileClose9000()
    {
        _acctFile?.Dispose();
        // The legacy program never CLOSEs OUT-FILE / ARRY-FILE / VBRC-FILE
        // (GnuCOBOL closes them implicitly); Run() disposes them in finally.
    }

    // 9999-ABEND-PROGRAM
    private void AbendProgram9999()
    {
        Display("ABENDING PROGRAM");
        throw new AbendException(AbendCode); // CALL 'CEE3ABD' USING ABCODE(999), TIMING(0)
    }

    // 9910-DISPLAY-IO-STATUS
    private void DisplayIoStatus9910(string ioStatus)
    {
        char stat1 = ioStatus[0], stat2 = ioStatus[1];
        bool numeric = char.IsAsciiDigit(stat1) && char.IsAsciiDigit(stat2);
        string status04 = !numeric || stat1 == '9'
            ? stat1 + ((int)stat2).ToString("D3")   // binary value of IO-STAT2
            : "00" + ioStatus;
        Display("FILE STATUS IS: NNNN" + status04);
    }

    /// <summary>
    /// ACCTFILE is a VSAM KSDS read with ACCESS MODE SEQUENTIAL, i.e. in
    /// ascending FD-ACCT-ID order. The input here is the 300-byte flat
    /// extract that is REPRO'd into the KSDS; like REPRO into a KSDS, a
    /// duplicate key is rejected and the first record for that key is kept.
    /// </summary>
    private static List<string> ReadKsdsSequential(string path)
    {
        string content = File.ReadAllText(path, Text.Latin1);
        var lines = content.Split('\n');
        int count = lines.Length > 0 && lines[^1].Length == 0 ? lines.Length - 1 : lines.Length;
        return lines.Take(count)
                    .Select(l => Text.PicX(l.TrimEnd('\r'), AccountRecord.Length))
                    .OrderBy(AccountRecord.KeyOf, StringComparer.Ordinal)
                    .DistinctBy(AccountRecord.KeyOf)
                    .ToList();
    }

    private Stream OpenOutput(string path, string errorText)
    {
        try
        {
            return new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.Read, bufferSize: 0);
        }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException)
        {
            string status = ex is UnauthorizedAccessException ? "37" : "30";
            Display(errorText + status);
            DisplayIoStatus9910(status);
            AbendProgram9999();
            throw; // unreachable
        }
    }

    private void WriteRecord(Stream file, byte[] record)
    {
        try
        {
            file.Write(record);
        }
        catch (IOException)
        {
            Display("ACCOUNT FILE WRITE STATUS IS:30");
            DisplayIoStatus9910("30");
            AbendProgram9999();
        }
    }

    /// <summary>
    /// RECORDING MODE V record: 4-byte header (2-byte big-endian length + 2 zero bytes)
    /// followed by the data, matching GnuCOBOL's default variable sequential format.
    /// </summary>
    private void WriteVariableRecord(Stream file, byte[] data) =>
        WriteRecord(file, [(byte)(data.Length >> 8), (byte)data.Length, 0, 0, .. data]);

    private void Display(string line) => _sysout.Write(line + "\n");

    private sealed class AbendException(int code) : Exception
    {
        public int Code { get; } = code;
    }
}
