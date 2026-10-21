using System.Diagnostics;
using System.Text;

namespace CardDemo.Cbact01c.Tests;

/// <summary>
/// The C# port must reproduce the GnuCOBOL run of the original CBACT01C byte-for-byte:
/// SYSOUT (DISPLAY output) plus all three output datasets.
/// </summary>
public class ParityTests
{
    public static readonly TheoryData<string> Datasets = new() { "acctdata", "edge_cases", "duplicate_keys" };
    private static readonly string[] Artifacts = ["stdout.txt", "OUTFILE", "ARRYFILE", "VBRCFILE"];

    /// <summary>Compares against golden output captured from GnuCOBOL (committed under golden/).</summary>
    [Theory]
    [MemberData(nameof(Datasets))]
    public void CSharpOutput_MatchesCommittedGnuCobolGolden(string dataset)
    {
        var (rc, csDir) = CSharpRunner.Run(TestPaths.TestData(dataset));

        Assert.Equal(0, rc);
        foreach (var artifact in Artifacts)
            AssertSameBytes(Path.Combine(TestPaths.Golden(dataset), artifact), Path.Combine(csDir, artifact));
    }

    /// <summary>Compiles and runs the original COBOL with GnuCOBOL right now, then compares.</summary>
    [SkippableTheory]
    [MemberData(nameof(Datasets))]
    public void CSharpOutput_MatchesLiveGnuCobolRun(string dataset)
    {
        Skip.If(FindOnPath("cobc") is null, "GnuCOBOL (cobc) is not installed.");

        string cobolDir = Directory.CreateTempSubdirectory("cbact01c-cobol-").FullName;
        RunProcess("bash", TestPaths.CobolHarness, TestPaths.TestData(dataset), cobolDir);
        var (rc, csDir) = CSharpRunner.Run(TestPaths.TestData(dataset));

        Assert.Equal(0, rc);
        foreach (var artifact in Artifacts)
            AssertSameBytes(Path.Combine(cobolDir, artifact), Path.Combine(csDir, artifact));
    }

    [Fact]
    public void MissingAcctFile_AbendsLikeCobol()
    {
        var (rc, csDir) = CSharpRunner.Run(Path.Combine(Path.GetTempPath(), "does-not-exist.ksds"));

        Assert.Equal(999, rc);
        AssertSameBytes(
            Path.Combine(TestPaths.Golden("missing_acctfile"), "stdout.txt"),
            Path.Combine(csDir, "stdout.txt"));
    }

    [SkippableFact]
    public void UnreadableAcctFile_Abends()
    {
        Skip.If(OperatingSystem.IsWindows() || Environment.UserName == "root", "Needs POSIX permissions as non-root.");
        string acct = Path.Combine(Directory.CreateTempSubdirectory("cbact01c-acct-").FullName, "ACCTFILE");
        File.Copy(TestPaths.TestData("acctdata"), acct);
        if (!OperatingSystem.IsWindows())
            File.SetUnixFileMode(acct, UnixFileMode.None);

        var (rc, csDir) = CSharpRunner.Run(acct);

        Assert.Equal(999, rc);
        Assert.Equal(
            "START OF EXECUTION OF PROGRAM CBACT01C\nERROR OPENING ACCTFILE\nFILE STATUS IS: NNNN0037\nABENDING PROGRAM\n",
            File.ReadAllText(Path.Combine(csDir, "stdout.txt")));
    }

    [SkippableFact]
    public void FailedOutputWrite_Abends()
    {
        Skip.IfNot(File.Exists("/dev/full"), "Needs /dev/full to simulate a full disk.");

        var (rc, csDir) = CSharpRunner.Run(TestPaths.TestData("acctdata"), outFile: "/dev/full");

        Assert.Equal(999, rc);
        Assert.EndsWith(
            "ACCOUNT FILE WRITE STATUS IS:30\nFILE STATUS IS: NNNN0030\nABENDING PROGRAM\n",
            File.ReadAllText(Path.Combine(csDir, "stdout.txt")));
    }

    private static void AssertSameBytes(string expectedPath, string actualPath)
    {
        byte[] expected = File.ReadAllBytes(expectedPath);
        byte[] actual = File.ReadAllBytes(actualPath);
        int diff = Enumerable.Range(0, Math.Min(expected.Length, actual.Length))
                             .FirstOrDefault(i => expected[i] != actual[i], -1);
        Assert.True(diff < 0 && expected.Length == actual.Length,
            $"{Path.GetFileName(expectedPath)} differs (COBOL {expected.Length} bytes, C# {actual.Length} bytes, " +
            $"first difference at offset {(diff < 0 ? Math.Min(expected.Length, actual.Length) : diff)}).");
    }

    private static void RunProcess(string file, params string[] args)
    {
        var psi = new ProcessStartInfo(file) { RedirectStandardError = true, RedirectStandardOutput = true };
        foreach (var a in args) psi.ArgumentList.Add(a);
        using var p = Process.Start(psi)!;
        var stderr = new StringBuilder(p.StandardError.ReadToEnd());
        p.WaitForExit();
        Assert.True(p.ExitCode == 0, $"{file} failed ({p.ExitCode}): {stderr}");
    }

    private static string? FindOnPath(string exe) =>
        (Environment.GetEnvironmentVariable("PATH") ?? "")
            .Split(Path.PathSeparator)
            .Select(d => Path.Combine(d, exe))
            .FirstOrDefault(File.Exists);
}
