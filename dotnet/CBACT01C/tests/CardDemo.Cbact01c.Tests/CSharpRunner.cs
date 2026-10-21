using CardDemo.Cbact01c.Cobol;

namespace CardDemo.Cbact01c.Tests;

/// <summary>Runs the C# port in-process into a temp directory (same file names as the COBOL DDs).</summary>
internal static class CSharpRunner
{
    public static (int ReturnCode, string OutDir) Run(string acctFile, string? outFile = null)
    {
        string outDir = Directory.CreateTempSubdirectory("cbact01c-cs-").FullName;
        var files = new Cbact01cFiles(
            acctFile,
            outFile ?? Path.Combine(outDir, "OUTFILE"),
            Path.Combine(outDir, "ARRYFILE"),
            Path.Combine(outDir, "VBRCFILE"));

        using var stdout = File.Create(Path.Combine(outDir, "stdout.txt"));
        using var writer = new StreamWriter(stdout, Text.Latin1);
        int rc = new Cbact01cProgram(files, writer).Run();
        return (rc, outDir);
    }
}
