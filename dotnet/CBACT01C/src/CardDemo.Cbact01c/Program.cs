using CardDemo.Cbact01c;
using CardDemo.Cbact01c.Cobol;

// Files are bound like JCL DD statements: environment variables named after
// the DD (ACCTFILE, OUTFILE, ARRYFILE, VBRCFILE), or positional arguments.
static string Dd(string[] args, int index, string ddName) =>
    args.Length > index ? args[index]
    : Environment.GetEnvironmentVariable(ddName)
      ?? throw new ArgumentException($"Missing {ddName}: pass it as argument {index + 1} or set ${ddName}.");

var files = new Cbact01cFiles(
    Dd(args, 0, "ACCTFILE"), Dd(args, 1, "OUTFILE"), Dd(args, 2, "ARRYFILE"), Dd(args, 3, "VBRCFILE"));

using var stdout = Console.OpenStandardOutput();
using var sysout = new StreamWriter(stdout, Text.Latin1) { AutoFlush = false };
return new Cbact01cProgram(files, sysout).Run();
