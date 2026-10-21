namespace CardDemo.Cbact01c.Tests;

internal static class TestPaths
{
    /// <summary>dotnet/CBACT01C - located by walking up to the folder holding cobol-harness/.</summary>
    public static readonly string Root = FindRoot();

    public static string TestData(string dataset) => Path.Combine(Root, "testdata", dataset + ".txt");
    public static string Golden(string dataset) => Path.Combine(Root, "golden", dataset);
    public static string CobolHarness => Path.Combine(Root, "cobol-harness", "run_cobol.sh");

    private static string FindRoot()
    {
        for (var dir = new DirectoryInfo(AppContext.BaseDirectory); dir != null; dir = dir.Parent)
            if (Directory.Exists(Path.Combine(dir.FullName, "cobol-harness")))
                return dir.FullName;
        throw new DirectoryNotFoundException("Could not locate dotnet/CBACT01C root.");
    }
}
