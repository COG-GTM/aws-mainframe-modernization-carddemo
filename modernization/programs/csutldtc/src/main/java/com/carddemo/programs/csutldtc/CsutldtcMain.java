package com.carddemo.programs.csutldtc;

import java.io.PrintStream;
import java.nio.charset.StandardCharsets;

/**
 * Command-line entry: {@code java -cp target/csutldtc-1.0.0-SNAPSHOT.jar com.carddemo.programs.csutldtc.CsutldtcMain <date> <format>}.
 * Writes LS-RESULT (80 bytes, ISO-8859-1) to stdout and exits with RETURN-CODE.
 */
public final class CsutldtcMain {
    /** Usage error exit code (CLI only; the COBOL program has no such path). */
    public static final int USAGE_ERROR = 16;

    private CsutldtcMain() {
    }

    public static void main(String[] args) {
        PrintStream out = new PrintStream(System.out, true, StandardCharsets.ISO_8859_1);
        System.exit(run(args, out, System.err, new Csutldtc()));
    }

    static int run(String[] args, PrintStream out, PrintStream err, Csutldtc program) {
        if (args.length != 2) {
            err.println("usage: csutldtc <date> <format>   e.g. csutldtc 2022-06-10 YYYY-MM-DD");
            return USAGE_ERROR;
        }
        CsutldtcResult result = program.call(args[0], args[1]);
        out.print(result.lsResult());
        out.println();
        return result.returnCode();
    }
}
