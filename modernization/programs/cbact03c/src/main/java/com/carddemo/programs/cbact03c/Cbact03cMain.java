package com.carddemo.programs.cbact03c;

import java.io.PrintStream;
import java.nio.file.Path;

/**
 * JCL READXREF STEP05 equivalent: {@code EXEC PGM=CBACT03C}, {@code XREFFILE DD} = CARDXREF KSDS,
 * SYSOUT = stdout.
 *
 * <p>Usage: {@code java -jar cbact03c.jar <xreffile> [--ebcdic]}. The XREFFILE path may also come
 * from the {@code XREFFILE} environment variable. Exit code 0 on GOBACK; on ABEND the abend code
 * (999) is reported and the process exits with {@link #ABEND_EXIT_CODE} (POSIX shells see 999 &amp; 0xFF = 231).
 */
public final class Cbact03cMain {

    public static final int ABEND_EXIT_CODE = 999;
    public static final int USAGE_EXIT_CODE = 16;

    private Cbact03cMain() {
    }

    public static void main(String[] args) {
        System.exit(run(args, System.getenv("XREFFILE"), System.out, System.err));
    }

    static int run(String[] args, String xrefEnv, PrintStream out, PrintStream err) {
        boolean ebcdic = false;
        String file = xrefEnv;
        for (String arg : args) {
            if ("--ebcdic".equals(arg)) {
                ebcdic = true;
            } else {
                file = arg;
            }
        }
        if (file == null || file.isBlank()) {
            err.println("usage: cbact03c <xreffile> [--ebcdic]  (or set XREFFILE)");
            return USAGE_EXIT_CODE;
        }
        Path path = Path.of(file);
        XrefFile xref = ebcdic ? FixedWidthXrefFile.ebcdic(path) : FixedWidthXrefFile.ascii(path);
        try {
            new Cbact03c(xref, out::println).run();
            return 0;
        } catch (AbendException e) {
            err.println(e.getMessage());
            return ABEND_EXIT_CODE;
        }
    }
}
