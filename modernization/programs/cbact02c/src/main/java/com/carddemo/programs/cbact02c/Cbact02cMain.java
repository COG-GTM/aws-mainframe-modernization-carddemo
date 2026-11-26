package com.carddemo.programs.cbact02c;

import com.carddemo.programs.cbact02c.io.FixedWidthCardFile;

import java.io.PrintStream;
import java.nio.file.Path;

/**
 * Batch entry point replacing JCL READCARD STEP05. The CARDFILE DD is supplied as the first
 * argument or the CARDFILE environment variable; SYSOUT is standard output.
 * Exit code 0 on GOBACK, {@value Cbact02c#ABEND_CODE} on abend (the OS may report it modulo 256).
 */
public final class Cbact02cMain {

    static final int USAGE_ERROR = 16;

    private Cbact02cMain() {
    }

    public static void main(String[] args) {
        System.exit(execute(args, System.getenv("CARDFILE"), System.out));
    }

    static int execute(String[] args, String cardfileEnv, PrintStream out) {
        String cardfile = args.length > 0 ? args[0] : cardfileEnv;
        if (cardfile == null || cardfile.isBlank()) {
            out.println("USAGE: Cbact02cMain <CARDFILE path> (or set CARDFILE)");
            return USAGE_ERROR;
        }
        Cbact02c program = new Cbact02c(new FixedWidthCardFile(Path.of(cardfile)), out::println);
        try {
            return program.run();
        } catch (AbendException e) {
            return e.abendCode();
        }
    }
}
