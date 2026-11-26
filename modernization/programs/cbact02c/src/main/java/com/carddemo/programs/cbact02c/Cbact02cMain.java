package com.carddemo.programs.cbact02c;

import com.carddemo.programs.cbact02c.io.CardFile;
import com.carddemo.programs.cbact02c.io.FixedWidthCardFile;

import java.io.PrintStream;
import java.nio.file.Path;

/**
 * Batch entry point replacing JCL READCARD STEP05:
 * {@code Cbact02cMain [--recfm=FB] <CARDFILE path>} (or set CARDFILE). Without {@code --recfm=FB}
 * the file is read as one 150-byte record per line; with it, as an EBCDIC RECFM=FB dataset.
 * SYSOUT is standard output. Exit code 0 on GOBACK, {@value Cbact02c#ABEND_CODE} on abend
 * (the OS may report it modulo 256), {@value #SYSOUT_WRITE_ERROR} if SYSOUT could not be written.
 */
public final class Cbact02cMain {

    static final int USAGE_ERROR = 16;
    static final int SYSOUT_WRITE_ERROR = 12;

    private Cbact02cMain() {
    }

    public static void main(String[] args) {
        System.exit(execute(args, System.getenv("CARDFILE"), System.out));
    }

    static int execute(String[] args, String cardfileEnv, PrintStream out) {
        boolean fixedBlock = false;
        String cardfile = cardfileEnv;
        for (String arg : args) {
            if (arg.equalsIgnoreCase("--recfm=FB")) {
                fixedBlock = true;
            } else {
                cardfile = arg;
            }
        }
        if (cardfile == null || cardfile.isBlank()) {
            out.println("USAGE: Cbact02cMain [--recfm=FB] <CARDFILE path> (or set CARDFILE)");
            return USAGE_ERROR;
        }
        Path path = Path.of(cardfile);
        CardFile file = fixedBlock ? FixedWidthCardFile.fixedBlockEbcdic(path) : new FixedWidthCardFile(path);
        Cbact02c program = new Cbact02c(file, out::println);
        int rc;
        try {
            rc = program.run();
        } catch (AbendException e) {
            rc = e.abendCode();
        }
        if (out.checkError()) {
            System.err.println("ERROR WRITING SYSOUT");
            return rc == 0 ? SYSOUT_WRITE_ERROR : rc;
        }
        return rc;
    }
}
