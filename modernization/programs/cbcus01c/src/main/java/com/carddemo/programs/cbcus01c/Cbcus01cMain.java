package com.carddemo.programs.cbcus01c;

import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

/**
 * Command-line entry equivalent to JCL READCUST STEP05:
 * {@code java -jar target/cbcus01c.jar [--fixed-block] [--charset=NAME] <CUSTFILE>}.
 *
 * <p>Default input is line-sequential ASCII (app/data/ASCII/custdata.txt). {@code --fixed-block}
 * reads the RECFM=FB feed and defaults the charset to EBCDIC (IBM037). Exit code is RETURN-CODE (0), the abend code
 * (999; the OS reports it modulo 256), or {@value #RC_SYSOUT_ERROR} if SYSOUT could not be written.
 */
public final class Cbcus01cMain {

    static final int RC_USAGE = 16;
    static final int RC_SYSOUT_ERROR = 12;

    private Cbcus01cMain() {
    }

    public static void main(String[] args) {
        System.exit(run(args, System.out, System.err));
    }

    static int run(String[] args, PrintStream sysout, PrintStream err) {
        Path custfile = null;
        boolean fixedBlock = false;
        String charsetName = null;
        for (String arg : args) {
            if (arg.equals("--fixed-block")) {
                fixedBlock = true;
            } else if (arg.startsWith("--charset=")) {
                charsetName = arg.substring("--charset=".length());
            } else if (custfile == null && !arg.startsWith("--")) {
                custfile = Path.of(arg);
            } else {
                custfile = null;
                break;
            }
        }
        CustomerFile file = null;
        if (custfile != null) {
            try {
                Charset charset = charsetName != null ? Charset.forName(charsetName)
                        : fixedBlock ? Cbcus01c.EBCDIC : StandardCharsets.US_ASCII;
                file = fixedBlock ? KsdsCustomerFile.fixedBlock(custfile, charset)
                        : new KsdsCustomerFile(custfile, charset);
            } catch (IllegalArgumentException e) {
                err.println("CBCUS01C: invalid --charset: " + e.getMessage());
            }
        }
        if (file == null) {
            err.println("usage: Cbcus01cMain [--fixed-block] [--charset=NAME] <CUSTFILE path>");
            return RC_USAGE;
        }
        return execute(file, sysout);
    }

    static int execute(Path custfile) {
        return execute(new KsdsCustomerFile(custfile), System.out);
    }

    static int execute(CustomerFile custfile, PrintStream sysout) {
        boolean interruptedBefore = Thread.currentThread().isInterrupted();
        Cbcus01c program = new Cbcus01c(custfile, sysout::println);
        int rc;
        try {
            rc = program.run();
        } catch (AbendException e) {
            rc = e.abendCode();
        }
        // PrintStream swallows InterruptedIOException and only re-interrupts the thread.
        boolean interruptedDuringRun = !interruptedBefore && Thread.currentThread().isInterrupted();
        if (sysout.checkError() || interruptedDuringRun) {
            System.err.println("CBCUS01C: error writing SYSOUT");
            return rc == 0 ? RC_SYSOUT_ERROR : rc;
        }
        return rc;
    }
}
