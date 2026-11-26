package com.carddemo.programs.cbcus01c;

import java.nio.file.Path;

/**
 * Command-line entry equivalent to JCL READCUST STEP05: {@code java -jar cbcus01c.jar <CUSTFILE>}.
 * Exit code is RETURN-CODE (0) or the abend code (999; the OS reports it modulo 256).
 */
public final class Cbcus01cMain {

    private Cbcus01cMain() {
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("usage: Cbcus01cMain <CUSTFILE path>");
            System.exit(16);
        }
        System.exit(execute(Path.of(args[0])));
    }

    static int execute(Path custfile) {
        Cbcus01c program = new Cbcus01c(new KsdsCustomerFile(custfile), System.out::println);
        try {
            return program.run();
        } catch (AbendException e) {
            return e.abendCode();
        }
    }
}
