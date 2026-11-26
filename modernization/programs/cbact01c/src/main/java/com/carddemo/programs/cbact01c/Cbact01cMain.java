package com.carddemo.programs.cbact01c;

import com.carddemo.programs.cbact01c.copybook.AccountRecord;
import com.carddemo.programs.cbact01c.gateway.CobdatftDateFormatter;
import com.carddemo.programs.cbact01c.io.FixedRecordFileWriter;
import com.carddemo.programs.cbact01c.io.KsdsFileReader;
import com.carddemo.programs.cbact01c.io.VariableRecordFileWriter;
import com.carddemo.programs.cbact01c.record.ArrArrayRec;
import com.carddemo.programs.cbact01c.record.OutAcctRec;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

/**
 * Command-line equivalent of JCL app/jcl/READACCT.jcl step STEP05. DD names resolve from arguments
 * {@code ACCTFILE OUTFILE ARRYFILE VBRCFILE} or, when absent, from environment variables of the same names.
 * Exit code is 0, or {@value Cbact01c#ABEND_CODE} on abend (the OS may report it modulo 256). On abend the
 * three output datasets are deleted, matching their {@code DISP=(NEW,CATLG,DELETE)}.
 */
public final class Cbact01cMain {
    private Cbact01cMain() {
    }

    public static void main(String[] args) {
        System.exit(execute(args, System.getenv(), System.out));
    }

    public static int execute(String[] args, Map<String, String> env, PrintStream sysout) {
        Path acctfile = dd(args, 0, "ACCTFILE", env);
        Path outfile = dd(args, 1, "OUTFILE", env);
        Path arryfile = dd(args, 2, "ARRYFILE", env);
        Path vbrcfile = dd(args, 3, "VBRCFILE", env);
        Cbact01c program = new Cbact01c(
                new KsdsFileReader(acctfile, AccountRecord.LENGTH, AccountRecord.ACCT_ID.length()),
                new FixedRecordFileWriter(outfile, OutAcctRec.LENGTH),
                new FixedRecordFileWriter(arryfile, ArrArrayRec.LENGTH),
                new VariableRecordFileWriter(vbrcfile, 10, 80, VariableRecordFileWriter.HeaderFormat.IBM_RDW),
                new CobdatftDateFormatter(),
                sysout::println);
        try {
            return program.run();
        } catch (AbendException e) {
            for (Path p : List.of(outfile, arryfile, vbrcfile)) {
                try {
                    Files.deleteIfExists(p);
                } catch (IOException ignored) {
                    // best effort, as with abnormal-termination disposition
                }
            }
            return e.abendCode();
        }
    }

    private static Path dd(String[] args, int index, String ddName, Map<String, String> env) {
        String value = args.length > index ? args[index] : env.get(ddName);
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("DD " + ddName + " not allocated (pass as argument " + (index + 1)
                    + " or set environment variable " + ddName + ")");
        }
        return Path.of(value);
    }
}
