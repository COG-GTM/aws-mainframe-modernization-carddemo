package com.carddemo.programs.cbexport;

import com.carddemo.programs.cbexport.copybook.AccountRecord;
import com.carddemo.programs.cbexport.copybook.CardRecord;
import com.carddemo.programs.cbexport.copybook.CardXrefRecord;
import com.carddemo.programs.cbexport.copybook.CustomerRecord;
import com.carddemo.programs.cbexport.copybook.ExportRecord;
import com.carddemo.programs.cbexport.copybook.TranRecord;
import com.carddemo.programs.cbexport.io.FixedLengthRecordReader;
import com.carddemo.programs.cbexport.io.FixedLengthRecordWriter;
import com.carddemo.programs.cbexport.io.LineSequentialRecordReader;
import com.carddemo.programs.cbexport.io.RecordReader;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.time.Clock;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Command-line stand-in for JCL step STEP02 of app/jcl/CBEXPORT.jcl.
 *
 * <pre>
 * java -jar target/cbexport-1.0.0-SNAPSHOT.jar CUSTFILE=custdata.txt ACCTFILE=acctdata.txt XREFFILE=cardxref.txt \
 *      TRANSACT=dailytran.txt CARDFILE=carddata.txt EXPFILE=export.dat [--input-format=line|fixed]
 * </pre>
 *
 * Inputs default to newline-delimited fixed-width text (app/data/ASCII); {@code fixed} reads
 * RECFM=F blocks. EXPFILE is always written as RECFM=F 500-byte records, keyed on
 * EXPORT-SEQUENCE-NUM.
 */
public final class CbexportMain {

    public static final int RC_OK = 0;
    /** Return code used for CEE3ABD (abend) and for invalid invocation. */
    public static final int RC_ABEND = 16;
    public static final List<String> DD_NAMES =
            List.of("CUSTFILE", "ACCTFILE", "XREFFILE", "TRANSACT", "CARDFILE", "EXPFILE");

    private CbexportMain() {
    }

    public static void main(String[] args) {
        System.exit(run(args, Clock.systemDefaultZone(), System.out, System.err));
    }

    public static int run(String[] args, Clock clock, PrintStream out, PrintStream err) {
        Map<String, Path> dd = new HashMap<>();
        boolean fixed = false;
        for (String arg : args) {
            if (arg.equals("--input-format=fixed")) {
                fixed = true;
            } else if (arg.equals("--input-format=line")) {
                fixed = false;
            } else {
                int eq = arg.indexOf('=');
                if (eq <= 0 || !DD_NAMES.contains(arg.substring(0, eq))) {
                    err.println("Unrecognised argument: " + arg);
                    return RC_ABEND;
                }
                try {
                    dd.put(arg.substring(0, eq), Path.of(arg.substring(eq + 1)));
                } catch (InvalidPathException e) {
                    err.println("Invalid path for " + arg.substring(0, eq) + ": " + e.getMessage());
                    return RC_ABEND;
                }
            }
        }
        for (String name : DD_NAMES) {
            if (!dd.containsKey(name)) {
                err.println("Missing DD " + name + "=<path>");
                return RC_ABEND;
            }
        }

        Path exp = dd.get("EXPFILE");
        for (String name : DD_NAMES) {
            if (!name.equals("EXPFILE") && sameFile(dd.get(name), exp)) {
                err.println("EXPFILE must not be the same file as " + name);
                return RC_ABEND;
            }
        }

        FixedLengthRecordWriter writer = new FixedLengthRecordWriter(dd.get("EXPFILE"), ExportRecord.LENGTH,
                ExportRecord.SEQUENCE_NUM_OFFSET, ExportRecord.SEQUENCE_NUM_LENGTH);
        CbexportFiles files = new CbexportFiles(
                reader(dd.get("CUSTFILE"), CustomerRecord.LENGTH, fixed),
                reader(dd.get("ACCTFILE"), AccountRecord.LENGTH, fixed),
                reader(dd.get("XREFFILE"), CardXrefRecord.LENGTH, fixed),
                reader(dd.get("TRANSACT"), TranRecord.LENGTH, fixed),
                reader(dd.get("CARDFILE"), CardRecord.LENGTH, fixed),
                writer);
        try {
            new Cbexport(files, clock, out::println).run();
            if (writer.publishFailed()) {
                err.println("EXPFILE could not be written: " + dd.get("EXPFILE"));
                return RC_ABEND;
            }
            return RC_OK;
        } catch (CbexportAbendException e) {
            return RC_ABEND;
        } finally {
            files.closeAll();
        }
    }

    private static boolean sameFile(Path a, Path b) {
        // An input under EXPFILE (e.g. EXPFILE is a directory symlink) would lose its path on replace.
        if (a.toAbsolutePath().normalize().startsWith(b.toAbsolutePath().normalize())) {
            return true;
        }
        try {
            return Files.exists(a) && Files.exists(b) && Files.isSameFile(a, b);
        } catch (IOException e) {
            return false;
        }
    }

    private static RecordReader reader(Path path, int length, boolean fixed) {
        return fixed ? new FixedLengthRecordReader(path, length) : new LineSequentialRecordReader(path, length);
    }
}
