package com.carddemo.programs.cbimport;

import com.carddemo.programs.cbimport.copybook.AccountRecord;
import com.carddemo.programs.cbimport.copybook.CardRecord;
import com.carddemo.programs.cbimport.copybook.CardXrefRecord;
import com.carddemo.programs.cbimport.copybook.CustomerRecord;
import com.carddemo.programs.cbimport.copybook.TranRecord;
import com.carddemo.programs.cbimport.io.FixedWidthExportFileReader;
import com.carddemo.programs.cbimport.io.FixedWidthRecordFileWriter;
import com.carddemo.programs.cbimport.io.RecordWriter;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Job step equivalent of app/jcl/CBIMPORT.jcl (STEP01 EXEC PGM=CBIMPORT).
 *
 * <pre>
 * java -jar target/cbimport-1.0.0-SNAPSHOT.jar --EXPFILE=export.dat --CUSTOUT=cust.dat --ACCTOUT=acct.dat --XREFOUT=xref.dat
 *      --TRNXOUT=tran.dat --CARDOUT=card.dat --ERROUT=err.dat
 *      [--export-encoding=IBM037] [--output-encoding=IBM037] [--record-separator=none|lf]
 * </pre>
 *
 * A DD omitted from the arguments behaves as a DD missing from the JCL (OPEN status '35'). On ABEND
 * the output datasets this run created are deleted, mirroring DISP=(NEW,CATLG,DELETE), and the step ends with
 * {@link #ABEND_EXIT_CODE}.
 */
public final class CbimportMain {

    public static final int ABEND_EXIT_CODE = 16;
    public static final int USAGE_EXIT_CODE = 8;

    private CbimportMain() {
    }

    public static void main(String[] args) {
        System.exit(run(args, Clock.systemDefaultZone(), System.out));
    }

    public static int run(String[] args, Clock clock, PrintStream sysout) {
        Map<String, String> opts = new HashMap<>();
        for (String arg : args) {
            int eq = arg.indexOf('=');
            if (!arg.startsWith("--") || eq < 0) {
                sysout.println("CBIMPORT: Invalid argument: " + arg);
                return USAGE_EXIT_CODE;
            }
            opts.put(arg.substring(2, eq).toUpperCase(Locale.ROOT), arg.substring(eq + 1));
        }
        Charset exportCharset = charset(opts.getOrDefault("EXPORT-ENCODING", "IBM037"));
        Charset outputCharset = charset(opts.getOrDefault("OUTPUT-ENCODING", "IBM037"));
        if (exportCharset == null || outputCharset == null) {
            sysout.println("CBIMPORT: Unsupported encoding: " + opts.getOrDefault("EXPORT-ENCODING", "IBM037")
                    + " / " + opts.getOrDefault("OUTPUT-ENCODING", "IBM037"));
            return USAGE_EXIT_CODE;
        }
        String separator = "lf".equalsIgnoreCase(opts.getOrDefault("RECORD-SEPARATOR", "none")) ? "\n" : "";

        List<FixedWidthRecordFileWriter> created = new ArrayList<>();
        CbimportFiles files = new CbimportFiles(
                new FixedWidthExportFileReader(path(opts, "EXPFILE")),
                writer(opts, "CUSTOUT", CustomerRecord.LENGTH, outputCharset, separator, created),
                writer(opts, "ACCTOUT", AccountRecord.LENGTH, outputCharset, separator, created),
                writer(opts, "XREFOUT", CardXrefRecord.LENGTH, outputCharset, separator, created),
                writer(opts, "TRNXOUT", TranRecord.LENGTH, outputCharset, separator, created),
                writer(opts, "CARDOUT", CardRecord.LENGTH, outputCharset, separator, created),
                writer(opts, "ERROUT", ErrorRecord.OUTPUT_LENGTH, outputCharset, separator, created));
        try {
            return new Cbimport(files, exportCharset, clock, sysout).run();
        } catch (AbendException e) {
            files.expfile().close();
            for (FixedWidthRecordFileWriter w : created) {
                w.close();
                if (!w.created()) {
                    continue;
                }
                try {
                    Files.deleteIfExists(w.path());
                } catch (IOException ignored) {
                    // best effort, as with DISP abnormal-termination processing
                }
            }
            return ABEND_EXIT_CODE;
        }
    }

    /**
     * Returns the charset when it keeps one byte per character and, for EBCDIC code pages, maps the
     * overpunch characters to the zoned sign nibbles ('{'/A-I = X'C0'-X'C9', '}'/J-R = X'D0'-X'D9'),
     * as IBM037 does. Otherwise returns null.
     */
    static Charset charset(String name) {
        Charset cs;
        try {
            cs = Charset.forName(name);
        } catch (IllegalArgumentException e) {
            return null;
        }
        if (!cs.canEncode() || cs.newEncoder().maxBytesPerChar() != 1f) {
            return null;
        }
        String repertoire = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 {}-.,/:";
        if (!cs.newEncoder().canEncode(repertoire)
                || !new String(repertoire.getBytes(cs), cs).equals(repertoire)) {
            return null;
        }
        String zoned = "{ABCDEFGHI}JKLMNOPQR0123456789";
        byte[] b = zoned.getBytes(cs);
        if (b.length != zoned.length()) {
            return null;
        }
        if ((b[20] & 0xFF) != 0xF0) {
            return cs;
        }
        for (int i = 0; i < 10; i++) {
            if ((b[i] & 0xFF) != 0xC0 + i || (b[10 + i] & 0xFF) != 0xD0 + i || (b[20 + i] & 0xFF) != 0xF0 + i) {
                return null;
            }
        }
        return cs;
    }

    private static Path path(Map<String, String> opts, String dd) {
        String p = opts.get(dd);
        return p == null ? null : Path.of(p);
    }

    private static RecordWriter writer(Map<String, String> opts, String dd, int lrecl, Charset cs,
                                       String separator, List<FixedWidthRecordFileWriter> created) {
        Path p = path(opts, dd);
        if (p == null) {
            return RecordWriter.unallocated();
        }
        FixedWidthRecordFileWriter w = new FixedWidthRecordFileWriter(p, lrecl, cs, separator);
        created.add(w);
        return w;
    }
}
