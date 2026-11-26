package com.carddemo.programs.cbtrn02c;

import com.carddemo.programs.cbtrn02c.copybook.AccountRecord;
import com.carddemo.programs.cbtrn02c.copybook.CardXrefRecord;
import com.carddemo.programs.cbtrn02c.copybook.DalytranRecord;
import com.carddemo.programs.cbtrn02c.copybook.TranCatBalRecord;
import com.carddemo.programs.cbtrn02c.copybook.TranRecord;
import com.carddemo.programs.cbtrn02c.io.FileIndexedFile;
import com.carddemo.programs.cbtrn02c.io.LineSequentialReader;
import com.carddemo.programs.cbtrn02c.io.LineSequentialWriter;

import java.nio.file.Path;
import java.time.Clock;
import java.util.HashMap;
import java.util.Map;

/**
 * Command-line equivalent of JCL POSTTRAN STEP15. Each DD is passed as {@code DDNAME=path}
 * (line-per-record fixed-width files):
 * <pre>DALYTRAN=... TRANFILE=... XREFFILE=... DALYREJS=... ACCTFILE=... TCATBALF=...</pre>
 * Exit status is RETURN-CODE (0 or 4); an ABEND exits with the abend code (999, reported by POSIX as 231).
 */
public final class Cbtrn02cMain {

    private Cbtrn02cMain() {
    }

    public static void main(String[] args) {
        System.exit(execute(args));
    }

    static int execute(String[] args) {
        Map<String, Path> dd = new HashMap<>();
        for (String a : args) {
            int eq = a.indexOf('=');
            if (eq > 0) {
                dd.put(a.substring(0, eq).toUpperCase(), Path.of(a.substring(eq + 1)));
            }
        }
        for (String name : new String[] {"DALYTRAN", "TRANFILE", "XREFFILE", "DALYREJS", "ACCTFILE", "TCATBALF"}) {
            if (!dd.containsKey(name)) {
                System.err.println("MISSING DD " + name);
                return 12;
            }
        }
        Cbtrn02c program = new Cbtrn02c(
                new LineSequentialReader(dd.get("DALYTRAN"), DalytranRecord.RECORD_LENGTH),
                new FileIndexedFile(dd.get("TRANFILE"), TranRecord.RECORD_LENGTH, TranRecord.KEY_OFFSET,
                        TranRecord.KEY_LENGTH),
                new FileIndexedFile(dd.get("XREFFILE"), CardXrefRecord.RECORD_LENGTH, CardXrefRecord.KEY_OFFSET,
                        CardXrefRecord.KEY_LENGTH),
                new LineSequentialWriter(dd.get("DALYREJS"), Cbtrn02c.REJECT_RECORD_LENGTH),
                new FileIndexedFile(dd.get("ACCTFILE"), AccountRecord.RECORD_LENGTH, AccountRecord.KEY_OFFSET,
                        AccountRecord.KEY_LENGTH),
                new FileIndexedFile(dd.get("TCATBALF"), TranCatBalRecord.RECORD_LENGTH, TranCatBalRecord.KEY_OFFSET,
                        TranCatBalRecord.KEY_LENGTH),
                Clock.systemDefaultZone(),
                System.out::println);
        try {
            return program.run();
        } catch (Cbtrn02cAbendException e) {
            return e.abendCode();
        }
    }
}
