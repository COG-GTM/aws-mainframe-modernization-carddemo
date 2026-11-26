package com.carddemo.programs.cbact04c;

import com.carddemo.programs.cbact04c.copybook.AccountRecord;
import com.carddemo.programs.cbact04c.copybook.CardXrefRecord;
import com.carddemo.programs.cbact04c.copybook.DisGroupRecord;
import com.carddemo.programs.cbact04c.copybook.TranCatBalRecord;
import com.carddemo.programs.cbact04c.copybook.TranRecord;
import com.carddemo.programs.cbact04c.io.FixedWidthIndexedFile;
import com.carddemo.programs.cbact04c.io.FixedWidthSequentialInputFile;
import com.carddemo.programs.cbact04c.io.FixedWidthSequentialOutputFile;

import java.nio.file.Path;
import java.time.Clock;
import java.util.HashMap;
import java.util.Map;

/**
 * INTCALC.jcl STEP15 equivalent:
 * {@code java -jar cbact04c.jar PARM=2022071800 TCATBALF=... XREFFILE=... ACCTFILE=... DISCGRP=... TRANSACT=...}
 * Exit code is the RETURN-CODE (0), or the abend code (999) on ABEND; the OS reports 999 & 0xFF = 231.
 */
public final class Cbact04cMain {

    private Cbact04cMain() {
    }

    public static void main(String[] args) {
        System.exit(execute(args, Clock.systemDefaultZone()));
    }

    static int execute(String[] args, Clock clock) {
        Map<String, String> dd = new HashMap<>();
        for (String a : args) {
            int eq = a.indexOf('=');
            if (eq > 0) {
                dd.put(a.substring(0, eq).toUpperCase(), a.substring(eq + 1));
            }
        }
        for (String required : new String[] {"PARM", "TCATBALF", "XREFFILE", "ACCTFILE", "DISCGRP", "TRANSACT"}) {
            if (!dd.containsKey(required)) {
                System.err.println("Missing argument " + required + "=...");
                return 16;
            }
        }
        Cbact04cFiles files = new Cbact04cFiles(
                new FixedWidthSequentialInputFile<>(Path.of(dd.get("TCATBALF")), TranCatBalRecord.CODEC),
                new FixedWidthIndexedFile<>(Path.of(dd.get("XREFFILE")), CardXrefRecord.CODEC,
                        CardXrefRecord::xrefAcctId),
                new FixedWidthIndexedFile<>(Path.of(dd.get("DISCGRP")), DisGroupRecord.CODEC, DisGroupRecord::key),
                new FixedWidthIndexedFile<>(Path.of(dd.get("ACCTFILE")), AccountRecord.CODEC, AccountRecord::acctId),
                new FixedWidthSequentialOutputFile<>(Path.of(dd.get("TRANSACT")), TranRecord.CODEC));
        try {
            return new Cbact04c(files, clock, System.out::println).run(dd.get("PARM"));
        } catch (AbendException e) {
            System.err.println(e.getMessage());
            return e.abendCode();
        }
    }
}
