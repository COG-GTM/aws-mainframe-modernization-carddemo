package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.CobolCodec;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthDetails;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthSummary;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Clock;

/**
 * Batch entry point replacing JCL UNLDGSAM step STEP01.
 * Usage: {@code java -jar dbunldgs.jar <DBPAUTP0 unload file> <PASFILOP output> <PADFILOP output>}.
 * Exit code is the program RETURN-CODE (0 or 16); 12 for a usage/IO error before the program starts.
 */
public final class DbunldgsMain {

    public static final int RC_SETUP_ERROR = 12;

    private DbunldgsMain() {
    }

    public static void main(String[] args) {
        System.exit(run(args));
    }

    public static int run(String[] args) {
        if (args.length != 3) {
            System.err.println("Usage: DbunldgsMain <DBPAUTP0-unload> <PASFILOP> <PADFILOP>");
            return RC_SETUP_ERROR;
        }
        InMemoryPautDatabase db;
        try {
            db = ImsUnloadReader.load(Path.of(args[0]), CobolCodec.EBCDIC);
        } catch (IOException | IllegalArgumentException e) {
            System.err.println("Unable to load DBPAUTP0 unload: " + e.getMessage());
            return RC_SETUP_ERROR;
        }
        try (FixedWidthGsamFile pasfl = new FixedWidthGsamFile(Path.of(args[1]), PendingAuthSummary.LENGTH);
             FixedWidthGsamFile padfl = new FixedWidthGsamFile(Path.of(args[2]), PendingAuthDetails.LENGTH)) {
            return new Dbunldgs(db, pasfl, padfl, System.out::println, Clock.systemDefaultZone(), CobolCodec.EBCDIC).run();
        } catch (IOException | RuntimeException e) {
            System.err.println("GSAM dataset error: " + e.getMessage());
            return RC_SETUP_ERROR;
        }
    }
}
