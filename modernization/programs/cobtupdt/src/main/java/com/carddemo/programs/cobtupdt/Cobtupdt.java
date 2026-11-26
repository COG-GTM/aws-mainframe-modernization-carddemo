package com.carddemo.programs.cobtupdt;

import com.carddemo.programs.cobtupdt.copybook.Sqlca;

import java.io.PrintStream;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * COBTUPDT - "Update Transaction type based on user input" (app/app-transaction-type-db2/cbl/COBTUPDT.cbl).
 *
 * <p>Batch step MNTTRDB2 (IKJEFT01 / DSN RUN PROGRAM(COBTUPDT) PLAN(CARDDEMO)) reads INPFILE and applies
 * each record to CARDDEMO.TRANSACTION_TYPE: A = INSERT, U = UPDATE, D = DELETE, * = comment.
 *
 * <p>9999-ABEND does not terminate: it DISPLAYs WS-RETURN-MSG, sets RETURN-CODE 4 and processing
 * continues with the next record. All output goes to SYSOUT via DISPLAY.
 */
public final class Cobtupdt {

    public static final int RC_OK = 0;
    public static final int RC_ERROR = 4;
    /** Exit code used by {@link #main} when the step cannot run at all (the COBOL loops or abends here). */
    public static final int RC_SEVERE = 12;

    public static final String ENV_JDBC_URL = "CARDDEMO_JDBC_URL";
    public static final String ENV_JDBC_USER = "CARDDEMO_JDBC_USER";
    public static final String ENV_JDBC_PASSWORD = "CARDDEMO_JDBC_PASSWORD";

    static final int WS_RETURN_MSG_LENGTH = 80;

    private final TrRecordReader trRecord;
    private final TransactionTypeGateway db;
    private final Consumer<String> display;

    // 01 FLAGS.
    private String lastrec = " ";
    // 01 WORKING-VARIABLES. 05 WS-RETURN-MSG PIC X(80) VALUE SPACES.
    private final char[] wsReturnMsg = spaces(WS_RETURN_MSG_LENGTH);
    // 01 WS-MISC-VARS. 05 WS-VAR-SQLCODE PIC ----9.
    private String wsVarSqlcode = "     ";
    // 01 WS-INF-STATUS.
    private String wsInfStatus = "  ";
    // 01 WS-INPUT-REC.
    private TrRecord wsInputRec = TrRecord.SPACES;
    // EXEC SQL INCLUDE SQLCA
    private final Sqlca sqlca = new Sqlca();
    // RETURN-CODE special register
    private int returnCode = RC_OK;

    public Cobtupdt(TrRecordReader trRecord, TransactionTypeGateway db, Consumer<String> display) {
        this.trRecord = Objects.requireNonNull(trRecord, "trRecord");
        this.db = Objects.requireNonNull(db, "db");
        this.display = Objects.requireNonNull(display, "display");
    }

    /**
     * PROCEDURE DIVISION: 0001-OPEN-FILES falls through into 1001-READ-NEXT-RECORDS, which ends with STOP RUN.
     *
     * @return RETURN-CODE (0, or 4 if any record hit 9999-ABEND)
     * @throws CobolFileStatusException if INPFILE could not be opened/read (the COBOL would loop forever)
     */
    public int run() {
        openFiles0001();
        readNextRecords1001();
        return returnCode;
    }

    // 0001-OPEN-FILES
    void openFiles0001() {
        wsInfStatus = trRecord.open();
        if ("00".equals(wsInfStatus)) {
            display.accept("OPEN FILE OK");
        } else {
            display.accept("OPEN FILE NOT OK");
        }
    }

    // 1001-READ-NEXT-RECORDS
    void readNextRecords1001() {
        try {
            readRecords1002();
            while (!"Y".equals(lastrec)) {
                treatRecord1003();
                readRecords1002();
            }
        } catch (RuntimeException e) {
            try {
                trRecord.close();
            } catch (RuntimeException suppressed) {
                e.addSuppressed(suppressed);
            }
            throw e;
        }
        closeStop2001();
    }

    // 1002-READ-RECORDS
    void readRecords1002() {
        Optional<String> record;
        try {
            record = trRecord.readNext();
        } catch (CobolFileStatusException e) {
            wsInfStatus = e.fileStatus();
            throw e;
        }
        if (record.isEmpty()) {
            lastrec = "Y";
        } else {
            wsInputRec = TrRecord.parse(record.get());
        }
        if (!"Y".equals(lastrec)) {
            display.accept("PROCESSING   " + wsInputRec.toFixedWidth());
        }
    }

    // 1003-TREAT-RECORD
    void treatRecord1003() {
        Optional<RecordAction> action = RecordAction.fromCode(wsInputRec.type());
        if (action.isEmpty()) {
            stringIntoReturnMsg("ERROR: TYPE NOT VALID");
            abend9999();
            return;
        }
        switch (action.get()) {
            case ADD -> {
                display.accept("ADDING RECORD");
                insertDb10031();
            }
            case UPDATE -> {
                display.accept("UPDATING RECORD");
                updateDb10032();
            }
            case DELETE -> {
                display.accept("DELETING RECORD");
                deleteDb10033();
            }
            case COMMENT -> display.accept("IGNORING COMMENTED LINE");
        }
    }

    // 10031-INSERT-DB
    void insertDb10031() {
        sqlca.setSqlcode(db.insert(wsInputRec.number(), wsInputRec.desc()));
        wsVarSqlcode = editSqlcode(sqlca.sqlcode());
        if (sqlca.isOk()) {
            display.accept("RECORD INSERTED SUCCESSFULLY");
        } else if (sqlca.isError()) {
            stringIntoReturnMsg(tableErrorMessage());
            abend9999();
        }
    }

    // 10032-UPDATE-DB
    void updateDb10032() {
        sqlca.setSqlcode(db.update(wsInputRec.number(), wsInputRec.desc()));
        wsVarSqlcode = editSqlcode(sqlca.sqlcode());
        if (sqlca.isOk()) {
            display.accept("RECORD UPDATED SUCCESSFULLY");
        } else if (sqlca.isNotFound()) {
            stringIntoReturnMsg("No records found.");
            abend9999();
        } else if (sqlca.isError()) {
            stringIntoReturnMsg(tableErrorMessage());
            abend9999();
        }
    }

    // 10033-DELETE-DB
    void deleteDb10033() {
        sqlca.setSqlcode(db.delete(wsInputRec.number()));
        wsVarSqlcode = editSqlcode(sqlca.sqlcode());
        if (sqlca.isOk()) {
            display.accept("RECORD DELETED SUCCESSFULLY");
        } else if (sqlca.isNotFound()) {
            stringIntoReturnMsg("No records found.");
            abend9999();
        } else if (sqlca.isError()) {
            stringIntoReturnMsg(tableErrorMessage());
            abend9999();
        }
    }

    // 9999-ABEND
    void abend9999() {
        display.accept(new String(wsReturnMsg));
        returnCode = RC_ERROR;
    }

    // 2001-CLOSE-STOP
    void closeStop2001() {
        wsInfStatus = trRecord.close();
    }

    private String tableErrorMessage() {
        return "Error accessing:" + " TRANSACTION_TYPE table. SQLCODE:" + wsVarSqlcode;
    }

    /**
     * STRING ... DELIMITED BY SIZE INTO WS-RETURN-MSG: overwrites from position 1 and leaves the rest of
     * the 80-byte field untouched (the COBOL never clears WS-RETURN-MSG), truncating at 80.
     */
    void stringIntoReturnMsg(String text) {
        int n = Math.min(text.length(), WS_RETURN_MSG_LENGTH);
        text.getChars(0, n, wsReturnMsg, 0);
    }

    /**
     * MOVE SQLCODE TO WS-VAR-SQLCODE (PIC ----9): 5 positions, floating minus, at most 4 digits
     * (high-order digits are truncated), positive values unsigned.
     */
    static String editSqlcode(int sqlcode) {
        long magnitude = Math.abs((long) sqlcode) % 10_000;
        String edited = (sqlcode < 0 ? "-" : "") + magnitude;
        return " ".repeat(5 - edited.length()) + edited;
    }

    private static char[] spaces(int n) {
        char[] c = new char[n];
        Arrays.fill(c, ' ');
        return c;
    }

    public int returnCode() {
        return returnCode;
    }

    public String wsReturnMsg() {
        return new String(wsReturnMsg);
    }

    public String wsInfStatus() {
        return wsInfStatus;
    }

    public String wsVarSqlcode() {
        return wsVarSqlcode;
    }

    public Sqlca sqlca() {
        return sqlca;
    }

    /**
     * Usage: {@code java -jar cobtupdt.jar <INPFILE>}. DB2 connection from CARDDEMO_JDBC_URL /
     * CARDDEMO_JDBC_USER / CARDDEMO_JDBC_PASSWORD; without a URL the step ends with RC 12.
     */
    public static void main(String[] args) {
        System.exit(execute(args, System.getenv(), System.out, System.err));
    }

    static int execute(String[] args, Map<String, String> env, PrintStream out, PrintStream err) {
        if (args.length < 1) {
            err.println("Usage: cobtupdt <INPFILE>");
            return RC_SEVERE;
        }
        TrRecordReader reader = FixedWidthTrRecordReader.of(Path.of(args[0]));
        String url = env.get(ENV_JDBC_URL);
        if (url == null || url.isBlank()) {
            err.println("COBTUPDT: " + ENV_JDBC_URL + " not set, no DB2 connection");
            return RC_SEVERE;
        }
        try (Connection c = DriverManager.getConnection(url, env.get(ENV_JDBC_USER), env.get(ENV_JDBC_PASSWORD))) {
            c.setAutoCommit(false);
            int rc = runStep(reader, new JdbcTransactionTypeGateway(c), out, err);
            if (rc == RC_SEVERE) {
                c.rollback();
            } else {
                c.commit();
            }
            return rc;
        } catch (SQLException e) {
            err.println("COBTUPDT: DB2 connection failed: SQLSTATE=" + e.getSQLState() + " errorCode=" + e.getErrorCode());
            return RC_SEVERE;
        }
    }

    private static int runStep(TrRecordReader reader, TransactionTypeGateway db, PrintStream out, PrintStream err) {
        try {
            return new Cobtupdt(reader, db, out::println).run();
        } catch (CobolFileStatusException e) {
            err.println("COBTUPDT: " + e.getMessage());
            return RC_SEVERE;
        }
    }
}
