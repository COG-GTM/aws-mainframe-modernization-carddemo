package com.carddemo.programs.paudbunl;

import com.carddemo.programs.paudbunl.copybook.ImsFunctionCodes;
import com.carddemo.programs.paudbunl.copybook.PautbPcb;
import com.carddemo.programs.paudbunl.copybook.PendingAuthDetails;
import com.carddemo.programs.paudbunl.copybook.PendingAuthSummary;
import com.carddemo.programs.paudbunl.ims.DliGateway;
import com.carddemo.programs.paudbunl.ims.ImsStatus;
import com.carddemo.programs.paudbunl.ims.ImsUnloadFile;
import com.carddemo.programs.paudbunl.ims.Ssa;
import com.carddemo.programs.paudbunl.io.FileStatus;
import com.carddemo.programs.paudbunl.io.FixedWidthFileRecordWriter;
import com.carddemo.programs.paudbunl.io.RecordWriter;
import com.carddemo.programs.paudbunl.support.CobolData;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.time.Clock;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * PAUDBUNL - unloads the pending authorization IMS database (DBPAUTP0 via PSB PAUTBUNL) to two
 * sequential files (JCL UNLDPADB):
 * <ul>
 *   <li>OUTFIL1 (LRECL 100): one PAUTSUM0 root segment per record, written only when
 *       PA-ACCT-ID IS NUMERIC.</li>
 *   <li>OUTFIL2 (LRECL 206): ROOT-SEG-KEY (parent PA-ACCT-ID, COMP-3) + PAUTDTL1 child segment.</li>
 * </ul>
 * RETURN-CODE is 0 on normal completion and 16 when 9999-ABEND is performed.
 */
public final class Paudbunl {

    public static final int ABEND_RETURN_CODE = 16;
    /** WS-PGMNAME. */
    static final String WS_PGMNAME = "IMSUNLOD";
    private static final int COUNTER_DIGITS = 8;
    private static final String Y = "Y";

    private final DliGateway dli;
    private final PautbPcb pautbPcb;
    private final RecordWriter opfile1;
    private final RecordWriter opfile2;
    private final Clock clock;
    private final Consumer<String> sysout;
    private final Charset charset;

    // WORKING-STORAGE
    private String currentDate;
    private String currentYyddd;
    private long wsAuthSmryProcCnt;
    private long wsNoSumryRead;
    private String wsEndOfAuthdbFlag = "N";
    private String wsMoreAuthsFlag = "N";
    private String wsEndOfRootSeg = " ";
    private String wsEndOfChildSeg = " ";
    private String wsOutfl1Status = "  ";
    private String wsOutfl2Status = "  ";
    private final byte[] pendingAuthSummary = new byte[PendingAuthSummary.LENGTH];
    private final byte[] pendingAuthDetails = new byte[PendingAuthDetails.LENGTH];
    // FILE SECTION
    private final byte[] opfil1Rec = new byte[PendingAuthSummary.LENGTH];
    private final Opfil2Rec opfil2Rec;

    private boolean started;
    private int rootRecordsWritten;
    private int childRecordsWritten;

    public Paudbunl(DliGateway dli, PautbPcb pautbPcb, RecordWriter opfile1, RecordWriter opfile2,
                    Clock clock, Consumer<String> sysout, Charset charset) {
        this.dli = Objects.requireNonNull(dli);
        this.pautbPcb = Objects.requireNonNull(pautbPcb);
        this.opfile1 = Objects.requireNonNull(opfile1);
        this.opfile2 = Objects.requireNonNull(opfile2);
        this.clock = Objects.requireNonNull(clock);
        this.sysout = Objects.requireNonNull(sysout);
        this.charset = Objects.requireNonNull(charset);
        this.opfil2Rec = new Opfil2Rec(charset);
    }

    /** PCB passed by IMS at ENTRY 'DLITCBL' for PSB PAUTBUNL. */
    public static PautbPcb pautbunlPcb() {
        return new PautbPcb("DBPAUTP0", "GOTP", 2);
    }

    /** Runs the program once (one run unit) and returns RETURN-CODE plus counters. */
    public PaudbunlResult run() {
        if (started) {
            throw new IllegalStateException("PAUDBUNL run unit already executed; create a new instance");
        }
        started = true;
        int returnCode = 0;
        try {
            mainPara();
        } catch (AbendException e) {
            returnCode = e.returnCode();
            closeOpenFilesAtRunUnitEnd();
        }
        return new PaudbunlResult(returnCode, wsNoSumryRead, wsAuthSmryProcCnt, Y.equals(wsEndOfAuthdbFlag),
                Y.equals(wsMoreAuthsFlag), currentDate, currentYyddd, rootRecordsWritten, childRecordsWritten);
    }

    // MAIN-PARA
    private void mainPara() {
        initialize1000();
        while (!Y.equals(wsEndOfRootSeg)) {
            findNextAuthSummary2000();
        }
        fileClose4000();
    }

    // 1000-INITIALIZE
    void initialize1000() {
        LocalDate today = LocalDate.now(clock);
        currentDate = today.format(DateTimeFormatter.ofPattern("yyMMdd"));
        currentYyddd = today.format(DateTimeFormatter.ofPattern("yy")) + String.format(Locale.ROOT, "%03d", today.getDayOfYear());

        display("STARTING PROGRAM PAUDBUNL::");
        display("*-------------------------------------*");
        display("TODAYS DATE            :" + currentDate);
        display(" ");

        wsOutfl1Status = opfile1.open();
        if (!FileStatus.isOk(wsOutfl1Status)) {
            display("ERROR IN OPENING OPFILE1:" + wsOutfl1Status);
            abend9999();
        }

        wsOutfl2Status = opfile2.open();
        if (!FileStatus.isOk(wsOutfl2Status)) {
            display("ERROR IN OPENING OPFILE2:" + wsOutfl2Status);
            abend9999();
        }
    }

    // 2000-FIND-NEXT-AUTH-SUMMARY
    void findNextAuthSummary2000() {
        pautbPcb.initializeStatus();
        dli.cbltdli(ImsFunctionCodes.GN, pautbPcb, pendingAuthSummary, Ssa.ROOT_UNQUAL_SSA);

        if (pautbPcb.isStatusSpaces()) {
            wsNoSumryRead = CobolData.addTruncating(wsNoSumryRead, 1, COUNTER_DIGITS);
            wsAuthSmryProcCnt = CobolData.addTruncating(wsAuthSmryProcCnt, 1, COUNTER_DIGITS);
            System.arraycopy(pendingAuthSummary, 0, opfil1Rec, 0, opfil1Rec.length);
            opfil2Rec.initializeRootSegKey();
            opfil2Rec.initializeChildSegRec();
            opfil2Rec.moveRootSegKey(pendingAuthSummary, PendingAuthSummary.ACCT_ID_OFFSET);
            if (PendingAuthSummary.isAcctIdNumeric(pendingAuthSummary)) {
                wsOutfl1Status = opfile1.write(opfil1Rec.clone());
                if (FileStatus.isOk(wsOutfl1Status)) {
                    rootRecordsWritten++;
                }
                wsEndOfChildSeg = " ";
                while (!Y.equals(wsEndOfChildSeg)) {
                    findNextAuthDtl3000();
                }
            }
        }
        if (ImsStatus.END_OF_DB.equals(pautbPcb.pcbStatus())) {
            wsEndOfAuthdbFlag = Y;
            wsEndOfRootSeg = Y;
        }
        if (!pautbPcb.isStatusSpaces() && !ImsStatus.END_OF_DB.equals(pautbPcb.pcbStatus())) {
            display("AUTH SUM  GN FAILED  :" + pautbPcb.pcbStatus());
            display("KEY FEEDBACK AREA    :" + pautbPcb.keyfbDisplay(charset));
            abend9999();
        }
    }

    // 3000-FIND-NEXT-AUTH-DTL
    void findNextAuthDtl3000() {
        dli.cbltdli(ImsFunctionCodes.GNP, pautbPcb, pendingAuthDetails, Ssa.CHILD_UNQUAL_SSA);

        if (pautbPcb.isStatusSpaces()) {
            wsMoreAuthsFlag = Y;
            wsNoSumryRead = CobolData.addTruncating(wsNoSumryRead, 1, COUNTER_DIGITS);
            wsAuthSmryProcCnt = CobolData.addTruncating(wsAuthSmryProcCnt, 1, COUNTER_DIGITS);
            opfil2Rec.moveChildSegRec(pendingAuthDetails);
            wsOutfl2Status = opfile2.write(opfil2Rec.toBytes());
            if (FileStatus.isOk(wsOutfl2Status)) {
                childRecordsWritten++;
            }
        }
        if (ImsStatus.SEGMENT_NOT_FOUND.equals(pautbPcb.pcbStatus())) {
            wsEndOfChildSeg = Y;
            display("CHILD SEG FLAG GE : " + wsEndOfChildSeg);
        }
        if (!pautbPcb.isStatusSpaces() && !ImsStatus.SEGMENT_NOT_FOUND.equals(pautbPcb.pcbStatus())) {
            display("GNP CALL FAILED  :" + pautbPcb.pcbStatus());
            display("KFB AREA IN CHILD:" + pautbPcb.keyfbDisplay(charset));
            abend9999();
        }
        pautbPcb.initializeStatus();
    }

    // 4000-FILE-CLOSE
    void fileClose4000() {
        display("CLOSING THE FILE");
        wsOutfl1Status = opfile1.close();
        if (!FileStatus.isOk(wsOutfl1Status)) {
            display("ERROR IN CLOSING 1ST FILE:" + wsOutfl1Status);
        }
        wsOutfl2Status = opfile2.close();
        if (!FileStatus.isOk(wsOutfl2Status)) {
            display("ERROR IN CLOSING 2ND FILE:" + wsOutfl2Status);
        }
    }

    // 9999-ABEND
    private void abend9999() {
        display(WS_PGMNAME + " ABENDING ...");
        throw new AbendException(ABEND_RETURN_CODE);
    }

    /** GOBACK from the main program ends the run unit, which closes any file still open. */
    private void closeOpenFilesAtRunUnitEnd() {
        if (opfile1.isOpen()) {
            opfile1.close();
        }
        if (opfile2.isOpen()) {
            opfile2.close();
        }
    }

    private void display(String line) {
        sysout.accept(line);
    }

    /** MOVE 16 TO RETURN-CODE + GOBACK inside 9999-ABEND. */
    static final class AbendException extends RuntimeException {
        private final int returnCode;

        AbendException(int returnCode) {
            super("PAUDBUNL abend, RETURN-CODE " + returnCode, null, false, false);
            this.returnCode = returnCode;
        }

        int returnCode() {
            return returnCode;
        }
    }

    /**
     * Batch entry point mirroring JCL UNLDPADB STEP01:
     * {@code java -jar paudbunl.jar <DBPAUTP0 unload file> <OUTFIL1> <OUTFIL2>}.
     * The IMS database is supplied as an HD unload dataset; the process exit code is RETURN-CODE.
     */
    public static void main(String[] args) throws IOException {
        if (args.length != 3) {
            System.err.println("Usage: Paudbunl <dbpautp0-unload-file> <outfil1> <outfil2>");
            System.exit(12);
        }
        PrintStream out = System.out;
        Paudbunl program = new Paudbunl(
                ImsUnloadFile.load(Path.of(args[0])),
                pautbunlPcb(),
                new FixedWidthFileRecordWriter(Path.of(args[1]), PendingAuthSummary.LENGTH),
                new FixedWidthFileRecordWriter(Path.of(args[2]), Opfil2Rec.LENGTH),
                Clock.systemDefaultZone(),
                out::println,
                CobolData.EBCDIC);
        System.exit(program.run().returnCode());
    }
}
