package com.carddemo.programs.paudblod;

import com.carddemo.programs.paudblod.copybook.CobolFields;
import com.carddemo.programs.paudblod.copybook.ImsFuncs;
import com.carddemo.programs.paudblod.copybook.PautbPcb;
import com.carddemo.programs.paudblod.copybook.PendingAuthDetails;
import com.carddemo.programs.paudblod.copybook.PendingAuthSummary;
import com.carddemo.programs.paudblod.ims.DliGateway;
import com.carddemo.programs.paudblod.ims.InMemoryPautDatabase;
import com.carddemo.programs.paudblod.ims.Ssa;
import com.carddemo.programs.paudblod.io.FileStatus;
import com.carddemo.programs.paudblod.io.FixedWidthFileReader;
import com.carddemo.programs.paudblod.io.ReadResult;
import com.carddemo.programs.paudblod.io.SequentialRecordReader;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.time.Clock;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.function.Consumer;

/**
 * PAUDBLOD: IMS BMP that loads the pending-authorization database DBPAUTP0 from the two
 * sequential unload files written by PAUDBUNL (JCL LOADPADB, PSB PSBPAUTB).
 * <ul>
 *   <li>INFILE1 (LRECL 100): PAUTSUM0 root segments, inserted with ISRT + ROOT-UNQUAL-SSA.</li>
 *   <li>INFILE2 (LRECL 206): ROOT-SEG-KEY S9(11) COMP-3 + PAUTDTL1 child segment; the parent is
 *       located with GU + ROOT-QUAL-SSA, then the child is inserted with ISRT + CHILD-UNQUAL-SSA.</li>
 * </ul>
 * {@link #run()} returns the program RETURN-CODE: 0 normally, 16 after 9999-ABEND.
 */
public final class Paudblod {

    /** Default record encoding: the unload files are EBCDIC (CCSID 037). */
    public static final Charset EBCDIC = Charset.forName("IBM037");

    /** FD INFILE1 / 01 INFIL1-REC PIC X(100). */
    public static final int INFIL1_REC_LENGTH = 100;
    /** FD INFILE2 / 01 INFIL2-REC: ROOT-SEG-KEY S9(11) COMP-3 (6) + CHILD-SEG-REC X(200). */
    public static final int ROOT_SEG_KEY_LENGTH = 6;
    public static final int INFIL2_REC_LENGTH = ROOT_SEG_KEY_LENGTH + 200;

    private static final DateTimeFormatter ACCEPT_DATE = DateTimeFormatter.ofPattern("yyMMdd");

    private final SequentialRecordReader infile1;
    private final SequentialRecordReader infile2;
    private final DliGateway dli;
    private final PautbPcb pautbPcb;
    private final Clock clock;
    private final Charset charset;
    private final Consumer<String> display;

    // WORKING-STORAGE
    private String currentDate;
    private String currentYyddd;
    private String wsInfil1Status = "  ";
    private String wsInfil2Status = "  ";
    private String endRootSegFile = " ";
    private String endChildSegFile = " ";
    private final byte[] pendingAuthSummary = new byte[PendingAuthSummary.LENGTH];
    private final byte[] pendingAuthDetails = new byte[PendingAuthDetails.LENGTH];
    private BigDecimal qualSsaKeyValue = BigDecimal.ZERO;

    public Paudblod(SequentialRecordReader infile1, SequentialRecordReader infile2, DliGateway dli,
                    PautbPcb pautbPcb, Clock clock, Charset charset, Consumer<String> display) {
        this.infile1 = infile1;
        this.infile2 = infile2;
        this.dli = dli;
        this.pautbPcb = pautbPcb;
        this.clock = clock;
        this.charset = charset;
        this.display = display;
    }

    // MAIN-PARA
    public int run() {
        try {
            display.accept("STARTING PAUDBLOD");
            initialize();
            while (!"Y".equals(endRootSegFile)) {
                readRootSegFile();
            }
            while (!"Y".equals(endChildSegFile)) {
                readChildSegFile();
            }
            fileClose();
            return 0;
        } catch (AbendException e) {
            return e.returnCode();
        }
    }

    // 1000-INITIALIZE
    void initialize() {
        LocalDate today = LocalDate.now(clock);
        currentDate = today.format(ACCEPT_DATE);
        currentYyddd = String.format("%02d%03d", today.getYear() % 100, today.getDayOfYear());

        display.accept("*-------------------------------------*");
        display.accept("TODAYS DATE            :" + currentDate);
        display.accept(" ");

        wsInfil1Status = infile1.open();
        if (!FileStatus.isOk(wsInfil1Status)) {
            display.accept("ERROR IN OPENING INFILE1:" + wsInfil1Status);
            abend();
        }
        wsInfil2Status = infile2.open();
        if (!FileStatus.isOk(wsInfil2Status)) {
            display.accept("ERROR IN OPENING INFILE2:" + wsInfil2Status);
            abend();
        }
    }

    // 2000-READ-ROOT-SEG-FILE
    void readRootSegFile() {
        ReadResult r = infile1.read();
        wsInfil1Status = r.status();
        if (FileStatus.isOk(wsInfil1Status)) {
            moveGroup(r.record(), 0, INFIL1_REC_LENGTH, pendingAuthSummary);
            insertRootSeg();
        } else if (FileStatus.END_OF_FILE.equals(wsInfil1Status)) {
            endRootSegFile = "Y";
        } else {
            display.accept("ERROR READING ROOT SEG INFILE");
        }
    }

    // 2100-INSERT-ROOT-SEG
    void insertRootSeg() {
        dli.cbltdli(ImsFuncs.FUNC_ISRT, pautbPcb, pendingAuthSummary, new Ssa.RootUnqualSsa());
        display.accept(" *******************************");
        display.accept(" *******************************");
        String status = pautbPcb.getPautPcbStatus();
        if (status.isBlank()) {
            display.accept("ROOT INSERT SUCCESS    ");
        }
        if ("II".equals(status)) {
            display.accept("ROOT SEGMENT ALREADY IN DB");
        }
        if (!status.isBlank() && !"II".equals(status)) {
            display.accept("ROOT INSERT FAILED  :" + status);
            abend();
        }
    }

    // 3000-READ-CHILD-SEG-FILE
    void readChildSegFile() {
        ReadResult r = infile2.read();
        wsInfil2Status = r.status();
        if (FileStatus.isOk(wsInfil2Status)) {
            byte[] rec = r.record();
            if (CobolFields.isPackedNumeric(rec, 0, ROOT_SEG_KEY_LENGTH)) {
                qualSsaKeyValue = CobolFields.decodePacked(rec, 0, ROOT_SEG_KEY_LENGTH, 0);
                moveGroup(rec, ROOT_SEG_KEY_LENGTH, PendingAuthDetails.LENGTH, pendingAuthDetails);
                insertChildSeg();
            }
        } else if (FileStatus.END_OF_FILE.equals(wsInfil2Status)) {
            endChildSegFile = "Y";
        } else {
            display.accept("ERROR READING CHILD SEG INFILE");
        }
    }

    // 3100-INSERT-CHILD-SEG
    void insertChildSeg() {
        pautbPcb.setPautPcbStatus("  ");
        dli.cbltdli(ImsFuncs.FUNC_GU, pautbPcb, pendingAuthSummary, new Ssa.RootQualSsa(qualSsaKeyValue));
        display.accept("***************************");
        display.accept("***************************");
        // The COBOL has no END-IF after PERFORM 3200, so the failure test is nested inside the
        // success branch: a failed GU (e.g. 'GE') skips the child silently.
        if (pautbPcb.isStatusSpaces()) {
            display.accept("GU CALL TO ROOT SEG SUCCESS");
            insertImsCall();
            String status = pautbPcb.getPautPcbStatus();
            if (!status.isBlank() && !"II".equals(status)) {
                display.accept("ROOT GU CALL FAIL:" + status);
                display.accept("KFB AREA IN CHILD:" + keyfb());
                abend();
            }
        }
    }

    // 3200-INSERT-IMS-CALL
    void insertImsCall() {
        dli.cbltdli(ImsFuncs.FUNC_ISRT, pautbPcb, pendingAuthDetails, new Ssa.ChildUnqualSsa());
        String status = pautbPcb.getPautPcbStatus();
        if (status.isBlank()) {
            display.accept("CHILD SEGMENT INSERTED SUCCESS");
        }
        if ("II".equals(status)) {
            display.accept("CHILD SEGMENT ALREADY IN DB");
        }
        if (!status.isBlank() && !"II".equals(status)) {
            display.accept("INSERT CALL FAIL FOR CHILD:" + status);
            display.accept("KFB AREA IN CHILD:" + keyfb());
            abend();
        }
    }

    // 4000-FILE-CLOSE
    void fileClose() {
        display.accept("CLOSING THE FILE");
        wsInfil1Status = infile1.close();
        if (!FileStatus.isOk(wsInfil1Status)) {
            display.accept("ERROR IN CLOSING 1ST FILE:" + wsInfil1Status);
        }
        wsInfil2Status = infile2.close();
        if (!FileStatus.isOk(wsInfil2Status)) {
            display.accept("ERROR IN CLOSING 2ND FILE:" + wsInfil2Status);
        }
    }

    // 9999-ABEND
    private void abend() {
        display.accept("IMS LOAD ABENDING ...");
        throw new AbendException();
    }

    private String keyfb() {
        return CobolFields.decodeText(pautbPcb.getPautKeyfb(), 0, PautbPcb.KEYFB_LENGTH, charset);
    }

    /** Alphanumeric group MOVE: copy then space-fill any remainder of the receiver. */
    private void moveGroup(byte[] src, int off, int len, byte[] dest) {
        int n = Math.min(len, dest.length);
        System.arraycopy(src, off, dest, 0, n);
        byte space = " ".getBytes(charset)[0];
        for (int i = n; i < dest.length; i++) {
            dest[i] = space;
        }
    }

    String currentDate() {
        return currentDate;
    }

    String currentYyddd() {
        return currentYyddd;
    }

    BigDecimal qualSsaKeyValue() {
        return qualSsaKeyValue;
    }

    /**
     * Runs PAUDBLOD against an in-memory DBPAUTP0 and prints the load totals.
     * Usage: {@code Paudblod <INFILE1> <INFILE2> [charset]}.
     */
    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("usage: Paudblod <INFILE1> <INFILE2> [charset]");
            System.exit(8);
        }
        Charset cs = args.length > 2 ? Charset.forName(args[2]) : EBCDIC;
        InMemoryPautDatabase db = new InMemoryPautDatabase(" ".getBytes(cs)[0]);
        Paudblod pgm = new Paudblod(
                new FixedWidthFileReader(Path.of(args[0]), INFIL1_REC_LENGTH),
                new FixedWidthFileReader(Path.of(args[1]), INFIL2_REC_LENGTH),
                db, new PautbPcb("DBPAUTP0", "A", 2), Clock.systemDefaultZone(), cs, System.out::println);
        int rc = pgm.run();
        System.out.println("PAUTSUM0 LOADED: " + db.rootCount() + "  PAUTDTL1 LOADED: " + db.childCount());
        System.exit(rc);
    }
}
