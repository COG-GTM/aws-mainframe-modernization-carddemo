package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.CobolCodec;
import com.carddemo.programs.dbunldgs.copybook.ImsFunctionCodes;
import com.carddemo.programs.dbunldgs.copybook.PadflPcb;
import com.carddemo.programs.dbunldgs.copybook.PasflPcb;
import com.carddemo.programs.dbunldgs.copybook.PautbPcb;
import com.carddemo.programs.dbunldgs.copybook.PcbMask;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthDetails;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthSummary;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.time.Clock;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * DBUNLDGS: unloads the pending-authorization IMS database (DBPAUTP0, PSB DLIGSAMP) into two GSAM
 * datasets. Every PAUTSUM0 root with a numeric PA-ACCT-ID is inserted into PASFLDBD (100-byte records)
 * and each of its PAUTDTL1 children into PADFLDBD (200-byte records). Run by JCL UNLDGSAM
 * (DFSRRC00 PARM='DLI,DBUNLDGS,DLIGSAMP'); outputs are DD PASFILOP and PADFILOP.
 *
 * <p>Return code 0 on normal end, 16 when 9999-ABEND is performed.
 */
public final class Dbunldgs {

    public static final int RC_NORMAL = 0;
    public static final int RC_ABEND = 16;

    /** ROOT-UNQUAL-SSA: 'PAUTSUM0' + ' '. */
    public static final String ROOT_UNQUAL_SSA = "PAUTSUM0 ";
    /** CHILD-UNQUAL-SSA: 'PAUTDTL1' + ' '. */
    public static final String CHILD_UNQUAL_SSA = "PAUTDTL1 ";

    private static final DateTimeFormatter ACCEPT_DATE = DateTimeFormatter.ofPattern("yyMMdd");
    private static final long MOD_8_DIGITS = 100_000_000L;

    private final DliCallHandler pautDb;
    private final DliCallHandler pasflGsam;
    private final DliCallHandler padflGsam;
    private final Consumer<String> sysout;
    private final Clock clock;
    private final Charset charset;

    // LINKAGE SECTION: PCB masks
    private final PautbPcb pautbPcb = new PautbPcb();
    private final PasflPcb pasflPcb = new PasflPcb();
    private final PadflPcb padflPcb = new PadflPcb();

    // IMS segment I/O areas
    private final byte[] pendingAuthSummary = new byte[PendingAuthSummary.LENGTH];
    private final byte[] pendingAuthDetails = new byte[PendingAuthDetails.LENGTH];

    // WORKING-STORAGE: OPFIL1-REC PIC X(100); OPFIL2-REC = ROOT-SEG-KEY S9(11) COMP-3 + CHILD-SEG-REC X(200)
    private final byte[] opfil1Rec = new byte[100];
    private final byte[] rootSegKey = new byte[PendingAuthSummary.ACCT_ID_LENGTH];
    private final byte[] childSegRec = new byte[200];

    // WS-VARIABLES
    private String currentDate = "000000";   // PIC 9(06)
    private String currentYyddd = "00000";   // PIC 9(05)
    private long wsAuthSmryProcCnt;          // PIC 9(8)
    private long wsNoSumryRead;              // PIC S9(8) COMP
    private char wsEndOfAuthdbFlag = 'N';    // 88 END-OF-AUTHDB 'Y' / NOT-END-OF-AUTHDB 'N'
    private char wsMoreAuthsFlag = 'N';      // 88 MORE-AUTHS 'Y' / NO-MORE-AUTHS 'N'
    private char wsEndOfRootSeg = ' ';
    private char wsEndOfChildSeg = ' ';

    private int returnCode = RC_NORMAL;

    public Dbunldgs(DliCallHandler pautDb, DliCallHandler pasflGsam, DliCallHandler padflGsam,
                    Consumer<String> sysout, Clock clock, Charset charset) {
        this.pautDb = Objects.requireNonNull(pautDb);
        this.pasflGsam = Objects.requireNonNull(pasflGsam);
        this.padflGsam = Objects.requireNonNull(padflGsam);
        this.sysout = Objects.requireNonNull(sysout);
        this.clock = Objects.requireNonNull(clock);
        this.charset = Objects.requireNonNull(charset);
    }

    public Dbunldgs(DliCallHandler pautDb, DliCallHandler pasflGsam, DliCallHandler padflGsam) {
        this(pautDb, pasflGsam, padflGsam, System.out::println, Clock.systemDefaultZone(), CobolCodec.EBCDIC);
    }

    /** Runs the program and returns RETURN-CODE. */
    public int run() {
        try {
            mainPara();
        } catch (DbunldgsAbendException e) {
            // 9999-ABEND already set RETURN-CODE and GOBACK ended the run unit
        }
        return returnCode;
    }

    // MAIN-PARA
    void mainPara() {
        initialize1000();
        while (wsEndOfRootSeg != 'Y') {
            findNextAuthSummary2000();
        }
        fileClose4000();
    }

    // 1000-INITIALIZE
    void initialize1000() {
        LocalDate today = LocalDate.now(clock);
        currentDate = today.format(ACCEPT_DATE);
        currentYyddd = String.format("%02d%03d", today.getYear() % 100, today.getDayOfYear());
        display("STARTING PROGRAM DBUNLDGS::");
        display("*-------------------------------------*");
        display("TODAYS DATE            :" + currentDate);
        display(" ");
    }

    // 2000-FIND-NEXT-AUTH-SUMMARY
    void findNextAuthSummary2000() {
        pautbPcb.initializeStatus();
        cbltdli(ImsFunctionCodes.FUNC_GN, pautbPcb, pendingAuthSummary, ROOT_UNQUAL_SSA);
        String status = pautbPcb.getPcbStatus();
        if ("  ".equals(status)) {
            wsNoSumryRead = add1(wsNoSumryRead);
            wsAuthSmryProcCnt = add1(wsAuthSmryProcCnt);
            System.arraycopy(pendingAuthSummary, 0, opfil1Rec, 0, opfil1Rec.length);
            CobolCodec.encodePacked(BigDecimal.ZERO, rootSegKey, 0, 11, 0);
            Arrays.fill(childSegRec, charset.encode(" ").get());
            System.arraycopy(pendingAuthSummary, PendingAuthSummary.ACCT_ID_OFFSET, rootSegKey, 0, rootSegKey.length);
            if (PendingAuthSummary.isAcctIdNumeric(pendingAuthSummary)) {
                insertParentSegGsam3100();
                wsEndOfChildSeg = ' ';
                while (wsEndOfChildSeg != 'Y') {
                    findNextAuthDtl3000();
                }
            }
        }
        if ("GB".equals(status)) {
            wsEndOfAuthdbFlag = 'Y';
            wsEndOfRootSeg = 'Y';
        }
        if (!"  ".equals(status) && !"GB".equals(status)) {
            display("AUTH SUM  GN FAILED  :" + status);
            display("KEY FEEDBACK AREA    :" + pautbPcb.keyFbAsDisplay(charset));
            abend9999();
        }
    }

    // 3000-FIND-NEXT-AUTH-DTL
    void findNextAuthDtl3000() {
        cbltdli(ImsFunctionCodes.FUNC_GNP, pautbPcb, pendingAuthDetails, CHILD_UNQUAL_SSA);
        String status = pautbPcb.getPcbStatus();
        if ("  ".equals(status)) {
            wsMoreAuthsFlag = 'Y';
            wsNoSumryRead = add1(wsNoSumryRead);
            wsAuthSmryProcCnt = add1(wsAuthSmryProcCnt);
            System.arraycopy(pendingAuthDetails, 0, childSegRec, 0, childSegRec.length);
            insertChildSegGsam3200();
        }
        if ("GE".equals(status)) {
            wsEndOfChildSeg = 'Y';
            display("CHILD SEG FLAG GE : " + wsEndOfChildSeg);
        }
        if (!"  ".equals(status) && !"GE".equals(status)) {
            display("GNP CALL FAILED  :" + status);
            display("KFB AREA IN CHILD:" + pautbPcb.keyFbAsDisplay(charset));
            abend9999();
        }
        pautbPcb.initializeStatus();
    }

    // 3100-INSERT-PARENT-SEG-GSAM
    void insertParentSegGsam3100() {
        cbltdli(ImsFunctionCodes.FUNC_ISRT, pasflPcb, pendingAuthSummary, null);
        if (!pasflPcb.statusIsSpaces()) {
            display("GSAM PARENT FAIL :" + pasflPcb.getPcbStatus());
            display("KFB AREA IN GSAM:" + pasflPcb.keyFbAsDisplay(charset));
            abend9999();
        }
    }

    // 3200-INSERT-CHILD-SEG-GSAM (the COBOL reuses the 'GSAM PARENT FAIL' text here)
    void insertChildSegGsam3200() {
        cbltdli(ImsFunctionCodes.FUNC_ISRT, padflPcb, pendingAuthDetails, null);
        if (!padflPcb.statusIsSpaces()) {
            display("GSAM PARENT FAIL :" + padflPcb.getPcbStatus());
            display("KFB AREA IN GSAM:" + padflPcb.keyFbAsDisplay(charset));
            abend9999();
        }
    }

    // 4000-FILE-CLOSE
    void fileClose4000() {
        display("CLOSING THE FILE");
    }

    // 9999-ABEND
    void abend9999() {
        display("DBUNLDGS ABENDING ...");
        returnCode = RC_ABEND;
        throw new DbunldgsAbendException("DBUNLDGS ABENDING ...");
    }

    private void cbltdli(String function, PcbMask pcb, byte[] ioArea, String ssa) {
        DliCallHandler handler;
        if (pcb == pautbPcb) {
            handler = pautDb;
        } else if (pcb == pasflPcb) {
            handler = pasflGsam;
        } else {
            handler = padflGsam;
        }
        handler.cbltdli(function, pcb, ioArea, ssa);
    }

    private static long add1(long counter) {
        return (counter + 1) % MOD_8_DIGITS;
    }

    private void display(String line) {
        sysout.accept(line);
    }

    public int getReturnCode() { return returnCode; }
    public String getCurrentDate() { return currentDate; }
    public String getCurrentYyddd() { return currentYyddd; }
    public long getWsAuthSmryProcCnt() { return wsAuthSmryProcCnt; }
    public long getWsNoSumryRead() { return wsNoSumryRead; }
    public boolean isEndOfAuthdb() { return wsEndOfAuthdbFlag == 'Y'; }
    public boolean isMoreAuths() { return wsMoreAuthsFlag == 'Y'; }
    public byte[] getOpfil1Rec() { return opfil1Rec.clone(); }
    public byte[] getRootSegKey() { return rootSegKey.clone(); }
    public byte[] getChildSegRec() { return childSegRec.clone(); }
    public PautbPcb getPautbPcb() { return pautbPcb; }
    public PasflPcb getPasflPcb() { return pasflPcb; }
    public PadflPcb getPadflPcb() { return padflPcb; }
}
