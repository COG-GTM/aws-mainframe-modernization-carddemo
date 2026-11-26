package com.carddemo.programs.cbpaup0c;

import static com.carddemo.programs.cbpaup0c.CobolSupport.compareNumericToAlphanumeric;
import static com.carddemo.programs.cbpaup0c.CobolSupport.displaySigned;
import static com.carddemo.programs.cbpaup0c.CobolSupport.displayUnsigned;
import static com.carddemo.programs.cbpaup0c.CobolSupport.truncateSigned;
import static com.carddemo.programs.cbpaup0c.CobolSupport.truncateUnsigned;

import com.carddemo.programs.cbpaup0c.copybook.CobolCodec;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthDetails;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthSummary;
import com.carddemo.programs.cbpaup0c.ims.DliResult;
import com.carddemo.programs.cbpaup0c.ims.HdUnloadFile;
import com.carddemo.programs.cbpaup0c.ims.PautDbGateway;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.channels.Channels;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermissions;
import java.time.Clock;
import java.time.LocalDate;
import java.util.EnumSet;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;

/**
 * CBPAUP0C - CardDemo Authorization Module, batch IMS BMP: delete expired pending authorization messages.
 *
 * <p>Walks every PAUTSUM0 root of DBPAUTP0, deletes each PAUTDTL1 child whose age in days (YYDDD arithmetic)
 * is at least the SYSIN expiry, deletes the root when its approved count drops to zero or below, and takes
 * DL/I checkpoints. Returns RETURN-CODE: 0 normally, 16 after 9999-ABEND.
 */
public final class Cbpaup0c {

    public static final String PROGRAM_NAME = "CBPAUP0C";

    private final PautDbGateway db;
    private final String sysinRecord;
    private final Clock clock;
    private final Consumer<String> display;

    // WS-VARIABLES
    private String currentDate;          // CURRENT-DATE          PIC 9(06)
    private long currentYyddd;           // CURRENT-YYDDD         PIC 9(05)
    private long wsAuthDate;             // WS-AUTH-DATE          PIC 9(05)
    private long wsExpiryDays;           // WS-EXPIRY-DAYS        PIC S9(4) COMP
    private long wsDayDiff;              // WS-DAY-DIFF           PIC S9(4) COMP
    private long wsCurrAppId;            // WS-CURR-APP-ID        PIC 9(11)
    private long wsNoChkp;               // WS-NO-CHKP            PIC 9(8)
    private long wsAuthSmryProcCnt;      // WS-AUTH-SMRY-PROC-CNT PIC 9(8)
    private long wsNoSumryRead;          // WS-NO-SUMRY-READ      PIC S9(8) COMP
    private long wsNoSumryDeleted;       // WS-NO-SUMRY-DELETED   PIC S9(8) COMP
    private long wsNoDtlRead;            // WS-NO-DTL-READ        PIC S9(8) COMP
    private long wsNoDtlDeleted;         // WS-NO-DTL-DELETED     PIC S9(8) COMP
    private boolean errFlgOn;            // WS-ERR-FLG            88 ERR-FLG-ON (never set by the program)
    private boolean endOfAuthdb;         // WS-END-OF-AUTHDB-FLAG 88 END-OF-AUTHDB
    private boolean moreAuths;           // WS-MORE-AUTHS-FLAG    88 MORE-AUTHS
    private boolean qualifiedForDelete;  // WS-QUALIFY-DELETE-FLAG 88 QUALIFIED-FOR-DELETE
    private final String wkChkptId = "RMAD" + "0000"; // WK-CHKPT-ID: 'RMAD' + WK-CHKPT-ID-CTR (never incremented)

    private PrmInfo prmInfo;
    private PendingAuthSummary pendingAuthSummary = new PendingAuthSummary();
    private PendingAuthDetails pendingAuthDetails;

    /**
     * @param db          DL/I gateway for PCB PAUTBPCB of PSB PSBPAUTB
     * @param sysinRecord the first SYSIN record (control card), or null when SYSIN is empty
     * @param clock       source for ACCEPT ... FROM DATE / FROM DAY
     * @param display     receives each DISPLAY line (SYSOUT)
     */
    public Cbpaup0c(PautDbGateway db, String sysinRecord, Clock clock, Consumer<String> display) {
        this.db = db;
        this.sysinRecord = sysinRecord;
        this.clock = clock;
        this.display = display;
    }

    /** Runs the program and returns RETURN-CODE. */
    public int run() {
        try {
            mainPara();
            return 0;
        } catch (AbendException e) {
            return e.returnCode();
        }
    }

    // MAIN-PARA
    private void mainPara() {
        initialize1000();
        findNextAuthSummary2000();

        while (!(errFlgOn || endOfAuthdb)) {
            findNextAuthDtl3000();

            while (moreAuths) {
                checkIfExpired4000();
                if (qualifiedForDelete) {
                    deleteAuthDtl5000();
                }
                findNextAuthDtl3000();
            }

            // COBOL: IF PA-APPROVED-AUTH-CNT <= 0 AND PA-APPROVED-AUTH-CNT <= 0 (declined count never tested)
            if (pendingAuthSummary.approvedAuthCount() <= 0 && pendingAuthSummary.approvedAuthCount() <= 0) {
                deleteAuthSummary6000();
            }

            if (compareNumericToAlphanumeric(wsAuthSmryProcCnt, 8, prmInfo.chkpFreq()) > 0) {
                takeCheckpoint9000();
                wsAuthSmryProcCnt = 0;
            }
            findNextAuthSummary2000();
        }

        takeCheckpoint9000();

        display.accept(" ");
        display.accept("*-------------------------------------*");
        display.accept("# TOTAL SUMMARY READ  :" + displaySigned(wsNoSumryRead, 8));
        display.accept("# SUMMARY REC DELETED :" + displaySigned(wsNoSumryDeleted, 8));
        display.accept("# TOTAL DETAILS READ  :" + displaySigned(wsNoDtlRead, 8));
        display.accept("# DETAILS REC DELETED :" + displaySigned(wsNoDtlDeleted, 8));
        display.accept("*-------------------------------------*");
        display.accept(" ");
    }

    // 1000-INITIALIZE
    private void initialize1000() {
        LocalDate today = LocalDate.now(clock);
        int yy = today.getYear() % 100;
        currentDate = String.format("%02d%02d%02d", yy, today.getMonthValue(), today.getDayOfMonth());
        currentYyddd = yy * 1000L + today.getDayOfYear();

        prmInfo = PrmInfo.fromSysin(sysinRecord);
        display.accept("STARTING PROGRAM CBPAUP0C::");
        display.accept("*-------------------------------------*");
        display.accept("CBPAUP0C PARM RECEIVED :" + prmInfo);
        display.accept("TODAYS DATE            :" + displayUnsigned(currentYyddd, 5));
        display.accept(" ");

        if (CobolSupport.isNumeric(prmInfo.expiryDays())) {
            wsExpiryDays = Long.parseLong(prmInfo.expiryDays());
        } else {
            wsExpiryDays = 5;
        }
        if (isSpacesZeroOrLowValues(prmInfo.chkpFreq())) {
            prmInfo.chkpFreq("5");
        }
        if (isSpacesZeroOrLowValues(prmInfo.chkpDisFreq())) {
            prmInfo.chkpDisFreq("10");
        }
        if (!"Y".equals(prmInfo.debugFlag())) {
            prmInfo.debugFlag("N");
        }
    }

    /** {@code X(5) = SPACES OR 0 OR LOW-VALUES}: the numeric literal 0 compares as alphanumeric '0    '. */
    private static boolean isSpacesZeroOrLowValues(String field) {
        return field.equals(" ".repeat(field.length()))
                || CobolSupport.compareAlphanumeric(field, "0") == 0
                || field.equals("\u0000".repeat(field.length()));
    }

    // 2000-FIND-NEXT-AUTH-SUMMARY
    private void findNextAuthSummary2000() {
        if (prmInfo.isDebugOn()) {
            display.accept("DEBUG: AUTH SMRY READ : " + displaySigned(wsNoSumryRead, 8));
        }

        DliResult<PendingAuthSummary> result = db.getNextSummary();
        String dibstat = result.status();
        if (result.segment() != null) {
            pendingAuthSummary = result.segment();
        }

        if (ImsReturnCode.OK.equals(dibstat)) {
            endOfAuthdb = false;
            wsNoSumryRead = truncateSigned(wsNoSumryRead + 1, 8);
            wsAuthSmryProcCnt = truncateUnsigned(wsAuthSmryProcCnt + 1, 8);
            wsCurrAppId = truncateUnsigned(pendingAuthSummary.accountId(), 11);
        } else if ("GB".equals(dibstat)) {
            endOfAuthdb = true;
        } else {
            display.accept("AUTH SUMMARY READ FAILED  :" + dibstat);
            display.accept("SUMMARY READ BEFORE ABEND :" + displaySigned(wsNoSumryRead, 8));
            abend9999("GN PAUTSUM0 status " + dibstat);
        }
    }

    // 3000-FIND-NEXT-AUTH-DTL
    private void findNextAuthDtl3000() {
        if (prmInfo.isDebugOn()) {
            display.accept("DEBUG: AUTH DTL READ : " + displaySigned(wsNoDtlRead, 8));
        }

        DliResult<PendingAuthDetails> result = db.getNextDetailWithinParent();
        String dibstat = result.status();
        if (result.segment() != null) {
            pendingAuthDetails = result.segment();
        }

        if (ImsReturnCode.OK.equals(dibstat)) {
            moreAuths = true;
            wsNoDtlRead = truncateSigned(wsNoDtlRead + 1, 8);
        } else if ("GE".equals(dibstat) || "GB".equals(dibstat)) {
            moreAuths = false;
        } else {
            display.accept("AUTH DETAIL READ FAILED  :" + dibstat);
            display.accept("SUMMARY AUTH APP ID      :" + displaySigned(pendingAuthSummary.accountId(), 11));
            display.accept("DETAIL READ BEFORE ABEND :" + displaySigned(wsNoDtlRead, 8));
            abend9999("GNP PAUTDTL1 status " + dibstat);
        }
    }

    // 4000-CHECK-IF-EXPIRED
    private void checkIfExpired4000() {
        wsAuthDate = truncateUnsigned(99999L - pendingAuthDetails.authDate9c(), 5);
        wsDayDiff = truncateSigned(currentYyddd - wsAuthDate, 4);

        if (wsDayDiff >= wsExpiryDays) {
            qualifiedForDelete = true;
            PendingAuthSummary s = pendingAuthSummary;
            if ("00".equals(pendingAuthDetails.authRespCode())) {
                s.approvedAuthCount((int) truncateSigned(s.approvedAuthCount() - 1L, 4));
                s.approvedAuthAmount(CobolSupport.truncateSigned(
                        s.approvedAuthAmount().subtract(pendingAuthDetails.approvedAmt()), 9, 2));
            } else {
                s.declinedAuthCount((int) truncateSigned(s.declinedAuthCount() - 1L, 4));
                s.declinedAuthAmount(CobolSupport.truncateSigned(
                        s.declinedAuthAmount().subtract(pendingAuthDetails.transactionAmt()), 9, 2));
            }
        } else {
            qualifiedForDelete = false;
        }
    }

    // 5000-DELETE-AUTH-DTL
    private void deleteAuthDtl5000() {
        if (prmInfo.isDebugOn()) {
            display.accept("DEBUG: AUTH DTL DLET : " + displaySigned(pendingAuthSummary.accountId(), 11));
        }

        String dibstat = db.deleteDetail(pendingAuthDetails);
        if (ImsReturnCode.OK.equals(dibstat)) {
            wsNoDtlDeleted = truncateSigned(wsNoDtlDeleted + 1, 8);
        } else {
            display.accept("AUTH DETAIL DELETE FAILED :" + dibstat);
            display.accept("AUTH APP ID               :" + displaySigned(pendingAuthSummary.accountId(), 11));
            abend9999("DLET PAUTDTL1 status " + dibstat);
        }
    }

    // 6000-DELETE-AUTH-SUMMARY
    private void deleteAuthSummary6000() {
        if (prmInfo.isDebugOn()) {
            display.accept("DEBUG: AUTH SMRY DLET : " + displaySigned(pendingAuthSummary.accountId(), 11));
        }

        String dibstat = db.deleteSummary(pendingAuthSummary);
        if (ImsReturnCode.OK.equals(dibstat)) {
            wsNoSumryDeleted = truncateSigned(wsNoSumryDeleted + 1, 8);
        } else {
            display.accept("AUTH SUMMARY DELETE FAILED :" + dibstat);
            display.accept("AUTH APP ID                :" + displaySigned(pendingAuthSummary.accountId(), 11));
            abend9999("DLET PAUTSUM0 status " + dibstat);
        }
    }

    // 9000-TAKE-CHECKPOINT
    private void takeCheckpoint9000() {
        String dibstat = db.checkpoint(wkChkptId);
        if (ImsReturnCode.OK.equals(dibstat)) {
            wsNoChkp = truncateUnsigned(wsNoChkp + 1, 8);
            if (compareNumericToAlphanumeric(wsNoChkp, 8, prmInfo.chkpDisFreq()) >= 0) {
                wsNoChkp = 0;
                display.accept("CHKP SUCCESS: AUTH COUNT - " + displaySigned(wsNoSumryRead, 8)
                        + ", APP ID - " + displayUnsigned(wsCurrAppId, 11));
            }
        } else {
            display.accept("CHKP FAILED: DIBSTAT - " + dibstat
                    + ", REC COUNT - " + displaySigned(wsNoSumryRead, 8)
                    + ", APP ID - " + displayUnsigned(wsCurrAppId, 11));
            abend9999("CHKP status " + dibstat);
        }
    }

    // 9999-ABEND
    private void abend9999(String reason) {
        display.accept("CBPAUP0C ABENDING ...");
        throw new AbendException(reason);
    }

    String currentDate() {
        return currentDate;
    }

    long currentYyddd() {
        return currentYyddd;
    }

    long expiryDays() {
        return wsExpiryDays;
    }

    PrmInfo prmInfo() {
        return prmInfo;
    }

    /**
     * Command-line entry: {@code Cbpaup0c <dbpautp0-unload-in> [<dbpautp0-unload-out>]}. SYSIN (the control card)
     * is read from standard input; the surviving database is written to the output unload when given.
     */
    public static void main(String[] args) throws IOException {
        System.exit(runCli(args, System.in, System.out, Clock.systemDefaultZone()));
    }

    static int runCli(String[] args, InputStream sysin, PrintStream sysout, Clock clock) throws IOException {
        if (args.length < 1 || args.length > 2) {
            sysout.println("usage: Cbpaup0c <dbpautp0-unload-in> [<dbpautp0-unload-out>]  < sysin");
            return 8;
        }
        HdUnloadFile unload;
        try (InputStream in = Files.newInputStream(Path.of(args[0]))) {
            unload = HdUnloadFile.read(in, CobolCodec.EBCDIC);
        }
        BufferedReader reader = new BufferedReader(new InputStreamReader(sysin, StandardCharsets.UTF_8));
        String card = reader.readLine();
        int rc = new Cbpaup0c(unload.database(), card, clock, sysout::println).run();
        if (args.length == 2) {
            writeUnload(unload, Path.of(args[1]).toAbsolutePath(), Path.of(args[0]));
        }
        return rc;
    }

    /**
     * Writes to a new owner-only temp file beside the target, then renames it over the target, so the old file
     * survives a failure. A symlinked target is followed only for an in-place run (target is the input file);
     * otherwise the link itself is replaced, so a planted link cannot redirect the write.
     */
    private static void writeUnload(HdUnloadFile unload, Path target, Path input) throws IOException {
        Path real = Files.exists(target) && Files.isSameFile(target, input) ? target.toRealPath() : target;
        Path tmp = real.resolveSibling(real.getFileName() + "." + UUID.randomUUID() + ".tmp");
        Set<StandardOpenOption> options = EnumSet.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
        boolean posix = real.getFileSystem().supportedFileAttributeViews().contains("posix");
        FileAttribute<?>[] attrs = posix
                ? new FileAttribute<?>[] {PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))}
                : new FileAttribute<?>[0];
        try {
            try (OutputStream out = Channels.newOutputStream(Files.newByteChannel(tmp, options, attrs))) {
                unload.write(out);
            }
            Files.move(tmp, real, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } finally {
            Files.deleteIfExists(tmp);
        }
    }
}
