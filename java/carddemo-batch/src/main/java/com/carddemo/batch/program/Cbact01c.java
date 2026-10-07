package com.carddemo.batch.program;

import com.carddemo.batch.io.AbendException;
import com.carddemo.batch.io.FileStatusException;
import com.carddemo.batch.io.FixedRecordWriter;
import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.io.RecordPrefix;
import com.carddemo.batch.io.VariableRecordWriter;
import com.carddemo.batch.record.AccountRecord;
import com.carddemo.batch.record.ArrArrayRec;
import com.carddemo.batch.record.CodatecnRec;
import com.carddemo.batch.record.OutAcctRec;
import com.carddemo.batch.record.VbrcRec1;
import com.carddemo.batch.record.VbrcRec2;

import java.io.PrintStream;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Optional;

/**
 * Java 17 port of {@code app/cbl/CBACT01C.cbl}: read the account KSDS sequentially and write OUTFILE
 * (fixed 107), ARRYFILE (fixed 110) and VBRCFILE (variable 12 / 39) plus the DISPLAY log.
 *
 * <p>The paragraph structure of the COBOL program is kept one-to-one (method names carry the paragraph
 * names); WORKING-STORAGE items are instance fields; the FD record areas are the copybook-backed record
 * classes, which are reused across records exactly as COBOL does.
 */
public final class Cbact01c {

    public static final int ABEND_CODE = 999;
    private static final BigDecimal CYC_DEBIT_SUBSTITUTE = new BigDecimal("2525.00");
    private static final BigDecimal ARR_CYC_DEBIT_1 = new BigDecimal("1005.00");
    private static final BigDecimal ARR_CYC_DEBIT_2 = new BigDecimal("1525.00");
    private static final BigDecimal ARR_CURR_BAL_3 = new BigDecimal("-1025.00");
    private static final BigDecimal ARR_CYC_DEBIT_3 = new BigDecimal("-2500.00");
    private static final int VBR_REC_MIN = 10;
    private static final int VBR_REC_MAX = 80;
    private static final int VB1_LENGTH = 12;
    private static final int VB2_LENGTH = 39;
    private static final int APPL_AOK = 0;
    private static final int APPL_EOF = 16;

    // ----- FILE SECTION -----------------------------------------------------------------------
    private final KsdsFile acctfileFile;
    private final FixedRecordWriter outFile;
    private final OutAcctRec outAcctRec = new OutAcctRec();
    private final FixedRecordWriter arryFile;
    private final ArrArrayRec arrArrayRec = new ArrArrayRec();
    private final VariableRecordWriter vbrcFile;
    private final byte[] vbrRec = blankVbrRec();

    // ----- WORKING-STORAGE SECTION ------------------------------------------------------------
    private final AccountRecord accountRecord = new AccountRecord();
    private final CodatecnRec codatecnRec = new CodatecnRec();
    private String acctfileStatus = "00";
    private String outfileStatus = "00";
    private String arryfileStatus = "00";
    private String vbrcfileStatus = "00";
    private String ioStatus = "00";
    private int applResult;
    private boolean endOfFile;
    private int wsRecdLen;
    private final VbrcRec1 vbrcRec1 = new VbrcRec1();
    private final VbrcRec2 vbrcRec2 = new VbrcRec2();
    private String wsAcctReissueDate = "          ";

    private final PrintStream display;

    public Cbact01c(Path acctfile, Path outfile, Path arryfile, Path vbrcfile, PrintStream display) {
        this(acctfile, outfile, arryfile, vbrcfile, RecordPrefix.GNUCOBOL_VARSEQ, display);
    }

    public Cbact01c(Path acctfile, Path outfile, Path arryfile, Path vbrcfile, RecordPrefix vbrcPrefix,
                    PrintStream display) {
        this.acctfileFile = new KsdsFile("ACCTFILE", acctfile, AccountRecord.LENGTH,
                AccountRecord.ACCT_ID.offset(), AccountRecord.ACCT_ID.length());
        this.outFile = new FixedRecordWriter("OUTFILE", outfile, OutAcctRec.LENGTH);
        this.arryFile = new FixedRecordWriter("ARRYFILE", arryfile, ArrArrayRec.LENGTH);
        this.vbrcFile = new VariableRecordWriter("VBRCFILE", vbrcfile, vbrcPrefix, VBR_REC_MIN, VBR_REC_MAX);
        this.display = display;
        // FD record areas are never INITIALIZEd by the program; OUT-ACCT-REC starts as LOW-VALUES
        // (GnuCOBOL) so that fields the program never MOVEs to (OUT-ACCT-CURR-CYC-DEBIT on a
        // non-zero input before the first zero-debit record) carry the same bytes as the golden.
        this.outAcctRec.lowValues();
    }

    /** {@code 01 VBR-REC PIC X(80)}: the FD record area of VBRC-FILE, space filled. */
    private static byte[] blankVbrRec() {
        byte[] b = new byte[VBR_REC_MAX];
        Arrays.fill(b, (byte) ' ');
        return b;
    }

    /**
     * {@code java -jar carddemo-batch.jar [ACCTFILE OUTFILE ARRYFILE VBRCFILE]}; defaults to the sample data
     * in {@code app/data/ASCII/acctdata.txt} and output files in the current directory.
     */
    public static void main(String[] args) {
        Path acct = args.length > 0 ? Paths.get(args[0]) : Paths.get("app", "data", "ASCII", "acctdata.txt");
        Path out = args.length > 1 ? Paths.get(args[1]) : Paths.get("OUTFILE");
        Path arry = args.length > 2 ? Paths.get(args[2]) : Paths.get("ARRYFILE");
        Path vbrc = args.length > 3 ? Paths.get(args[3]) : Paths.get("VBRCFILE");
        try {
            new Cbact01c(acct, out, arry, vbrc, System.out).run();
        } catch (AbendException e) {
            System.out.println(e.getMessage());
            System.out.flush();
            System.exit(e.abendCode());
        }
        System.out.flush();
    }

    /** PROCEDURE DIVISION. Returns the COBOL RETURN-CODE (0); abends surface as {@link AbendException}. */
    public int run() {
        display.println("START OF EXECUTION OF PROGRAM CBACT01C");
        acctfileOpen();            // 0000-ACCTFILE-OPEN
        outfileOpen();             // 2000-OUTFILE-OPEN
        arrfileOpen();             // 3000-ARRFILE-OPEN
        vbrfileOpen();             // 4000-VBRFILE-OPEN

        while (!endOfFile) {
            acctfileGetNext();     // 1000-ACCTFILE-GET-NEXT
            if (!endOfFile) {
                display.println(accountRecord.display());
            }
        }

        acctfileClose();           // 9000-ACCTFILE-CLOSE
        // The COBOL program never CLOSEs its three output files; GOBACK closes them implicitly.
        outFile.close();
        arryFile.close();
        vbrcFile.close();

        display.println("END OF EXECUTION OF PROGRAM CBACT01C");
        display.flush();
        return 0;
    }

    // ----- 1000-ACCTFILE-GET-NEXT -------------------------------------------------------------
    void acctfileGetNext() {
        Optional<byte[]> next;
        try {
            next = acctfileFile.readNext();
            acctfileStatus = next.isPresent() ? "00" : FileStatusException.END_OF_FILE;
        } catch (FileStatusException e) {
            acctfileStatus = e.status();
            next = Optional.empty();
        }
        if ("00".equals(acctfileStatus)) {
            accountRecord.moveFrom(next.get());
            applResult = 0;
            arrArrayRec.initialize();
            displayAcctRecord();   // 1100
            populAcctRecord();     // 1300
            writeAcctRecord();     // 1350
            populArrayRecord();    // 1400
            writeArryRecord();     // 1450
            vbrcRec1.initialize();
            populVbrcRecord();     // 1500
            writeVb1Record();      // 1550
            writeVb2Record();      // 1575
        } else if (FileStatusException.END_OF_FILE.equals(acctfileStatus)) {
            applResult = APPL_EOF;
        } else {
            applResult = 12;
        }
        if (applResult != APPL_AOK) {
            if (applResult == APPL_EOF) {
                endOfFile = true;
            } else {
                display.println("ERROR READING ACCOUNT FILE");
                ioStatus = acctfileStatus;
                displayIoStatus();
                abendProgram();
            }
        }
    }

    // ----- 1100-DISPLAY-ACCT-RECORD -----------------------------------------------------------
    void displayAcctRecord() {
        display.println("ACCT-ID                 :" + accountRecord.display(AccountRecord.ACCT_ID));
        display.println("ACCT-ACTIVE-STATUS      :" + accountRecord.display(AccountRecord.ACCT_ACTIVE_STATUS));
        display.println("ACCT-CURR-BAL           :" + accountRecord.display(AccountRecord.ACCT_CURR_BAL));
        display.println("ACCT-CREDIT-LIMIT       :" + accountRecord.display(AccountRecord.ACCT_CREDIT_LIMIT));
        display.println("ACCT-CASH-CREDIT-LIMIT  :" + accountRecord.display(AccountRecord.ACCT_CASH_CREDIT_LIMIT));
        display.println("ACCT-OPEN-DATE          :" + accountRecord.display(AccountRecord.ACCT_OPEN_DATE));
        display.println("ACCT-EXPIRAION-DATE     :" + accountRecord.display(AccountRecord.ACCT_EXPIRAION_DATE));
        display.println("ACCT-REISSUE-DATE       :" + accountRecord.display(AccountRecord.ACCT_REISSUE_DATE));
        display.println("ACCT-CURR-CYC-CREDIT    :" + accountRecord.display(AccountRecord.ACCT_CURR_CYC_CREDIT));
        display.println("ACCT-CURR-CYC-DEBIT     :" + accountRecord.display(AccountRecord.ACCT_CURR_CYC_DEBIT));
        display.println("ACCT-GROUP-ID           :" + accountRecord.display(AccountRecord.ACCT_GROUP_ID));
        display.println("-------------------------------------------------");
    }

    // ----- 1300-POPUL-ACCT-RECORD -------------------------------------------------------------
    void populAcctRecord() {
        outAcctRec.setOutAcctId(accountRecord.acctId());
        outAcctRec.setOutAcctActiveStatus(accountRecord.acctActiveStatus());
        outAcctRec.setOutAcctCurrBal(accountRecord.acctCurrBal());
        outAcctRec.setOutAcctCreditLimit(accountRecord.acctCreditLimit());
        outAcctRec.setOutAcctCashCreditLimit(accountRecord.acctCashCreditLimit());
        outAcctRec.setOutAcctOpenDate(accountRecord.acctOpenDate());
        outAcctRec.setOutAcctExpiraionDate(accountRecord.acctExpiraionDate());
        codatecnRec.setCodatecnInpDate(accountRecord.acctReissueDate());
        wsAcctReissueDate = accountRecord.acctReissueDate();
        codatecnRec.setCodatecnType(CodatecnRec.YYYY_MM_DD);
        codatecnRec.setCodatecnOuttype(CodatecnRec.YYYY_MM_DD);

        // CALL 'COBDATFT' USING CODATECN-REC (assembler date formatting)
        CobDatFt.call(codatecnRec);

        // MOVE X(20) TO X(10): the first ten bytes, i.e. YYYYMMDD plus the two untouched bytes (spaces)
        outAcctRec.setOutAcctReissueDate(codatecnRec.codatecnOutDate().substring(0, 10));

        outAcctRec.setOutAcctCurrCycCredit(accountRecord.acctCurrCycCredit());
        if (accountRecord.acctCurrCycDebit().signum() == 0) {
            outAcctRec.setOutAcctCurrCycDebit(CYC_DEBIT_SUBSTITUTE);
        }
        outAcctRec.setOutAcctGroupId(accountRecord.acctGroupId());
    }

    // ----- 1350-WRITE-ACCT-RECORD -------------------------------------------------------------
    void writeAcctRecord() {
        try {
            outFile.write(outAcctRec.encode());
            outfileStatus = "00";
        } catch (FileStatusException e) {
            outfileStatus = e.status();
        }
        if (!"00".equals(outfileStatus) && !"10".equals(outfileStatus)) {
            display.println("ACCOUNT FILE WRITE STATUS IS:" + outfileStatus);
            ioStatus = outfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 1400-POPUL-ARRAY-RECORD ------------------------------------------------------------
    void populArrayRecord() {
        arrArrayRec.setArrAcctId(accountRecord.acctId());
        arrArrayRec.arrAcctBal(1).setArrAcctCurrBal(accountRecord.acctCurrBal());
        arrArrayRec.arrAcctBal(1).setArrAcctCurrCycDebit(ARR_CYC_DEBIT_1);
        arrArrayRec.arrAcctBal(2).setArrAcctCurrBal(accountRecord.acctCurrBal());
        arrArrayRec.arrAcctBal(2).setArrAcctCurrCycDebit(ARR_CYC_DEBIT_2);
        arrArrayRec.arrAcctBal(3).setArrAcctCurrBal(ARR_CURR_BAL_3);
        arrArrayRec.arrAcctBal(3).setArrAcctCurrCycDebit(ARR_CYC_DEBIT_3);
    }

    // ----- 1450-WRITE-ARRY-RECORD -------------------------------------------------------------
    void writeArryRecord() {
        try {
            arryFile.write(arrArrayRec.encode());
            arryfileStatus = "00";
        } catch (FileStatusException e) {
            arryfileStatus = e.status();
        }
        if (!"00".equals(arryfileStatus) && !"10".equals(arryfileStatus)) {
            display.println("ACCOUNT FILE WRITE STATUS IS:" + arryfileStatus);
            ioStatus = arryfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 1500-POPUL-VBRC-RECORD -------------------------------------------------------------
    void populVbrcRecord() {
        vbrcRec1.setVb1AcctId(accountRecord.acctId());
        vbrcRec2.setVb2AcctId(accountRecord.acctId());
        vbrcRec1.setVb1AcctActiveStatus(accountRecord.acctActiveStatus());
        vbrcRec2.setVb2AcctCurrBal(accountRecord.acctCurrBal());
        vbrcRec2.setVb2AcctCreditLimit(accountRecord.acctCreditLimit());
        vbrcRec2.setVb2AcctReissueYyyy(wsAcctReissueYyyy());
        display.println("VBRC-REC1:" + vbrcRec1.display());
        display.println("VBRC-REC2:" + vbrcRec2.display());
    }

    /** {@code WS-ACCT-REISSUE-YYYY PIC X(04)}: the first four characters of WS-ACCT-REISSUE-DATE. */
    private String wsAcctReissueYyyy() {
        return wsAcctReissueDate.substring(0, 4);
    }

    // ----- 1550-WRITE-VB1-RECORD --------------------------------------------------------------
    void writeVb1Record() {
        wsRecdLen = VB1_LENGTH;
        moveToVbrRec(vbrcRec1.encode());
        try {
            vbrcFile.write(vbrRec, wsRecdLen);
            vbrcfileStatus = "00";
        } catch (FileStatusException e) {
            vbrcfileStatus = e.status();
        }
        checkVbrcWrite();
    }

    // ----- 1575-WRITE-VB2-RECORD --------------------------------------------------------------
    void writeVb2Record() {
        wsRecdLen = VB2_LENGTH;
        moveToVbrRec(vbrcRec2.encode());
        try {
            vbrcFile.write(vbrRec, wsRecdLen);
            vbrcfileStatus = "00";
        } catch (FileStatusException e) {
            vbrcfileStatus = e.status();
        }
        checkVbrcWrite();
    }

    /** {@code MOVE VBRC-RECn TO VBR-REC(1:WS-RECD-LEN)}. */
    private void moveToVbrRec(byte[] source) {
        int n = Math.min(source.length, wsRecdLen);
        System.arraycopy(source, 0, vbrRec, 0, n);
        Arrays.fill(vbrRec, n, wsRecdLen, (byte) ' ');
    }

    private void checkVbrcWrite() {
        if (!"00".equals(vbrcfileStatus) && !"10".equals(vbrcfileStatus)) {
            display.println("ACCOUNT FILE WRITE STATUS IS:" + vbrcfileStatus);
            ioStatus = vbrcfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 0000-ACCTFILE-OPEN -----------------------------------------------------------------
    void acctfileOpen() {
        applResult = 8;
        try {
            acctfileFile.open();
            acctfileStatus = "00";
        } catch (FileStatusException e) {
            acctfileStatus = e.status();
        }
        applResult = "00".equals(acctfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING ACCTFILE");
            ioStatus = acctfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 2000-OUTFILE-OPEN ------------------------------------------------------------------
    void outfileOpen() {
        applResult = 8;
        try {
            outFile.open();
            outfileStatus = "00";
        } catch (FileStatusException e) {
            outfileStatus = e.status();
        }
        applResult = "00".equals(outfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING OUTFILE" + outfileStatus);
            ioStatus = outfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 3000-ARRFILE-OPEN ------------------------------------------------------------------
    void arrfileOpen() {
        applResult = 8;
        try {
            arryFile.open();
            arryfileStatus = "00";
        } catch (FileStatusException e) {
            arryfileStatus = e.status();
        }
        applResult = "00".equals(arryfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING ARRAYFILE" + arryfileStatus);
            ioStatus = arryfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 4000-VBRFILE-OPEN ------------------------------------------------------------------
    void vbrfileOpen() {
        applResult = 8;
        try {
            vbrcFile.open();
            vbrcfileStatus = "00";
        } catch (FileStatusException e) {
            vbrcfileStatus = e.status();
        }
        applResult = "00".equals(vbrcfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING VBRC FILE" + vbrcfileStatus);
            ioStatus = vbrcfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 9000-ACCTFILE-CLOSE ----------------------------------------------------------------
    void acctfileClose() {
        applResult = 8;
        try {
            acctfileFile.close();
            acctfileStatus = "00";
        } catch (FileStatusException e) {
            acctfileStatus = e.status();
        }
        applResult = "00".equals(acctfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR CLOSING ACCOUNT FILE");
            ioStatus = acctfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 9999-ABEND-PROGRAM -----------------------------------------------------------------
    void abendProgram() {
        display.println("ABENDING PROGRAM");
        display.flush();
        int timing = 0;
        int abcode = ABEND_CODE;
        throw new AbendException(abcode, timing,
                new FileStatusException(lastFailedDdname(), "I/O", ioStatus));
    }

    private String lastFailedDdname() {
        if (ioStatus.equals(acctfileStatus) && !"00".equals(acctfileStatus)) {
            return "ACCTFILE";
        }
        if (ioStatus.equals(outfileStatus) && !"00".equals(outfileStatus)) {
            return "OUTFILE";
        }
        if (ioStatus.equals(arryfileStatus) && !"00".equals(arryfileStatus)) {
            return "ARRYFILE";
        }
        return "VBRCFILE";
    }

    // ----- 9910-DISPLAY-IO-STATUS -------------------------------------------------------------
    void displayIoStatus() {
        String ioStatus04;
        if (!isNumeric(ioStatus) || ioStatus.charAt(0) == '9') {
            // IO-STATUS-0401 <- IO-STAT1; IO-STATUS-0403 <- binary value of the IO-STAT2 byte
            int twoBytesBinary = ioStatus.charAt(1) & 0xFF;
            ioStatus04 = ioStatus.charAt(0) + String.format("%03d", twoBytesBinary % 1000);
        } else {
            ioStatus04 = "00" + ioStatus;
        }
        display.println("FILE STATUS IS: NNNN" + ioStatus04);
    }

    private static boolean isNumeric(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) < '0' || s.charAt(i) > '9') {
                return false;
            }
        }
        return true;
    }

    /** The current FILE STATUS of ACCTFILE (for tests). */
    public String acctfileStatus() {
        return acctfileStatus;
    }
}
