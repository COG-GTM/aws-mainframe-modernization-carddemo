package com.carddemo.programs.cbact01c;

import com.carddemo.programs.cbact01c.copybook.AccountRecord;
import com.carddemo.programs.cbact01c.copybook.CodatecnRec;
import com.carddemo.programs.cbact01c.gateway.DateFormatter;
import com.carddemo.programs.cbact01c.io.FileStatus;
import com.carddemo.programs.cbact01c.io.ReadResult;
import com.carddemo.programs.cbact01c.io.RecordWriter;
import com.carddemo.programs.cbact01c.io.SequentialRecordReader;
import com.carddemo.programs.cbact01c.record.ArrArrayRec;
import com.carddemo.programs.cbact01c.record.OutAcctRec;
import com.carddemo.programs.cbact01c.record.VbrcRec1;
import com.carddemo.programs.cbact01c.record.VbrcRec2;
import com.carddemo.programs.cbact01c.record.WsAcctReissueDate;
import com.carddemo.programs.cbact01c.support.Pic;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Locale;
import java.util.function.Consumer;

/**
 * CBACT01C: reads the account master (DD ACCTFILE) sequentially and, for every account, writes
 * <ul>
 *   <li>OUT-ACCT-REC to DD OUTFILE (fixed 107, reissue date reformatted by COBDATFT, COMP-3 cycle debit),</li>
 *   <li>ARR-ARRAY-REC to DD ARRYFILE (fixed 110, OCCURS 5 balance/debit pairs),</li>
 *   <li>VBRC-REC1 (12 bytes) and VBRC-REC2 (39 bytes) to DD VBRCFILE (variable length).</li>
 * </ul>
 * Paragraph names are kept as method names. DISPLAY output goes to {@code sysout}; 9999-ABEND-PROGRAM
 * throws {@link AbendException}.
 */
public final class Cbact01c {
    public static final int RETURN_CODE_OK = 0;
    public static final int ABEND_CODE = 999;

    static final int APPL_AOK = 0;
    static final int APPL_EOF = 16;

    private final SequentialRecordReader acctfileFile;
    private final RecordWriter outFile;
    private final RecordWriter arryFile;
    private final RecordWriter vbrcFile;
    private final DateFormatter cobdatft;
    private final Consumer<String> sysout;

    // FD record areas
    private final OutAcctRec outAcctRec = new OutAcctRec();
    private final ArrArrayRec arrArrayRec = new ArrArrayRec();

    // WORKING-STORAGE
    private AccountRecord accountRecord = AccountRecord.fromBytes(new byte[0]);
    private final CodatecnRec codatecnRec = new CodatecnRec();
    private String acctfileStatus = Pic.spaces(2);
    private String outfileStatus = Pic.spaces(2);
    private String arryfileStatus = Pic.spaces(2);
    private String vbrcfileStatus = Pic.spaces(2);
    private String ioStatus = Pic.spaces(2);
    private int applResult;
    private String endOfFile = "N";
    private int wsRecdLen;
    private final VbrcRec1 vbrcRec1 = new VbrcRec1();
    private final VbrcRec2 vbrcRec2 = new VbrcRec2();
    private final WsAcctReissueDate wsAcctReissueDate = new WsAcctReissueDate();

    public Cbact01c(SequentialRecordReader acctfileFile, RecordWriter outFile, RecordWriter arryFile,
                    RecordWriter vbrcFile, DateFormatter cobdatft, Consumer<String> sysout) {
        this.acctfileFile = acctfileFile;
        this.outFile = outFile;
        this.arryFile = arryFile;
        this.vbrcFile = vbrcFile;
        this.cobdatft = cobdatft;
        this.sysout = sysout;
    }

    /**
     * PROCEDURE DIVISION mainline. Returns RETURN-CODE 0 at GOBACK; throws {@link AbendException} when a
     * paragraph reaches 9999-ABEND-PROGRAM. Output files that COBOL closes implicitly at GOBACK are closed here.
     */
    public int run() {
        display("START OF EXECUTION OF PROGRAM CBACT01C");
        acctfileOpen0000();
        outfileOpen2000();
        arrfileOpen3000();
        vbrfileOpen4000();

        while (!endOfFile.equals("Y")) {
            if (endOfFile.equals("N")) {
                acctfileGetNext1000();
                if (endOfFile.equals("N")) {
                    display(accountRecord.asText());
                }
            }
        }

        acctfileClose9000();

        display("END OF EXECUTION OF PROGRAM CBACT01C");

        // GOBACK: run-unit end closes OUT-FILE, ARRY-FILE and VBRC-FILE implicitly.
        outFile.close();
        arryFile.close();
        vbrcFile.close();
        return RETURN_CODE_OK;
    }

    // 1000-ACCTFILE-GET-NEXT
    void acctfileGetNext1000() {
        ReadResult result = acctfileFile.read();
        acctfileStatus = result.status();
        if (acctfileStatus.equals(FileStatus.OK)) {
            accountRecord = AccountRecord.fromBytes(result.record());
            applResult = 0;
            arrArrayRec.initializeRecord();
            displayAcctRecord1100();
            populAcctRecord1300();
            writeAcctRecord1350();
            populArrayRecord1400();
            writeArryRecord1450();
            vbrcRec1.initializeRecord();
            populVbrcRecord1500();
            writeVb1Record1550();
            writeVb2Record1575();
        } else {
            if (acctfileStatus.equals(FileStatus.END_OF_FILE)) {
                applResult = 16;
            } else {
                applResult = 12;
            }
        }
        if (applResult == APPL_AOK) {
            // CONTINUE
        } else if (applResult == APPL_EOF) {
            endOfFile = "Y";
        } else {
            display("ERROR READING ACCOUNT FILE");
            ioStatus = acctfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 1100-DISPLAY-ACCT-RECORD
    void displayAcctRecord1100() {
        display("ACCT-ID                 :" + accountRecord.text(AccountRecord.ACCT_ID));
        display("ACCT-ACTIVE-STATUS      :" + accountRecord.text(AccountRecord.ACCT_ACTIVE_STATUS));
        display("ACCT-CURR-BAL           :" + accountRecord.text(AccountRecord.ACCT_CURR_BAL));
        display("ACCT-CREDIT-LIMIT       :" + accountRecord.text(AccountRecord.ACCT_CREDIT_LIMIT));
        display("ACCT-CASH-CREDIT-LIMIT  :" + accountRecord.text(AccountRecord.ACCT_CASH_CREDIT_LIMIT));
        display("ACCT-OPEN-DATE          :" + accountRecord.text(AccountRecord.ACCT_OPEN_DATE));
        display("ACCT-EXPIRAION-DATE     :" + accountRecord.text(AccountRecord.ACCT_EXPIRAION_DATE));
        display("ACCT-REISSUE-DATE       :" + accountRecord.text(AccountRecord.ACCT_REISSUE_DATE));
        display("ACCT-CURR-CYC-CREDIT    :" + accountRecord.text(AccountRecord.ACCT_CURR_CYC_CREDIT));
        display("ACCT-CURR-CYC-DEBIT     :" + accountRecord.text(AccountRecord.ACCT_CURR_CYC_DEBIT));
        display("ACCT-GROUP-ID           :" + accountRecord.text(AccountRecord.ACCT_GROUP_ID));
        display("-------------------------------------------------");
    }

    // 1300-POPUL-ACCT-RECORD
    void populAcctRecord1300() {
        outAcctRec.putRaw(OutAcctRec.OUT_ACCT_ID, accountRecord.raw(AccountRecord.ACCT_ID));
        outAcctRec.putRaw(OutAcctRec.OUT_ACCT_ACTIVE_STATUS, accountRecord.raw(AccountRecord.ACCT_ACTIVE_STATUS));
        outAcctRec.putRaw(OutAcctRec.OUT_ACCT_CURR_BAL, accountRecord.raw(AccountRecord.ACCT_CURR_BAL));
        outAcctRec.putRaw(OutAcctRec.OUT_ACCT_CREDIT_LIMIT, accountRecord.raw(AccountRecord.ACCT_CREDIT_LIMIT));
        outAcctRec.putRaw(OutAcctRec.OUT_ACCT_CASH_CREDIT_LIMIT,
                accountRecord.raw(AccountRecord.ACCT_CASH_CREDIT_LIMIT));
        outAcctRec.putRaw(OutAcctRec.OUT_ACCT_OPEN_DATE, accountRecord.raw(AccountRecord.ACCT_OPEN_DATE));
        outAcctRec.putRaw(OutAcctRec.OUT_ACCT_EXPIRAION_DATE, accountRecord.raw(AccountRecord.ACCT_EXPIRAION_DATE));
        codatecnRec.setCodatecnInpDate(accountRecord.acctReissueDate());
        wsAcctReissueDate.setWsReissueDate(accountRecord.acctReissueDate());
        codatecnRec.setCodatecnType("2");
        codatecnRec.setCodatecnOuttype("2");

        cobdatft.call(codatecnRec);

        outAcctRec.putText(OutAcctRec.OUT_ACCT_REISSUE_DATE, codatecnRec.codatecn0utDate());

        outAcctRec.putRaw(OutAcctRec.OUT_ACCT_CURR_CYC_CREDIT, accountRecord.raw(AccountRecord.ACCT_CURR_CYC_CREDIT));
        if (accountRecord.acctCurrCycDebit().signum() == 0) {
            outAcctRec.putNumber(OutAcctRec.OUT_ACCT_CURR_CYC_DEBIT, new BigDecimal("2525.00"));
        }
        outAcctRec.putRaw(OutAcctRec.OUT_ACCT_GROUP_ID, accountRecord.raw(AccountRecord.ACCT_GROUP_ID));
    }

    // 1350-WRITE-ACCT-RECORD
    void writeAcctRecord1350() {
        outfileStatus = outFile.write(outAcctRec.toBytes());
        if (!outfileStatus.equals("00") && !outfileStatus.equals("10")) {
            display("ACCOUNT FILE WRITE STATUS IS:" + outfileStatus);
            ioStatus = outfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 1400-POPUL-ARRAY-RECORD
    void populArrayRecord1400() {
        arrArrayRec.putRaw(ArrArrayRec.ARR_ACCT_ID, accountRecord.raw(AccountRecord.ACCT_ID));
        arrArrayRec.putRaw(ArrArrayRec.arrAcctCurrBal(1), accountRecord.raw(AccountRecord.ACCT_CURR_BAL));
        arrArrayRec.putNumber(ArrArrayRec.arrAcctCurrCycDebit(1), new BigDecimal("1005.00"));
        arrArrayRec.putRaw(ArrArrayRec.arrAcctCurrBal(2), accountRecord.raw(AccountRecord.ACCT_CURR_BAL));
        arrArrayRec.putNumber(ArrArrayRec.arrAcctCurrCycDebit(2), new BigDecimal("1525.00"));
        arrArrayRec.putNumber(ArrArrayRec.arrAcctCurrBal(3), new BigDecimal("-1025.00"));
        arrArrayRec.putNumber(ArrArrayRec.arrAcctCurrCycDebit(3), new BigDecimal("-2500.00"));
    }

    // 1450-WRITE-ARRY-RECORD
    void writeArryRecord1450() {
        arryfileStatus = arryFile.write(arrArrayRec.toBytes());
        if (!arryfileStatus.equals("00") && !arryfileStatus.equals("10")) {
            display("ACCOUNT FILE WRITE STATUS IS:" + arryfileStatus);
            ioStatus = arryfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 1500-POPUL-VBRC-RECORD
    void populVbrcRecord1500() {
        vbrcRec1.putRaw(VbrcRec1.VB1_ACCT_ID, accountRecord.raw(AccountRecord.ACCT_ID));
        vbrcRec2.putRaw(VbrcRec2.VB2_ACCT_ID, accountRecord.raw(AccountRecord.ACCT_ID));
        vbrcRec1.putRaw(VbrcRec1.VB1_ACCT_ACTIVE_STATUS, accountRecord.raw(AccountRecord.ACCT_ACTIVE_STATUS));
        vbrcRec2.putRaw(VbrcRec2.VB2_ACCT_CURR_BAL, accountRecord.raw(AccountRecord.ACCT_CURR_BAL));
        vbrcRec2.putRaw(VbrcRec2.VB2_ACCT_CREDIT_LIMIT, accountRecord.raw(AccountRecord.ACCT_CREDIT_LIMIT));
        vbrcRec2.putText(VbrcRec2.VB2_ACCT_REISSUE_YYYY, wsAcctReissueDate.wsAcctReissueYyyy());
        display("VBRC-REC1:" + vbrcRec1.asText());
        display("VBRC-REC2:" + vbrcRec2.asText());
    }

    // 1550-WRITE-VB1-RECORD
    void writeVb1Record1550() {
        wsRecdLen = 12;
        vbrcfileStatus = vbrcFile.write(Arrays.copyOf(vbrcRec1.toBytes(), wsRecdLen));
        if (!vbrcfileStatus.equals("00") && !vbrcfileStatus.equals("10")) {
            display("ACCOUNT FILE WRITE STATUS IS:" + vbrcfileStatus);
            ioStatus = vbrcfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 1575-WRITE-VB2-RECORD
    void writeVb2Record1575() {
        wsRecdLen = 39;
        vbrcfileStatus = vbrcFile.write(Arrays.copyOf(vbrcRec2.toBytes(), wsRecdLen));
        if (!vbrcfileStatus.equals("00") && !vbrcfileStatus.equals("10")) {
            display("ACCOUNT FILE WRITE STATUS IS:" + vbrcfileStatus);
            ioStatus = vbrcfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 0000-ACCTFILE-OPEN
    void acctfileOpen0000() {
        applResult = 8;
        acctfileStatus = acctfileFile.open();
        applResult = acctfileStatus.equals("00") ? 0 : 12;
        if (applResult != APPL_AOK) {
            display("ERROR OPENING ACCTFILE");
            ioStatus = acctfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 2000-OUTFILE-OPEN
    void outfileOpen2000() {
        applResult = 8;
        outfileStatus = outFile.open();
        applResult = outfileStatus.equals("00") ? 0 : 12;
        if (applResult != APPL_AOK) {
            display("ERROR OPENING OUTFILE" + outfileStatus);
            ioStatus = outfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 3000-ARRFILE-OPEN
    void arrfileOpen3000() {
        applResult = 8;
        arryfileStatus = arryFile.open();
        applResult = arryfileStatus.equals("00") ? 0 : 12;
        if (applResult != APPL_AOK) {
            display("ERROR OPENING ARRAYFILE" + arryfileStatus);
            ioStatus = arryfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 4000-VBRFILE-OPEN
    void vbrfileOpen4000() {
        applResult = 8;
        vbrcfileStatus = vbrcFile.open();
        applResult = vbrcfileStatus.equals("00") ? 0 : 12;
        if (applResult != APPL_AOK) {
            display("ERROR OPENING VBRC FILE" + vbrcfileStatus);
            ioStatus = vbrcfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 9000-ACCTFILE-CLOSE
    void acctfileClose9000() {
        applResult = 8;
        acctfileStatus = acctfileFile.close();
        if (acctfileStatus.equals("00")) {
            applResult = 0;
        } else {
            applResult = 12;
        }
        if (applResult != APPL_AOK) {
            display("ERROR CLOSING ACCOUNT FILE");
            ioStatus = acctfileStatus;
            displayIoStatus9910();
            abendProgram9999();
        }
    }

    // 9999-ABEND-PROGRAM
    void abendProgram9999() {
        display("ABENDING PROGRAM");
        throw new AbendException(ABEND_CODE, 0);
    }

    // 9910-DISPLAY-IO-STATUS
    void displayIoStatus9910() {
        display("FILE STATUS IS: NNNN" + formatIoStatus04(ioStatus));
    }

    /**
     * IO-STATUS-04 as built by 9910-DISPLAY-IO-STATUS. Non-numeric or '9x' statuses show IO-STAT1 followed by
     * the binary value of the IO-STAT2 byte as 3 digits (TWO-BYTES-BINARY trick); others show '00' + status.
     */
    static String formatIoStatus04(String ioStatus) {
        String status = Pic.x(ioStatus, 2);
        char ioStat1 = status.charAt(0);
        char ioStat2 = status.charAt(1);
        if (!Pic.isNumeric(status) || ioStat1 == '9') {
            int twoBytesBinary = ioStat2 & 0xFF;
            return ioStat1 + String.format(Locale.ROOT, "%03d", twoBytesBinary % 1000);
        }
        return "00" + status;
    }

    private void display(String line) {
        sysout.accept(line);
    }

    int applResult() {
        return applResult;
    }

    String endOfFile() {
        return endOfFile;
    }

    String ioStatus() {
        return ioStatus;
    }

    OutAcctRec outAcctRec() {
        return outAcctRec;
    }

    ArrArrayRec arrArrayRec() {
        return arrArrayRec;
    }

    CodatecnRec codatecnRec() {
        return codatecnRec;
    }

    int wsRecdLen() {
        return wsRecdLen;
    }
}
