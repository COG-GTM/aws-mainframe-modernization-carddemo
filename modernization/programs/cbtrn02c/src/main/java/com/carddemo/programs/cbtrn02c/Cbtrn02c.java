package com.carddemo.programs.cbtrn02c;

import com.carddemo.programs.cbtrn02c.copybook.AccountRecord;
import com.carddemo.programs.cbtrn02c.copybook.CardXrefRecord;
import com.carddemo.programs.cbtrn02c.copybook.DalytranRecord;
import com.carddemo.programs.cbtrn02c.copybook.Pic;
import com.carddemo.programs.cbtrn02c.copybook.TranCatBalRecord;
import com.carddemo.programs.cbtrn02c.copybook.TranCatBalRecord.TranCatKey;
import com.carddemo.programs.cbtrn02c.copybook.TranRecord;
import com.carddemo.programs.cbtrn02c.io.FileStatus;
import com.carddemo.programs.cbtrn02c.io.IndexedFile;
import com.carddemo.programs.cbtrn02c.io.OpenMode;
import com.carddemo.programs.cbtrn02c.io.ReadResult;
import com.carddemo.programs.cbtrn02c.io.SequentialReader;
import com.carddemo.programs.cbtrn02c.io.SequentialWriter;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.function.Consumer;

/**
 * CBTRN02C - post the records from the daily transaction file (JCL POSTTRAN, step STEP15).
 *
 * <p>DD names: DALYTRAN (sequential in), TRANFILE (KSDS, OPEN OUTPUT), XREFFILE (KSDS in),
 * DALYREJS (sequential out, LRECL 430), ACCTFILE (KSDS I-O), TCATBALF (KSDS I-O).
 * {@link #run()} returns RETURN-CODE (0, or 4 when any transaction was rejected); a failed file
 * operation ends in {@link Cbtrn02cAbendException} with ABCODE 999.
 */
public class Cbtrn02c {

    public static final int REJECT_RECORD_LENGTH = 430;
    public static final int ABCODE = 999;

    private final SequentialReader dalytranFile;
    private final IndexedFile transactFile;
    private final IndexedFile xrefFile;
    private final SequentialWriter dalyrejsFile;
    private final IndexedFile accountFile;
    private final IndexedFile tcatbalFile;
    private final Clock clock;
    private final Consumer<String> sysout;

    // WORKING-STORAGE
    private String dalytranRaw = " ".repeat(DalytranRecord.RECORD_LENGTH);
    private DalytranRecord dalytranRecord;
    private TranRecord tranRecord = TranRecord.parse(initialRecord(TranRecord.RECORD_LENGTH, 18, 4, 132, 11, 143, 9));
    private CardXrefRecord cardXrefRecord = CardXrefRecord.parse(initialRecord(CardXrefRecord.RECORD_LENGTH, 16, 9, 25, 11));
    private AccountRecord accountRecord = AccountRecord.parse(initialRecord(AccountRecord.RECORD_LENGTH,
            0, 11, 12, 36, 78, 24));
    private TranCatBalRecord tranCatBalRecord = TranCatBalRecord.parse(initialRecord(TranCatBalRecord.RECORD_LENGTH,
            0, 11, 13, 15));
    private String dalytranStatus = "00";
    private String tranfileStatus = "00";
    private String xreffileStatus = "00";
    private String dalyrejsStatus = "00";
    private String acctfileStatus = "00";
    private String tcatbalfStatus = "00";
    private ApplResult applResult = ApplResult.OTHER;
    private boolean endOfFile;
    private int wsValidationFailReason;
    private String wsValidationFailReasonDesc = "";
    private long wsTransactionCount;
    private long wsRejectCount;
    private boolean wsCreateTrancatRec;
    private int returnCode;

    /** APPL-RESULT PIC S9(9) COMP with 88 APPL-AOK VALUE 0 and 88 APPL-EOF VALUE 16. */
    enum ApplResult {
        AOK(0), EOF(16), ERROR(12), OTHER(8);

        final int value;

        ApplResult(int value) {
            this.value = value;
        }
    }

    public Cbtrn02c(SequentialReader dalytranFile, IndexedFile transactFile, IndexedFile xrefFile,
                    SequentialWriter dalyrejsFile, IndexedFile accountFile, IndexedFile tcatbalFile,
                    Clock clock, Consumer<String> sysout) {
        this.dalytranFile = dalytranFile;
        this.transactFile = transactFile;
        this.xrefFile = xrefFile;
        this.dalyrejsFile = dalyrejsFile;
        this.accountFile = accountFile;
        this.tcatbalFile = tcatbalFile;
        this.clock = clock;
        this.sysout = sysout;
    }

    /** PROCEDURE DIVISION main line. */
    public int run() {
        display("START OF EXECUTION OF PROGRAM CBTRN02C");
        dalytranOpen();
        tranfileOpen();
        xreffileOpen();
        dalyrejsOpen();
        acctfileOpen();
        tcatbalfOpen();

        while (!endOfFile) {
            dalytranGetNext();
            if (!endOfFile) {
                wsTransactionCount = addCounter(wsTransactionCount);
                wsValidationFailReason = 0;
                wsValidationFailReasonDesc = "";
                validateTran();
                if (wsValidationFailReason == 0) {
                    postTransaction();
                } else {
                    wsRejectCount = addCounter(wsRejectCount);
                    writeRejectRec();
                }
            }
        }

        dalytranClose();
        tranfileClose();
        xreffileClose();
        dalyrejsClose();
        acctfileClose();
        tcatbalfClose();
        display("TRANSACTIONS PROCESSED :" + Pic.unsigned(wsTransactionCount, 9));
        display("TRANSACTIONS REJECTED  :" + Pic.unsigned(wsRejectCount, 9));
        if (wsRejectCount > 0) {
            returnCode = 4;
        }
        display("END OF EXECUTION OF PROGRAM CBTRN02C");
        return returnCode;
    }

    public long transactionCount() {
        return wsTransactionCount;
    }

    public long rejectCount() {
        return wsRejectCount;
    }

    // 0000-DALYTRAN-OPEN
    void dalytranOpen() {
        dalytranStatus = dalytranFile.open();
        checkStatus(dalytranStatus, "ERROR OPENING DALYTRAN", dalytranStatus);
    }

    // 0100-TRANFILE-OPEN
    void tranfileOpen() {
        tranfileStatus = transactFile.open(OpenMode.OUTPUT);
        checkStatus(tranfileStatus, "ERROR OPENING TRANSACTION FILE", tranfileStatus);
    }

    // 0200-XREFFILE-OPEN
    void xreffileOpen() {
        xreffileStatus = xrefFile.open(OpenMode.INPUT);
        checkStatus(xreffileStatus, "ERROR OPENING CROSS REF FILE", xreffileStatus);
    }

    // 0300-DALYREJS-OPEN
    void dalyrejsOpen() {
        dalyrejsStatus = dalyrejsFile.open();
        checkStatus(dalyrejsStatus, "ERROR OPENING DALY REJECTS FILE", dalyrejsStatus);
    }

    // 0400-ACCTFILE-OPEN
    void acctfileOpen() {
        acctfileStatus = accountFile.open(OpenMode.I_O);
        checkStatus(acctfileStatus, "ERROR OPENING ACCOUNT MASTER FILE", acctfileStatus);
    }

    // 0500-TCATBALF-OPEN
    void tcatbalfOpen() {
        tcatbalfStatus = tcatbalFile.open(OpenMode.I_O);
        checkStatus(tcatbalfStatus, "ERROR OPENING TRANSACTION BALANCE FILE", tcatbalfStatus);
    }

    // 1000-DALYTRAN-GET-NEXT
    void dalytranGetNext() {
        ReadResult r = dalytranFile.read();
        dalytranStatus = r.status();
        if (FileStatus.OK.equals(dalytranStatus)) {
            dalytranRaw = r.record();
            dalytranRecord = DalytranRecord.parse(dalytranRaw);
            applResult = ApplResult.AOK;
        } else if (FileStatus.END_OF_FILE.equals(dalytranStatus)) {
            applResult = ApplResult.EOF;
        } else {
            applResult = ApplResult.ERROR;
        }
        if (applResult == ApplResult.AOK) {
            return;
        }
        if (applResult == ApplResult.EOF) {
            endOfFile = true;
        } else {
            display("ERROR READING DALYTRAN FILE");
            displayIoStatus(dalytranStatus);
            abendProgram("ERROR READING DALYTRAN FILE", dalytranStatus);
        }
    }

    // 1500-VALIDATE-TRAN
    void validateTran() {
        lookupXref();
        if (wsValidationFailReason == 0) {
            lookupAcct();
        }
    }

    // 1500-A-LOOKUP-XREF
    void lookupXref() {
        ReadResult r = xrefFile.read(Pic.alnum(dalytranRecord.dalytranCardNum(), CardXrefRecord.KEY_LENGTH));
        xreffileStatus = r.status();
        if (FileStatus.isInvalidKey(xreffileStatus)) {
            wsValidationFailReason = 100;
            wsValidationFailReasonDesc = "INVALID CARD NUMBER FOUND";
        } else if (r.ok()) {
            cardXrefRecord = CardXrefRecord.parse(r.record());
        }
    }

    // 1500-B-LOOKUP-ACCT
    void lookupAcct() {
        ReadResult r = accountFile.read(Pic.unsigned(cardXrefRecord.xrefAcctId(), AccountRecord.KEY_LENGTH));
        acctfileStatus = r.status();
        if (FileStatus.isInvalidKey(acctfileStatus)) {
            wsValidationFailReason = 101;
            wsValidationFailReasonDesc = "ACCOUNT RECORD NOT FOUND";
        } else if (r.ok()) {
            accountRecord = AccountRecord.parse(r.record());
            BigDecimal wsTempBal = Pic.truncate(accountRecord.acctCurrCycCredit()
                    .subtract(accountRecord.acctCurrCycDebit())
                    .add(dalytranRecord.dalytranAmt()), 9, 2);
            if (accountRecord.acctCreditLimit().compareTo(wsTempBal) < 0) {
                wsValidationFailReason = 102;
                wsValidationFailReasonDesc = "OVERLIMIT TRANSACTION";
            }
            if (accountRecord.acctExpiraionDate().compareTo(dalytranRecord.dalytranOrigTs().substring(0, 10)) < 0) {
                wsValidationFailReason = 103;
                wsValidationFailReasonDesc = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION";
            }
        }
    }

    // 2000-POST-TRANSACTION
    void postTransaction() {
        DalytranRecord d = dalytranRecord;
        tranRecord = new TranRecord(d.dalytranId(), d.dalytranTypeCd(), d.dalytranCatCd(), d.dalytranSource(),
                d.dalytranDesc(), d.dalytranAmt(), d.dalytranMerchantId(), d.dalytranMerchantName(),
                d.dalytranMerchantCity(), d.dalytranMerchantZip(), d.dalytranCardNum(), d.dalytranOrigTs(),
                getDb2FormatTimestamp(), tranRecord.filler());
        updateTcatbal();
        updateAccountRec();
        writeTransactionFile();
    }

    // 2500-WRITE-REJECT-REC
    void writeRejectRec() {
        String rejectRecord = Pic.alnum(dalytranRaw, DalytranRecord.RECORD_LENGTH) + validationTrailer();
        applResult = ApplResult.OTHER;
        dalyrejsStatus = dalyrejsFile.write(rejectRecord);
        checkStatus(dalyrejsStatus, "ERROR WRITING TO REJECTS FILE", dalyrejsStatus);
    }

    /** WS-VALIDATION-TRAILER: WS-VALIDATION-FAIL-REASON 9(04) + WS-VALIDATION-FAIL-REASON-DESC X(76). */
    String validationTrailer() {
        return Pic.unsigned(wsValidationFailReason, 4) + Pic.alnum(wsValidationFailReasonDesc, 76);
    }

    // 2700-UPDATE-TCATBAL
    void updateTcatbal() {
        TranCatKey key = new TranCatKey(cardXrefRecord.xrefAcctId(), dalytranRecord.dalytranTypeCd(),
                dalytranRecord.dalytranCatCd());
        String fdTranCatKey = key.format();
        wsCreateTrancatRec = false;
        ReadResult r = tcatbalFile.read(fdTranCatKey);
        tcatbalfStatus = r.status();
        if (FileStatus.isInvalidKey(tcatbalfStatus)) {
            display("TCATBAL record not found for key : " + fdTranCatKey + ".. Creating.");
            wsCreateTrancatRec = true;
        } else if (r.ok()) {
            tranCatBalRecord = TranCatBalRecord.parse(r.record());
        }
        if (FileStatus.OK.equals(tcatbalfStatus) || FileStatus.RECORD_NOT_FOUND.equals(tcatbalfStatus)) {
            applResult = ApplResult.AOK;
        } else {
            applResult = ApplResult.ERROR;
        }
        if (applResult != ApplResult.AOK) {
            display("ERROR READING TRANSACTION BALANCE FILE");
            displayIoStatus(tcatbalfStatus);
            abendProgram("ERROR READING TRANSACTION BALANCE FILE", tcatbalfStatus);
        }
        if (wsCreateTrancatRec) {
            createTcatbalRec();
        } else {
            updateTcatbalRec();
        }
    }

    // 2700-A-CREATE-TCATBAL-REC (INITIALIZE leaves FILLER untouched)
    void createTcatbalRec() {
        TranCatKey key = new TranCatKey(cardXrefRecord.xrefAcctId(), dalytranRecord.dalytranTypeCd(),
                dalytranRecord.dalytranCatCd());
        BigDecimal bal = Pic.truncate(BigDecimal.ZERO.setScale(2).add(dalytranRecord.dalytranAmt()),
                TranCatBalRecord.BAL_INT_DIGITS, TranCatBalRecord.BAL_SCALE);
        tranCatBalRecord = new TranCatBalRecord(key, bal, tranCatBalRecord.filler());
        tcatbalfStatus = tcatbalFile.write(tranCatBalRecord.format());
        checkStatus(tcatbalfStatus, "ERROR WRITING TRANSACTION BALANCE FILE", tcatbalfStatus);
    }

    // 2700-B-UPDATE-TCATBAL-REC
    void updateTcatbalRec() {
        BigDecimal bal = Pic.truncate(tranCatBalRecord.tranCatBal().add(dalytranRecord.dalytranAmt()),
                TranCatBalRecord.BAL_INT_DIGITS, TranCatBalRecord.BAL_SCALE);
        tranCatBalRecord = new TranCatBalRecord(tranCatBalRecord.tranCatKey(), bal, tranCatBalRecord.filler());
        tcatbalfStatus = tcatbalFile.rewrite(tranCatBalRecord.format());
        checkStatus(tcatbalfStatus, "ERROR REWRITING TRANSACTION BALANCE FILE", tcatbalfStatus);
    }

    // 2800-UPDATE-ACCOUNT-REC
    void updateAccountRec() {
        BigDecimal amt = dalytranRecord.dalytranAmt();
        BigDecimal currBal = acctAdd(accountRecord.acctCurrBal(), amt);
        BigDecimal cycCredit = accountRecord.acctCurrCycCredit();
        BigDecimal cycDebit = accountRecord.acctCurrCycDebit();
        if (amt.signum() >= 0) {
            cycCredit = acctAdd(cycCredit, amt);
        } else {
            cycDebit = acctAdd(cycDebit, amt);
        }
        accountRecord = accountRecord.withBalances(currBal, cycCredit, cycDebit);
        acctfileStatus = accountFile.rewrite(accountRecord.format());
        // REWRITE ... INVALID KEY only; no FILE STATUS check, so other failures are ignored as in COBOL.
        if (FileStatus.isInvalidKey(acctfileStatus)) {
            wsValidationFailReason = 109;
            wsValidationFailReasonDesc = "ACCOUNT RECORD NOT FOUND";
        }
    }

    // 2900-WRITE-TRANSACTION-FILE
    void writeTransactionFile() {
        applResult = ApplResult.OTHER;
        tranfileStatus = transactFile.write(tranRecord.format());
        checkStatus(tranfileStatus, "ERROR WRITING TO TRANSACTION FILE", tranfileStatus);
    }

    // 9000-DALYTRAN-CLOSE
    void dalytranClose() {
        dalytranStatus = dalytranFile.close();
        checkStatus(dalytranStatus, "ERROR CLOSING DALYTRAN FILE", dalytranStatus);
    }

    // 9100-TRANFILE-CLOSE
    void tranfileClose() {
        tranfileStatus = transactFile.close();
        checkStatus(tranfileStatus, "ERROR CLOSING TRANSACTION FILE", tranfileStatus);
    }

    // 9200-XREFFILE-CLOSE
    void xreffileClose() {
        xreffileStatus = xrefFile.close();
        checkStatus(xreffileStatus, "ERROR CLOSING CROSS REF FILE", xreffileStatus);
    }

    // 9300-DALYREJS-CLOSE (the COBOL displays XREFFILE-STATUS here, not DALYREJS-STATUS)
    void dalyrejsClose() {
        dalyrejsStatus = dalyrejsFile.close();
        checkStatus(dalyrejsStatus, "ERROR CLOSING DAILY REJECTS FILE", xreffileStatus);
    }

    // 9400-ACCTFILE-CLOSE
    void acctfileClose() {
        acctfileStatus = accountFile.close();
        checkStatus(acctfileStatus, "ERROR CLOSING ACCOUNT FILE", acctfileStatus);
    }

    // 9500-TCATBALF-CLOSE
    void tcatbalfClose() {
        tcatbalfStatus = tcatbalFile.close();
        checkStatus(tcatbalfStatus, "ERROR CLOSING TRANSACTION BALANCE FILE", tcatbalfStatus);
    }

    // Z-GET-DB2-FORMAT-TIMESTAMP: FUNCTION CURRENT-DATE -> YYYY-MM-DD-HH.MM.SS.hh0000
    String getDb2FormatTimestamp() {
        LocalDateTime now = LocalDateTime.now(clock);
        return String.format("%04d-%02d-%02d-%02d.%02d.%02d.%02d0000", now.getYear(), now.getMonthValue(),
                now.getDayOfMonth(), now.getHour(), now.getMinute(), now.getSecond(), now.getNano() / 10_000_000);
    }

    // 9999-ABEND-PROGRAM
    void abendProgram(String reason, String status) {
        display("ABENDING PROGRAM");
        throw new Cbtrn02cAbendException(ABCODE, reason, status);
    }

    // 9910-DISPLAY-IO-STATUS
    void displayIoStatus(String ioStatus) {
        display("FILE STATUS IS: NNNN" + formatIoStatus04(ioStatus));
    }

    static String formatIoStatus04(String ioStatus) {
        String s = Pic.alnum(ioStatus, 2);
        char stat1 = s.charAt(0);
        char stat2 = s.charAt(1);
        boolean numeric = Character.isDigit(stat1) && Character.isDigit(stat2)
                && stat1 < 128 && stat2 < 128;
        if (!numeric || stat1 == '9') {
            int twoBytesBinary = stat2 & 0xFF;
            return stat1 + Pic.unsigned(twoBytesBinary, 3);
        }
        return "00" + s;
    }

    /** The common "MOVE 8/0/12 TO APPL-RESULT; IF APPL-AOK ... ELSE DISPLAY/ABEND" tail of each I/O paragraph. */
    private void checkStatus(String status, String errorMessage, String ioStatus) {
        applResult = FileStatus.OK.equals(status) ? ApplResult.AOK : ApplResult.ERROR;
        if (applResult != ApplResult.AOK) {
            display(errorMessage);
            displayIoStatus(ioStatus);
            abendProgram(errorMessage, status);
        }
    }

    private static BigDecimal acctAdd(BigDecimal field, BigDecimal amt) {
        return Pic.truncate(field.add(amt), AccountRecord.AMT_INT_DIGITS, AccountRecord.AMT_SCALE);
    }

    /** ADD 1 TO a PIC 9(09) counter: high-order truncation on overflow. */
    private static long addCounter(long counter) {
        return (counter + 1) % 1_000_000_000L;
    }

    private void display(String line) {
        sysout.accept(line);
    }

    /**
     * Uninitialised WORKING-STORAGE record (GnuCOBOL default): spaces, with the numeric fields given as
     * (offset, length) pairs set to zeros.
     */
    private static String initialRecord(int length, int... numericFields) {
        char[] c = " ".repeat(length).toCharArray();
        for (int i = 0; i < numericFields.length; i += 2) {
            for (int j = 0; j < numericFields[i + 1]; j++) {
                c[numericFields[i] + j] = '0';
            }
        }
        return new String(c);
    }
}
