package com.carddemo.programs.cbstm03b;

import com.carddemo.programs.cbstm03b.copybook.FdAcctFileRec;
import com.carddemo.programs.cbstm03b.copybook.FdCustFileRec;
import com.carddemo.programs.cbstm03b.copybook.FdTrnxFileRec;
import com.carddemo.programs.cbstm03b.copybook.FdXrefFileRec;
import com.carddemo.programs.cbstm03b.copybook.FileStatus;
import com.carddemo.programs.cbstm03b.copybook.LkM03bArea;
import com.carddemo.programs.cbstm03b.io.FixedWidthRecordReader;
import com.carddemo.programs.cbstm03b.io.InMemoryIndexedFile;
import com.carddemo.programs.cbstm03b.io.IndexedFile;
import com.carddemo.programs.cbstm03b.io.ReadResult;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;

/**
 * CBSTM03B: file-access subroutine CALLed by CBSTM03A (job CREASTMT step STEP040).
 * One instance = one loaded copy of the subroutine; file states and file-status
 * WORKING-STORAGE persist across {@link #call} invocations, as in COBOL.
 */
public class Cbstm03b {

    public static final String DD_TRNXFILE = "TRNXFILE";
    public static final String DD_XREFFILE = "XREFFILE";
    public static final String DD_CUSTFILE = "CUSTFILE";
    public static final String DD_ACCTFILE = "ACCTFILE";

    private final IndexedFile trnxFile;
    private final IndexedFile xrefFile;
    private final IndexedFile custFile;
    private final IndexedFile acctFile;

    private FileStatus trnxfileStatus = FileStatus.INITIAL;
    private FileStatus xreffileStatus = FileStatus.INITIAL;
    private FileStatus custfileStatus = FileStatus.INITIAL;
    private FileStatus acctfileStatus = FileStatus.INITIAL;

    public Cbstm03b(IndexedFile trnxFile, IndexedFile xrefFile, IndexedFile custFile, IndexedFile acctFile) {
        this.trnxFile = Objects.requireNonNull(trnxFile);
        this.xrefFile = Objects.requireNonNull(xrefFile);
        this.custFile = Objects.requireNonNull(custFile);
        this.acctFile = Objects.requireNonNull(acctFile);
    }

    /**
     * Binds the four DDs of CREASTMT STEP040 to line-delimited fixed-width datasets.
     * A null or non-existent path behaves like an unallocated DD (OPEN status 35).
     */
    public static Cbstm03b fromDatasets(Path trnxfile, Path xreffile, Path custfile, Path acctfile)
            throws IOException {
        return new Cbstm03b(
                load(DD_TRNXFILE, trnxfile, FdTrnxFileRec.RECORD_LENGTH, FdTrnxFileRec.KEY_OFFSET, FdTrnxFileRec.KEY_LENGTH),
                load(DD_XREFFILE, xreffile, FdXrefFileRec.RECORD_LENGTH, FdXrefFileRec.KEY_OFFSET, FdXrefFileRec.KEY_LENGTH),
                load(DD_CUSTFILE, custfile, FdCustFileRec.RECORD_LENGTH, FdCustFileRec.KEY_OFFSET, FdCustFileRec.KEY_LENGTH),
                load(DD_ACCTFILE, acctfile, FdAcctFileRec.RECORD_LENGTH, FdAcctFileRec.KEY_OFFSET, FdAcctFileRec.KEY_LENGTH));
    }

    private static InMemoryIndexedFile load(String dd, Path path, int lrecl, int keyOffset, int keyLength)
            throws IOException {
        if (path == null || !Files.exists(path)) {
            return InMemoryIndexedFile.missing(dd, lrecl, keyOffset, keyLength);
        }
        return InMemoryIndexedFile.of(dd, lrecl, keyOffset, keyLength,
                new FixedWidthRecordReader(lrecl).readAll(path));
    }

    /** PROCEDURE DIVISION USING LK-M03B-AREA. */
    public void call(LkM03bArea lk) {
        start0000(lk);
    }

    // 0000-START
    private void start0000(LkM03bArea lk) {
        switch (lk.dd()) {
            case DD_TRNXFILE -> trnxfileProc1000(lk);
            case DD_XREFFILE -> xreffileProc2000(lk);
            case DD_CUSTFILE -> custfileProc3000(lk);
            case DD_ACCTFILE -> acctfileProc4000(lk);
            default -> {
                // WHEN OTHER: GO TO 9999-GOBACK
            }
        }
        // 9999-GOBACK: GOBACK
    }

    // 1000-TRNXFILE-PROC THRU 1999-EXIT
    private void trnxfileProc1000(LkM03bArea lk) {
        if (lk.isM03bOpen()) {
            trnxfileStatus = trnxFile.open();
        } else if (lk.isM03bRead()) {
            trnxfileStatus = readInto(trnxFile.readNext(), lk);
        } else if (lk.isM03bClose()) {
            trnxfileStatus = trnxFile.close();
        }
        // 1900-EXIT
        lk.rc(trnxfileStatus.code());
    }

    // 2000-XREFFILE-PROC THRU 2999-EXIT
    private void xreffileProc2000(LkM03bArea lk) {
        if (lk.isM03bOpen()) {
            xreffileStatus = xrefFile.open();
        } else if (lk.isM03bRead()) {
            xreffileStatus = readInto(xrefFile.readNext(), lk);
        } else if (lk.isM03bClose()) {
            xreffileStatus = xrefFile.close();
        }
        // 2900-EXIT
        lk.rc(xreffileStatus.code());
    }

    // 3000-CUSTFILE-PROC THRU 3999-EXIT
    private void custfileProc3000(LkM03bArea lk) {
        if (lk.isM03bOpen()) {
            custfileStatus = custFile.open();
        } else if (lk.isM03bReadK()) {
            String fdCustId = FdCustFileRec.toKey(keyRefMod(lk));
            custfileStatus = readInto(custFile.readByKey(fdCustId), lk);
        } else if (lk.isM03bClose()) {
            custfileStatus = custFile.close();
        }
        // 3900-EXIT
        lk.rc(custfileStatus.code());
    }

    // 4000-ACCTFILE-PROC THRU 4999-EXIT
    private void acctfileProc4000(LkM03bArea lk) {
        if (lk.isM03bOpen()) {
            acctfileStatus = acctFile.open();
        } else if (lk.isM03bReadK()) {
            String fdAcctId = FdAcctFileRec.toKey(keyRefMod(lk));
            acctfileStatus = readInto(acctFile.readByKey(fdAcctId), lk);
        } else if (lk.isM03bClose()) {
            acctfileStatus = acctFile.close();
        }
        // 4900-EXIT
        lk.rc(acctfileStatus.code());
    }

    /** READ ... INTO LK-M03B-FLDT: the INTO move happens only when the READ succeeds. */
    private static FileStatus readInto(ReadResult result, LkM03bArea lk) {
        if (result.status().isSuccessful() && result.record() != null) {
            lk.fldt(result.record());
        }
        return result.status();
    }

    /** LK-M03B-KEY (1:LK-M03B-KEY-LN). An out-of-range length raises EC-BOUND-REF-MOD. */
    private static String keyRefMod(LkM03bArea lk) {
        int len = lk.keyLn();
        if (len < 1 || len > LkM03bArea.KEY_LEN) {
            throw new Cbstm03bAbend("EC-BOUND-REF-MOD: LK-M03B-KEY (1:" + len + ") outside 1.."
                    + LkM03bArea.KEY_LEN + " (DD " + lk.dd().trim() + ")");
        }
        return lk.key().substring(0, len);
    }

    FileStatus trnxfileStatus() {
        return trnxfileStatus;
    }

    FileStatus xreffileStatus() {
        return xreffileStatus;
    }

    FileStatus custfileStatus() {
        return custfileStatus;
    }

    FileStatus acctfileStatus() {
        return acctfileStatus;
    }
}
