package com.carddemo.batch.print;

import com.carddemo.common.AbendException;
import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.RecordSink;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.harness.VariableFileSink;
import com.carddemo.common.codec.CobolDisplay;
import com.carddemo.common.codec.Copybook;
import com.carddemo.common.codec.Field;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.common.date.CobDatFt;
import com.carddemo.common.file.FileStatusException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * {@code CBACT01C} (job {@code READACCT}): reads ACCTFILE in key order, {@code DISPLAY}s each account field by field,
 * and writes OUTFILE (reissue date reformatted by {@code COBDATFT}, a zero cycle debit replaced by 2525.00),
 * ARRYFILE (5-occurrence array, 3 populated) and VBRCFILE (a 12- and a 39-byte variable record per account).
 *
 * <p>Working storage persists across records as in the program: OUT-ACCT-REC starts as LOW-VALUES and is not
 * cleared, so a non-zero cycle debit leaves the previous record's value in OUT-ACCT-CURR-CYC-DEBIT; CODATECN-REC
 * keeps its previous output on a conversion error. ARR-ARRAY-REC and VBRC-REC1 are {@code INITIALIZE}d per record.
 */
public final class Cbact01c {

    public static final String PROGRAM = "CBACT01C";
    public static final String ACCTFILE = "ACCTFILE";
    public static final String OUTFILE = "OUTFILE";
    public static final String ARRYFILE = "ARRYFILE";
    public static final String VBRCFILE = "VBRCFILE";
    public static final int VBRC_MIN = 10;
    public static final int VBRC_MAX = 80;

    static final Copybook RECORDS = loadRecords();
    static final RecordLayout OUT_ACCT_REC = RECORDS.record("OUT-ACCT-REC");
    static final RecordLayout ARR_ARRAY_REC = RECORDS.record("ARR-ARRAY-REC");
    static final RecordLayout VBRC_REC1 = RECORDS.record("VBRC-REC1");
    static final RecordLayout VBRC_REC2 = RECORDS.record("VBRC-REC2");

    private static final List<String> DISPLAYED = List.of("ACCT-ID", "ACCT-ACTIVE-STATUS", "ACCT-CURR-BAL",
            "ACCT-CREDIT-LIMIT", "ACCT-CASH-CREDIT-LIMIT", "ACCT-OPEN-DATE", "ACCT-EXPIRAION-DATE",
            "ACCT-REISSUE-DATE", "ACCT-CURR-CYC-CREDIT", "ACCT-CURR-CYC-DEBIT", "ACCT-GROUP-ID");

    private final KsdsInput acctFile;
    private final RecordSink outFile;
    private final RecordSink arryFile;
    private final VariableFileSink vbrcFile;
    private final Sysout sysout;
    private final RecordEncoding encoding;
    private final FixedWidthRecord outAcctRec;
    private final FixedWidthRecord codatecn;
    private final FixedWidthRecord vbrcRec2;
    private long written;

    public Cbact01c(KsdsInput acctFile, RecordSink outFile, RecordSink arryFile, VariableFileSink vbrcFile,
                    Sysout sysout, RecordEncoding encoding) {
        this.acctFile = acctFile;
        this.outFile = outFile;
        this.arryFile = arryFile;
        this.vbrcFile = vbrcFile;
        this.sysout = sysout;
        this.encoding = encoding;
        this.outAcctRec = new FixedWidthRecord(OUT_ACCT_REC, new byte[OUT_ACCT_REC.length()], encoding);
        this.codatecn = FixedWidthRecord.spaces(CobDatFt.layout(), RecordEncoding.ASCII);
        this.vbrcRec2 = FixedWidthRecord.spaces(VBRC_REC2, encoding);
    }

    public ProgramCounts run() {
        sysout.display("START OF EXECUTION OF PROGRAM " + PROGRAM);
        long read = 0;
        boolean ended = false;
        try {
            open(acctFile::open, "ERROR OPENING ACCTFILE", null);
            open(outFile::open, "ERROR OPENING OUTFILE", outFile);
            open(arryFile::open, "ERROR OPENING ARRAYFILE", arryFile);
            open(vbrcFile::open, "ERROR OPENING VBRC FILE", vbrcFile);
            Optional<FixedWidthRecord> account;
            while ((account = getNext()).isPresent()) {
                read++;
                sysout.display(account.get().text());
            }
            try {
                acctFile.close();
            } catch (FileStatusException e) {
                throw sysout.ioAbend("ERROR CLOSING ACCOUNT FILE", e);
            }
            ended = true;
        } finally {
            if (!ended) {
                closeQuietly(outFile);
                closeQuietly(arryFile);
                closeQuietly(vbrcFile);
            }
        }
        closeOutputs(outFile, arryFile, vbrcFile);
        sysout.display("END OF EXECUTION OF PROGRAM " + PROGRAM);
        return new ProgramCounts(read, written);
    }

    /** {@code 1000-ACCTFILE-GET-NEXT}. */
    private Optional<FixedWidthRecord> getNext() {
        Optional<FixedWidthRecord> next;
        try {
            next = acctFile.readNext();
        } catch (FileStatusException e) {
            throw sysout.ioAbend("ERROR READING ACCOUNT FILE", e);
        }
        if (next.isEmpty()) {
            return next;
        }
        FixedWidthRecord account = next.get();
        FixedWidthRecord arrArrayRec = initializedArray();
        displayAccount(account);
        populateAccount(account);
        write(outFile, outAcctRec, "ACCOUNT FILE WRITE STATUS IS:");
        populateArray(account, arrArrayRec);
        write(arryFile, arrArrayRec, "ACCOUNT FILE WRITE STATUS IS:");
        FixedWidthRecord vbrcRec1 = FixedWidthRecord.spaces(VBRC_REC1, encoding);
        vbrcRec1.moveDecimal(VBRC_REC1.field("VB1-ACCT-ID"), BigDecimal.ZERO);
        populateVbrc(account, vbrcRec1);
        writeVariable(vbrcRec1, 12);
        writeVariable(vbrcRec2, 39);
        return next;
    }

    /** {@code 1100-DISPLAY-ACCT-RECORD}. */
    private void displayAccount(FixedWidthRecord account) {
        for (String name : DISPLAYED) {
            sysout.display(String.format("%-24s:", name), CobolDisplay.of(account, name));
        }
        sysout.display("-------------------------------------------------");
    }

    /** {@code 1300-POPUL-ACCT-RECORD}, including {@code CALL 'COBDATFT'} (type 2 YYYY-MM-DD → YYYYMMDD). */
    private void populateAccount(FixedWidthRecord account) {
        moveNumeric(account, "ACCT-ID", outAcctRec, "OUT-ACCT-ID");
        moveText(account, "ACCT-ACTIVE-STATUS", outAcctRec, "OUT-ACCT-ACTIVE-STATUS");
        moveNumeric(account, "ACCT-CURR-BAL", outAcctRec, "OUT-ACCT-CURR-BAL");
        moveNumeric(account, "ACCT-CREDIT-LIMIT", outAcctRec, "OUT-ACCT-CREDIT-LIMIT");
        moveNumeric(account, "ACCT-CASH-CREDIT-LIMIT", outAcctRec, "OUT-ACCT-CASH-CREDIT-LIMIT");
        moveText(account, "ACCT-OPEN-DATE", outAcctRec, "OUT-ACCT-OPEN-DATE");
        moveText(account, "ACCT-EXPIRAION-DATE", outAcctRec, "OUT-ACCT-EXPIRAION-DATE");
        RecordLayout cd = CobDatFt.layout();
        codatecn.moveString(cd.field("CODATECN-INP-DATE"), account.getString("ACCT-REISSUE-DATE"));
        codatecn.moveString(cd.field("CODATECN-TYPE"), "2");
        codatecn.moveString(cd.field("CODATECN-OUTTYPE"), "2");
        CobDatFt.call(codatecn);
        outAcctRec.moveString(OUT_ACCT_REC.field("OUT-ACCT-REISSUE-DATE"),
                codatecn.getString(cd.field("CODATECN-0UT-DATE")));
        moveNumeric(account, "ACCT-CURR-CYC-CREDIT", outAcctRec, "OUT-ACCT-CURR-CYC-CREDIT");
        if (account.getDecimal("ACCT-CURR-CYC-DEBIT").signum() == 0) {
            outAcctRec.moveDecimal(OUT_ACCT_REC.field("OUT-ACCT-CURR-CYC-DEBIT"), new BigDecimal("2525.00"));
        }
        moveText(account, "ACCT-GROUP-ID", outAcctRec, "OUT-ACCT-GROUP-ID");
    }

    /** {@code INITIALIZE ARR-ARRAY-REC}: numeric DISPLAY items to '0' digits, COMP-3 to zero, alphanumerics to spaces. */
    private FixedWidthRecord initializedArray() {
        FixedWidthRecord rec = FixedWidthRecord.spaces(ARR_ARRAY_REC, encoding);
        byte zero = encoding.encode("0")[0];
        rec.fill(ARR_ARRAY_REC.field("ARR-ACCT-ID"), zero);
        for (int i = 1; i <= 5; i++) {
            rec.fill(ARR_ARRAY_REC.field("ARR-ACCT-CURR-BAL").subscript(i), zero);
            rec.moveDecimal(ARR_ARRAY_REC.field("ARR-ACCT-CURR-CYC-DEBIT").subscript(i), BigDecimal.ZERO);
        }
        return rec;
    }

    /** {@code 1400-POPUL-ARRAY-RECORD}. */
    private static void populateArray(FixedWidthRecord account, FixedWidthRecord rec) {
        Field bal = ARR_ARRAY_REC.field("ARR-ACCT-CURR-BAL");
        Field debit = ARR_ARRAY_REC.field("ARR-ACCT-CURR-CYC-DEBIT");
        BigDecimal currBal = account.getDecimal("ACCT-CURR-BAL");
        rec.moveDecimal(ARR_ARRAY_REC.field("ARR-ACCT-ID"), account.getDecimal("ACCT-ID"));
        rec.moveDecimal(bal.subscript(1), currBal);
        rec.moveDecimal(debit.subscript(1), new BigDecimal("1005.00"));
        rec.moveDecimal(bal.subscript(2), currBal);
        rec.moveDecimal(debit.subscript(2), new BigDecimal("1525.00"));
        rec.moveDecimal(bal.subscript(3), new BigDecimal("-1025.00"));
        rec.moveDecimal(debit.subscript(3), new BigDecimal("-2500.00"));
    }

    /** {@code 1500-POPUL-VBRC-RECORD}: WS-ACCT-REISSUE-YYYY is the first 4 bytes of ACCT-REISSUE-DATE. */
    private void populateVbrc(FixedWidthRecord account, FixedWidthRecord vbrcRec1) {
        moveNumeric(account, "ACCT-ID", vbrcRec1, "VB1-ACCT-ID");
        moveNumeric(account, "ACCT-ID", vbrcRec2, "VB2-ACCT-ID");
        moveText(account, "ACCT-ACTIVE-STATUS", vbrcRec1, "VB1-ACCT-ACTIVE-STATUS");
        moveNumeric(account, "ACCT-CURR-BAL", vbrcRec2, "VB2-ACCT-CURR-BAL");
        moveNumeric(account, "ACCT-CREDIT-LIMIT", vbrcRec2, "VB2-ACCT-CREDIT-LIMIT");
        vbrcRec2.moveString(VBRC_REC2.field("VB2-ACCT-REISSUE-YYYY"),
                account.getString("ACCT-REISSUE-DATE").substring(0, 4));
        sysout.display("VBRC-REC1:", vbrcRec1.text());
        sysout.display("VBRC-REC2:", vbrcRec2.text());
    }

    private void write(RecordSink sink, FixedWidthRecord rec, String message) {
        try {
            sink.write(rec.copy());
        } catch (FileStatusException e) {
            throw sysout.ioAbend(message + e.status().code(), e);
        }
        written++;
    }

    /** {@code MOVE rec TO VBR-REC(1:len)} + {@code WRITE VBR-REC}. */
    private void writeVariable(FixedWidthRecord rec, int length) {
        byte[] area = Arrays.copyOf(rec.bytes(), VBRC_MAX);
        try {
            vbrcFile.write(area, length);
        } catch (FileStatusException e) {
            throw sysout.ioAbend("ACCOUNT FILE WRITE STATUS IS:" + e.status().code(), e);
        }
        written++;
    }

    private void open(Runnable open, String message, RecordSink sink) {
        try {
            open.run();
        } catch (FileStatusException e) {
            throw sysout.ioAbend(sink == null ? message : message + e.status().code(), e);
        }
    }

    private static void moveNumeric(FixedWidthRecord from, String fromName, FixedWidthRecord to, String toName) {
        to.moveDecimal(to.field(toName), from.getDecimal(fromName));
    }

    private static void moveText(FixedWidthRecord from, String fromName, FixedWidthRecord to, String toName) {
        to.moveString(to.field(toName), from.getString(fromName));
    }

    /** Implicit CLOSE at GOBACK: buffered records are flushed here, so a failure is an abend, not RC 0. */
    private static void closeOutputs(RecordSink... sinks) {
        AbendException failure = null;
        for (RecordSink sink : sinks) {
            try {
                sink.close();
            } catch (RuntimeException e) {
                if (failure == null) {
                    failure = AbendException.carddemo("ERROR CLOSING " + sink.ddname(), e);
                }
            }
        }
        if (failure != null) {
            throw failure;
        }
    }

    private static void closeQuietly(RecordSink sink) {
        try {
            sink.close();
        } catch (RuntimeException e) {
            // implicit CLOSE at GOBACK after an abend: the abend is what the step reports
        }
    }

    /** The ACCOUNT-RECORD layout read INTO by CBACT01C. */
    public static RecordLayout accountLayout() {
        return AccountRecord.MAPPER.layout();
    }

    private static Copybook loadRecords() {
        try (InputStream in = Cbact01c.class.getClassLoader().getResourceAsStream("layouts/CBACT01C.cpy")) {
            if (in == null) {
                throw new IllegalStateException("layouts/CBACT01C.cpy missing from the classpath");
            }
            return Copybook.parse(PROGRAM, new String(in.readAllBytes(), StandardCharsets.ISO_8859_1));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
