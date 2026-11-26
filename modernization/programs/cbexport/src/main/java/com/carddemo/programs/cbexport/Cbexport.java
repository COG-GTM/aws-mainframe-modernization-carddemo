package com.carddemo.programs.cbexport;

import com.carddemo.programs.cbexport.copybook.AccountRecord;
import com.carddemo.programs.cbexport.copybook.CardRecord;
import com.carddemo.programs.cbexport.copybook.CardXrefRecord;
import com.carddemo.programs.cbexport.copybook.CustomerRecord;
import com.carddemo.programs.cbexport.copybook.ExportAccountData;
import com.carddemo.programs.cbexport.copybook.ExportCardData;
import com.carddemo.programs.cbexport.copybook.ExportCardXrefData;
import com.carddemo.programs.cbexport.copybook.ExportCustomerData;
import com.carddemo.programs.cbexport.copybook.ExportRecord;
import com.carddemo.programs.cbexport.copybook.ExportRecordType;
import com.carddemo.programs.cbexport.copybook.ExportTransactionData;
import com.carddemo.programs.cbexport.copybook.TranRecord;
import com.carddemo.programs.cbexport.io.FileStatus;
import com.carddemo.programs.cbexport.io.ReadResult;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.List;
import java.util.function.Consumer;

/**
 * CBEXPORT - Export Customer Data for Branch Migration.
 *
 * <p>Reads the five normalised CardDemo KSDS files sequentially and writes one 500-byte
 * multi-record export file (CVEXPORT) in the fixed order customers (C), accounts (A),
 * cross-references (X), transactions (T), cards (D), each record stamped with a run timestamp, an
 * ascending sequence number, branch '0001' and region 'NORTH'.
 */
public final class Cbexport {

    public static final String BRANCH_ID = "0001";
    public static final String REGION_CODE = "NORTH";

    private static final long PIC_9_09_MODULUS = 1_000_000_000L;

    private final CbexportFiles files;
    private final Clock clock;
    private final Consumer<String> sysout;

    // WS-FILE-STATUS-AREA
    private String wsCustomerStatus;
    private String wsAccountStatus;
    private String wsXrefStatus;
    private String wsTransactionStatus;
    private String wsCardStatus;
    private String wsExportStatus;

    // WS-EXPORT-CONTROL
    private String wsExportDate = " ".repeat(10);
    private String wsExportTime = " ".repeat(8);
    private String wsFormattedTimestamp = " ".repeat(26);
    private long wsSequenceCounter;

    // WS-EXPORT-STATISTICS
    private long wsCustomerRecordsExported;
    private long wsAccountRecordsExported;
    private long wsXrefRecordsExported;
    private long wsTranRecordsExported;
    private long wsCardRecordsExported;
    private long wsTotalRecordsExported;

    // FD record areas
    private CustomerRecord customerRecord;
    private AccountRecord accountRecord;
    private CardXrefRecord cardXrefRecord;
    private TranRecord tranRecord;
    private CardRecord cardRecord;

    // CVEXPORT (WORKING-STORAGE)
    private final ExportRecord exportRecord = new ExportRecord();

    public Cbexport(CbexportFiles files, Clock clock, Consumer<String> sysout) {
        this.files = files;
        this.clock = clock;
        this.sysout = sysout;
    }

    /** 0000-MAIN-PROCESSING. Throws {@link CbexportAbendException} where COBOL calls CEE3ABD. */
    public ExportStatistics run() {
        resetWorkingStorage();
        try {
            initialize();
            exportCustomers();
            exportAccounts();
            exportXrefs();
            exportTransactions();
            exportCards();
        } catch (CbexportAbendException e) {
            // CEE3ABD ends the enclave and the runtime closes open files; do the same for callers.
            files.closeAll();
            throw e;
        }
        finalizeExport();
        return statistics();
    }

    // Re-applies the WORKING-STORAGE VALUE clauses so a reused instance starts like a fresh load module.
    private void resetWorkingStorage() {
        wsExportDate = " ".repeat(10);
        wsExportTime = " ".repeat(8);
        wsFormattedTimestamp = " ".repeat(26);
        wsSequenceCounter = 0;
        wsCustomerRecordsExported = 0;
        wsAccountRecordsExported = 0;
        wsXrefRecordsExported = 0;
        wsTranRecordsExported = 0;
        wsCardRecordsExported = 0;
        wsTotalRecordsExported = 0;
    }

    public ExportStatistics statistics() {
        return new ExportStatistics(wsCustomerRecordsExported, wsAccountRecordsExported,
                wsXrefRecordsExported, wsTranRecordsExported, wsCardRecordsExported, wsTotalRecordsExported);
    }

    public String formattedTimestamp() {
        return wsFormattedTimestamp;
    }

    // 1000-INITIALIZE
    void initialize() {
        display("CBEXPORT: Starting Customer Data Export");
        generateTimestamp();
        openFiles();
        display("CBEXPORT: Export Date: " + wsExportDate);
        display("CBEXPORT: Export Time: " + wsExportTime);
    }

    // 1050-GENERATE-TIMESTAMP
    void generateTimestamp() {
        // ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD / ACCEPT WS-CURRENT-TIME FROM TIME
        LocalDateTime now = LocalDateTime.now(clock);
        String year = pic9(now.getYear(), 4);
        String month = pic9(now.getMonthValue(), 2);
        String day = pic9(now.getDayOfMonth(), 2);
        String hour = pic9(now.getHour(), 2);
        String minute = pic9(now.getMinute(), 2);
        String second = pic9(now.getSecond(), 2);

        wsExportDate = stringInto(wsExportDate, year + "-" + month + "-" + day);
        wsExportTime = stringInto(wsExportTime, hour + ":" + minute + ":" + second);
        // Hundredths are not used: the literal '.00' is appended.
        wsFormattedTimestamp = stringInto(wsFormattedTimestamp, wsExportDate + " " + wsExportTime + ".00");
    }

    // 1100-OPEN-FILES
    void openFiles() {
        wsCustomerStatus = files.custFile().open();
        if (!FileStatus.isOk(wsCustomerStatus)) {
            abend("ERROR: Cannot open CUSTOMER-INPUT, Status: ", wsCustomerStatus);
        }
        wsAccountStatus = files.acctFile().open();
        if (!FileStatus.isOk(wsAccountStatus)) {
            abend("ERROR: Cannot open ACCOUNT-INPUT, Status: ", wsAccountStatus);
        }
        wsXrefStatus = files.xrefFile().open();
        if (!FileStatus.isOk(wsXrefStatus)) {
            abend("ERROR: Cannot open XREF-INPUT, Status: ", wsXrefStatus);
        }
        wsTransactionStatus = files.transact().open();
        if (!FileStatus.isOk(wsTransactionStatus)) {
            abend("ERROR: Cannot open TRANSACTION-INPUT, Status: ", wsTransactionStatus);
        }
        wsCardStatus = files.cardFile().open();
        if (!FileStatus.isOk(wsCardStatus)) {
            abend("ERROR: Cannot open CARD-INPUT, Status: ", wsCardStatus);
        }
        wsExportStatus = files.expFile().open();
        if (!FileStatus.isOk(wsExportStatus)) {
            abend("ERROR: Cannot open EXPORT-OUTPUT, Status: ", wsExportStatus);
        }
    }

    // 2000-EXPORT-CUSTOMERS
    void exportCustomers() {
        display("CBEXPORT: Processing customer records");
        readCustomerRecord();
        while (!FileStatus.isEof(wsCustomerStatus)) {
            createCustomerExpRec();
            readCustomerRecord();
        }
        display("CBEXPORT: Customers exported: " + pic9(wsCustomerRecordsExported, 9));
    }

    // 2100-READ-CUSTOMER-RECORD
    void readCustomerRecord() {
        ReadResult r = files.custFile().read();
        wsCustomerStatus = r.status();
        if (!FileStatus.isOk(wsCustomerStatus) && !FileStatus.isEof(wsCustomerStatus)) {
            abend("ERROR: Reading CUSTOMER-INPUT, Status: ", wsCustomerStatus);
        }
        if (FileStatus.isOk(wsCustomerStatus)) {
            customerRecord = CustomerRecord.parse(r.record());
        }
    }

    // 2200-CREATE-CUSTOMER-EXP-REC
    void createCustomerExpRec() {
        setCommonFields(ExportRecordType.CUSTOMER);
        CustomerRecord c = customerRecord;
        exportRecord.setCustomerData(new ExportCustomerData(
                c.custId(), c.firstName(), c.middleName(), c.lastName(),
                List.of(c.addrLine1(), c.addrLine2(), c.addrLine3()),
                c.addrStateCd(), c.addrCountryCd(), c.addrZip(),
                List.of(c.phoneNum1(), c.phoneNum2()),
                c.ssn(), c.govtIssuedId(), c.dobYyyyMmDd(), c.eftAccountId(),
                c.priCardHolderInd(), c.ficoCreditScore()));
        writeExportRecord();
        wsCustomerRecordsExported = addOne(wsCustomerRecordsExported);
        wsTotalRecordsExported = addOne(wsTotalRecordsExported);
    }

    // 3000-EXPORT-ACCOUNTS
    void exportAccounts() {
        display("CBEXPORT: Processing account records");
        readAccountRecord();
        while (!FileStatus.isEof(wsAccountStatus)) {
            createAccountExpRec();
            readAccountRecord();
        }
        display("CBEXPORT: Accounts exported: " + pic9(wsAccountRecordsExported, 9));
    }

    // 3100-READ-ACCOUNT-RECORD
    void readAccountRecord() {
        ReadResult r = files.acctFile().read();
        wsAccountStatus = r.status();
        if (!FileStatus.isOk(wsAccountStatus) && !FileStatus.isEof(wsAccountStatus)) {
            abend("ERROR: Reading ACCOUNT-INPUT, Status: ", wsAccountStatus);
        }
        if (FileStatus.isOk(wsAccountStatus)) {
            accountRecord = AccountRecord.parse(r.record());
        }
    }

    // 3200-CREATE-ACCOUNT-EXP-REC
    void createAccountExpRec() {
        setCommonFields(ExportRecordType.ACCOUNT);
        AccountRecord a = accountRecord;
        exportRecord.setAccountData(new ExportAccountData(
                a.acctId(), a.activeStatus(), a.currBal(), a.creditLimit(), a.cashCreditLimit(),
                a.openDate(), a.expiraionDate(), a.reissueDate(),
                a.currCycCredit(), a.currCycDebit(), a.addrZip(), a.groupId()));
        writeExportRecord();
        wsAccountRecordsExported = addOne(wsAccountRecordsExported);
        wsTotalRecordsExported = addOne(wsTotalRecordsExported);
    }

    // 4000-EXPORT-XREFS
    void exportXrefs() {
        display("CBEXPORT: Processing cross-reference records");
        readXrefRecord();
        while (!FileStatus.isEof(wsXrefStatus)) {
            createXrefExportRecord();
            readXrefRecord();
        }
        display("CBEXPORT: Cross-references exported: " + pic9(wsXrefRecordsExported, 9));
    }

    // 4100-READ-XREF-RECORD
    void readXrefRecord() {
        ReadResult r = files.xrefFile().read();
        wsXrefStatus = r.status();
        if (!FileStatus.isOk(wsXrefStatus) && !FileStatus.isEof(wsXrefStatus)) {
            abend("ERROR: Reading XREF-INPUT, Status: ", wsXrefStatus);
        }
        if (FileStatus.isOk(wsXrefStatus)) {
            cardXrefRecord = CardXrefRecord.parse(r.record());
        }
    }

    // 4200-CREATE-XREF-EXPORT-RECORD
    void createXrefExportRecord() {
        setCommonFields(ExportRecordType.XREF);
        CardXrefRecord x = cardXrefRecord;
        exportRecord.setCardXrefData(new ExportCardXrefData(x.cardNum(), x.custId(), x.acctId()));
        writeExportRecord();
        wsXrefRecordsExported = addOne(wsXrefRecordsExported);
        wsTotalRecordsExported = addOne(wsTotalRecordsExported);
    }

    // 5000-EXPORT-TRANSACTIONS
    void exportTransactions() {
        display("CBEXPORT: Processing transaction records");
        readTransactionRecord();
        while (!FileStatus.isEof(wsTransactionStatus)) {
            createTranExpRec();
            readTransactionRecord();
        }
        display("CBEXPORT: Transactions exported: " + pic9(wsTranRecordsExported, 9));
    }

    // 5100-READ-TRANSACTION-RECORD
    void readTransactionRecord() {
        ReadResult r = files.transact().read();
        wsTransactionStatus = r.status();
        if (!FileStatus.isOk(wsTransactionStatus) && !FileStatus.isEof(wsTransactionStatus)) {
            abend("ERROR: Reading TRANSACTION-INPUT, Status: ", wsTransactionStatus);
        }
        if (FileStatus.isOk(wsTransactionStatus)) {
            tranRecord = TranRecord.parse(r.record());
        }
    }

    // 5200-CREATE-TRAN-EXP-REC
    void createTranExpRec() {
        setCommonFields(ExportRecordType.TRANSACTION);
        TranRecord t = tranRecord;
        exportRecord.setTransactionData(new ExportTransactionData(
                t.tranId(), t.typeCd(), t.catCd(), t.source(), t.desc(), t.amt(), t.merchantId(),
                t.merchantName(), t.merchantCity(), t.merchantZip(), t.cardNum(), t.origTs(), t.procTs()));
        writeExportRecord();
        wsTranRecordsExported = addOne(wsTranRecordsExported);
        wsTotalRecordsExported = addOne(wsTotalRecordsExported);
    }

    // 5500-EXPORT-CARDS
    void exportCards() {
        display("CBEXPORT: Processing card records");
        readCardRecord();
        while (!FileStatus.isEof(wsCardStatus)) {
            createCardExportRecord();
            readCardRecord();
        }
        display("CBEXPORT: Cards exported: " + pic9(wsCardRecordsExported, 9));
    }

    // 5600-READ-CARD-RECORD
    void readCardRecord() {
        ReadResult r = files.cardFile().read();
        wsCardStatus = r.status();
        if (!FileStatus.isOk(wsCardStatus) && !FileStatus.isEof(wsCardStatus)) {
            abend("ERROR: Reading CARD-INPUT, Status: ", wsCardStatus);
        }
        if (FileStatus.isOk(wsCardStatus)) {
            cardRecord = CardRecord.parse(r.record());
        }
    }

    // 5700-CREATE-CARD-EXPORT-RECORD
    void createCardExportRecord() {
        setCommonFields(ExportRecordType.CARD);
        CardRecord c = cardRecord;
        exportRecord.setCardData(new ExportCardData(
                c.cardNum(), c.acctId(), c.cvvCd(), c.embossedName(), c.expiraionDate(), c.activeStatus()));
        writeExportRecord();
        wsCardRecordsExported = addOne(wsCardRecordsExported);
        wsTotalRecordsExported = addOne(wsTotalRecordsExported);
    }

    // 6000-FINALIZE
    void finalizeExport() {
        files.custFile().close();
        files.acctFile().close();
        files.xrefFile().close();
        files.transact().close();
        files.cardFile().close();
        files.expFile().close();

        display("CBEXPORT: Export completed");
        display("CBEXPORT: Customers Exported: " + pic9(wsCustomerRecordsExported, 9));
        display("CBEXPORT: Accounts Exported: " + pic9(wsAccountRecordsExported, 9));
        display("CBEXPORT: XRefs Exported: " + pic9(wsXrefRecordsExported, 9));
        display("CBEXPORT: Transactions Exported: " + pic9(wsTranRecordsExported, 9));
        display("CBEXPORT: Cards Exported: " + pic9(wsCardRecordsExported, 9));
        display("CBEXPORT: Total Records Exported: " + pic9(wsTotalRecordsExported, 9));
    }

    // 9999-ABEND-PROGRAM
    private void abend(String errorText, String status) {
        String error = errorText + status;
        display(error);
        display("CBEXPORT: ABENDING PROGRAM");
        throw new CbexportAbendException(error, status);
    }

    // Common prologue of 2200/3200/4200/5200/5700: INITIALIZE, header fields, ADD 1 TO sequence.
    private void setCommonFields(ExportRecordType type) {
        exportRecord.initialize();
        exportRecord.setRecType(type.code());
        exportRecord.setTimestamp(wsFormattedTimestamp);
        wsSequenceCounter = addOne(wsSequenceCounter);
        exportRecord.setSequenceNum(wsSequenceCounter);
        exportRecord.setBranchId(BRANCH_ID);
        exportRecord.setRegionCode(REGION_CODE);
    }

    // Common epilogue: WRITE EXPORT-OUTPUT-RECORD FROM EXPORT-RECORD + status check.
    private void writeExportRecord() {
        wsExportStatus = files.expFile().write(exportRecord.toBytes());
        if (!FileStatus.isOk(wsExportStatus)) {
            abend("ERROR: Writing export record, Status: ", wsExportStatus);
        }
    }

    private void display(String line) {
        sysout.accept(line);
    }

    /** ADD 1 TO a PIC 9(09) item with no ON SIZE ERROR: the high-order digit is truncated. */
    static long addOne(long pic9of9) {
        return (pic9of9 + 1) % PIC_9_09_MODULUS;
    }

    private static String pic9(long value, int digits) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > digits) {
            return s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    /**
     * STRING ... DELIMITED BY SIZE INTO target: characters are transferred left to right until the
     * receiver is full; positions not reached keep their previous content (no space fill).
     */
    static String stringInto(String target, String source) {
        if (source.length() >= target.length()) {
            return source.substring(0, target.length());
        }
        return source + target.substring(source.length());
    }
}
