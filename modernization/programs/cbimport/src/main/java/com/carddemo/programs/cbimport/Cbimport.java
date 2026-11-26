package com.carddemo.programs.cbimport;

import com.carddemo.programs.cbimport.copybook.AccountRecord;
import com.carddemo.programs.cbimport.copybook.CardRecord;
import com.carddemo.programs.cbimport.copybook.CardXrefRecord;
import com.carddemo.programs.cbimport.copybook.CustomerRecord;
import com.carddemo.programs.cbimport.copybook.ExportRecord;
import com.carddemo.programs.cbimport.copybook.Pic;
import com.carddemo.programs.cbimport.copybook.TranRecord;
import com.carddemo.programs.cbimport.io.ExportRecordReader;

import java.io.PrintStream;
import java.nio.charset.Charset;
import java.time.Clock;

/**
 * CBIMPORT: imports customer data from the branch migration export file, splitting the multi-record
 * EXPORT-RECORD layout into the normalized CUSTOMER, ACCOUNT, CARD-XREF, TRANSACTION and CARD files
 * and writing unknown record types to the error file.
 */
public final class Cbimport {

    public static final int RETURN_CODE_OK = 0;

    private final CbimportFiles files;
    private final Charset exportCharset;
    private final Clock clock;
    private final PrintStream sysout;

    private final ImportStatistics stats = new ImportStatistics();
    private String wsExportStatus = "  ";
    private ExportRecord exportRecord;
    private String wsImportDate;
    private String wsImportTime;

    public Cbimport(CbimportFiles files, Charset exportCharset, Clock clock, PrintStream sysout) {
        this.files = files;
        this.exportCharset = exportCharset;
        this.clock = clock;
        this.sysout = sysout;
    }

    public ImportStatistics statistics() {
        return stats;
    }

    // 0000-MAIN-PROCESSING
    public int run() {
        initialize();
        processExportFile();
        validateImport();
        finalizeImport();
        return RETURN_CODE_OK;
    }

    // 1000-INITIALIZE
    void initialize() {
        display("CBIMPORT: Starting Customer Data Import");

        String now = CurrentDate.of(clock);
        wsImportDate = now.substring(0, 4) + "-" + now.substring(4, 6) + "-" + now.substring(6, 8);
        wsImportTime = now.substring(8, 10) + ":" + now.substring(10, 12) + ":" + now.substring(12, 14);

        openFiles();

        display("CBIMPORT: Import Date: " + wsImportDate);
        display("CBIMPORT: Import Time: " + wsImportTime);
    }

    // 1100-OPEN-FILES
    void openFiles() {
        wsExportStatus = files.expfile().open();
        checkOpen(wsExportStatus, "EXPORT-INPUT");
        checkOpen(files.custout().open(), "CUSTOMER-OUTPUT");
        checkOpen(files.acctout().open(), "ACCOUNT-OUTPUT");
        checkOpen(files.xrefout().open(), "XREF-OUTPUT");
        checkOpen(files.trnxout().open(), "TRANSACTION-OUTPUT");
        checkOpen(files.cardout().open(), "CARD-OUTPUT");
        checkOpen(files.errout().open(), "ERROR-OUTPUT");
    }

    private void checkOpen(String status, String fileName) {
        if (!FileStatus.isOk(status)) {
            display("ERROR: Cannot open " + fileName + ", Status: " + status);
            abendProgram();
        }
    }

    // 2000-PROCESS-EXPORT-FILE
    void processExportFile() {
        readExportRecord();
        while (!FileStatus.isEof(wsExportStatus)) {
            stats.totalRecordsRead = ImportStatistics.add1(stats.totalRecordsRead);
            processRecordByType();
            readExportRecord();
        }
    }

    // 2100-READ-EXPORT-RECORD: READ EXPORT-INPUT INTO EXPORT-RECORD
    void readExportRecord() {
        ExportRecordReader.ReadResult result = files.expfile().read();
        wsExportStatus = result.status();
        if (FileStatus.isOk(wsExportStatus)) {
            exportRecord = ExportRecord.decode(result.record(), exportCharset);
        }
        if (!FileStatus.isOk(wsExportStatus) && !FileStatus.isEof(wsExportStatus)) {
            display("ERROR: Reading EXPORT-INPUT, Status: " + wsExportStatus);
            abendProgram();
        }
    }

    // 2200-PROCESS-RECORD-BY-TYPE
    void processRecordByType() {
        switch (exportRecord.recType()) {
            case "C" -> processCustomerRecord();
            case "A" -> processAccountRecord();
            case "X" -> processXrefRecord();
            case "T" -> processTranRecord();
            case "D" -> processCardRecord();
            default -> processUnknownRecord();
        }
    }

    // 2300-PROCESS-CUSTOMER-RECORD
    void processCustomerRecord() {
        ExportRecord.CustomerData exp = exportRecord.customerData();
        CustomerRecord customerRecord = new CustomerRecord(
                Pic.unsigned(exp.custId(), 9),
                exp.firstName(),
                exp.middleName(),
                exp.lastName(),
                exp.addrLines().get(0),
                exp.addrLines().get(1),
                exp.addrLines().get(2),
                exp.addrStateCd(),
                exp.addrCountryCd(),
                exp.addrZip(),
                exp.phoneNums().get(0),
                exp.phoneNums().get(1),
                Pic.unsigned(exp.ssn(), 9),
                exp.govtIssuedId(),
                exp.dobYyyyMmDd(),
                exp.eftAccountId(),
                exp.priCardHolderInd(),
                (int) Pic.unsigned(exp.ficoCreditScore(), 3));

        String status = files.custout().write(customerRecord.toFixedWidth());
        if (!FileStatus.isOk(status)) {
            display("ERROR: Writing customer record, Status: " + status);
            abendProgram();
        }
        stats.customerRecordsImported = ImportStatistics.add1(stats.customerRecordsImported);
    }

    // 2400-PROCESS-ACCOUNT-RECORD
    void processAccountRecord() {
        ExportRecord.AccountData exp = exportRecord.accountData();
        AccountRecord accountRecord = new AccountRecord(
                Pic.unsigned(exp.acctId(), 11),
                exp.activeStatus(),
                exp.currBal(),
                exp.creditLimit(),
                exp.cashCreditLimit(),
                exp.openDate(),
                exp.expirationDate(),
                exp.reissueDate(),
                exp.currCycCredit(),
                exp.currCycDebit(),
                exp.addrZip(),
                exp.groupId());

        String status = files.acctout().write(accountRecord.toFixedWidth());
        if (!FileStatus.isOk(status)) {
            display("ERROR: Writing account record, Status: " + status);
            abendProgram();
        }
        stats.accountRecordsImported = ImportStatistics.add1(stats.accountRecordsImported);
    }

    // 2500-PROCESS-XREF-RECORD
    void processXrefRecord() {
        ExportRecord.CardXrefData exp = exportRecord.cardXrefData();
        CardXrefRecord xrefRecord = new CardXrefRecord(
                exp.cardNum(),
                Pic.unsigned(exp.custId(), 9),
                Pic.unsigned(exp.acctId(), 11));

        String status = files.xrefout().write(xrefRecord.toFixedWidth());
        if (!FileStatus.isOk(status)) {
            display("ERROR: Writing xref record, Status: " + status);
            abendProgram();
        }
        stats.xrefRecordsImported = ImportStatistics.add1(stats.xrefRecordsImported);
    }

    // 2600-PROCESS-TRAN-RECORD
    void processTranRecord() {
        ExportRecord.TransactionData exp = exportRecord.transactionData();
        TranRecord tranRecord = new TranRecord(
                exp.tranId(),
                exp.typeCd(),
                (int) Pic.unsigned(exp.catCd(), 4),
                exp.source(),
                exp.desc(),
                exp.amt(),
                Pic.unsigned(exp.merchantId(), 9),
                exp.merchantName(),
                exp.merchantCity(),
                exp.merchantZip(),
                exp.cardNum(),
                exp.origTs(),
                exp.procTs());

        String status = files.trnxout().write(tranRecord.toFixedWidth());
        if (!FileStatus.isOk(status)) {
            display("ERROR: Writing transaction record, Status: " + status);
            abendProgram();
        }
        stats.tranRecordsImported = ImportStatistics.add1(stats.tranRecordsImported);
    }

    // 2650-PROCESS-CARD-RECORD
    void processCardRecord() {
        ExportRecord.CardData exp = exportRecord.cardData();
        CardRecord cardRecord = new CardRecord(
                exp.cardNum(),
                Pic.unsigned(exp.acctId(), 11),
                (int) Pic.unsigned(exp.cvvCd(), 3),
                exp.embossedName(),
                exp.expirationDate(),
                exp.activeStatus());

        String status = files.cardout().write(cardRecord.toFixedWidth());
        if (!FileStatus.isOk(status)) {
            display("ERROR: Writing card record, Status: " + status);
            abendProgram();
        }
        stats.cardRecordsImported = ImportStatistics.add1(stats.cardRecordsImported);
    }

    // 2700-PROCESS-UNKNOWN-RECORD
    void processUnknownRecord() {
        stats.unknownRecordTypeCount = ImportStatistics.add1(stats.unknownRecordTypeCount);

        ErrorRecord errorRecord = new ErrorRecord(
                CurrentDate.of(clock),
                exportRecord.recType(),
                exportRecord.sequenceNum(),
                "Unknown record type encountered");

        writeError(errorRecord);
    }

    // 2750-WRITE-ERROR: a failed write is reported but does not abend, and is still counted
    void writeError(ErrorRecord errorRecord) {
        String status = files.errout().write(errorRecord.toOutputRecord());
        if (!FileStatus.isOk(status)) {
            display("ERROR: Writing error record, Status: " + status);
        }
        stats.errorRecordsWritten = ImportStatistics.add1(stats.errorRecordsWritten);
    }

    // 3000-VALIDATE-IMPORT
    void validateImport() {
        display("CBIMPORT: Import validation completed");
        display("CBIMPORT: No validation errors detected");
    }

    // 4000-FINALIZE (close statuses are not checked, as in the COBOL)
    void finalizeImport() {
        files.expfile().close();
        files.custout().close();
        files.acctout().close();
        files.xrefout().close();
        files.trnxout().close();
        files.cardout().close();
        files.errout().close();

        display("CBIMPORT: Import completed");
        display("CBIMPORT: Total Records Read: " + ImportStatistics.display(stats.totalRecordsRead));
        display("CBIMPORT: Customers Imported: " + ImportStatistics.display(stats.customerRecordsImported));
        display("CBIMPORT: Accounts Imported: " + ImportStatistics.display(stats.accountRecordsImported));
        display("CBIMPORT: XRefs Imported: " + ImportStatistics.display(stats.xrefRecordsImported));
        display("CBIMPORT: Transactions Imported: " + ImportStatistics.display(stats.tranRecordsImported));
        display("CBIMPORT: Cards Imported: " + ImportStatistics.display(stats.cardRecordsImported));
        display("CBIMPORT: Errors Written: " + ImportStatistics.display(stats.errorRecordsWritten));
        display("CBIMPORT: Unknown Record Types: " + ImportStatistics.display(stats.unknownRecordTypeCount));
    }

    // 9999-ABEND-PROGRAM
    void abendProgram() {
        display("CBIMPORT: ABENDING PROGRAM");
        throw new AbendException("CEE3ABD");
    }

    private void display(String line) {
        sysout.println(line);
    }
}
