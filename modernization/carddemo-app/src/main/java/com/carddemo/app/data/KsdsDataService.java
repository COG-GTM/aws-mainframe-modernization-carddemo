package com.carddemo.app.data;

import com.carddemo.account.domain.AccountRecords;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.card.domain.CardXrefRecords;
import com.carddemo.card.repository.CardXrefRepository;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.Copybooks;
import com.carddemo.common.record.RecordFiles;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.reference.domain.TranCatBalanceRecords;
import com.carddemo.reference.repository.TranCatBalanceRepository;
import com.carddemo.transaction.domain.TransactionRecords;
import com.carddemo.transaction.repository.TransactionRepository;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.file.Path;
import java.util.List;

/**
 * Loads and unloads the four POSTTRAN KSDS tables as copybook records (D-021): the seed for local runs and golden cases,
 * and the unload that the parity harness compares with the oracle's {@code *.unl}. Unloads are in KSDS key order.
 */
@Service
public class KsdsDataService {

    /** The KSDS files and their line-sequential sample names under {@code app/data/ASCII}. */
    public enum Ksds {
        ACCTDATA("acctdata.txt"), CARDXREF("cardxref.txt"), TCATBALF("tcatbal.txt"), TRANSACT("transact.txt");

        private final String sampleFile;

        Ksds(String sampleFile) {
            this.sampleFile = sampleFile;
        }

        public String sampleFile() {
            return sampleFile;
        }

        public RecordLayout layout() {
            return switch (this) {
                case ACCTDATA -> Copybooks.CVACT01Y;
                case CARDXREF -> Copybooks.CVACT03Y;
                case TCATBALF -> Copybooks.CVTRA01Y;
                case TRANSACT -> Copybooks.CVTRA05Y;
            };
        }

        public int recordLength() {
            return layout().length();
        }
    }

    private final AccountRepository accounts;
    private final CardXrefRepository xrefs;
    private final TranCatBalanceRepository balances;
    private final TransactionRepository transactions;

    public KsdsDataService(AccountRepository accounts, CardXrefRepository xrefs,
                           TranCatBalanceRepository balances, TransactionRepository transactions) {
        this.accounts = accounts;
        this.xrefs = xrefs;
        this.balances = balances;
        this.transactions = transactions;
    }

    /** Loads ACCTDATA, CARDXREF and TCATBALF from {@code dir/{acctdata,cardxref,tcatbal}.txt} (POSTTRAN inputs). */
    @Transactional
    public void loadSampleData(Path dir) {
        for (Ksds ksds : List.of(Ksds.ACCTDATA, Ksds.CARDXREF, Ksds.TCATBALF)) {
            load(ksds, RecordFiles.readLines(dir.resolve(ksds.sampleFile()), ksds.recordLength(), CobolCharsets.ASCII));
        }
    }

    /** Inserts fixed-width records; a duplicate key fails like a KSDS WRITE with status 22. */
    @Transactional
    public void load(Ksds ksds, List<String> records) {
        switch (ksds) {
            case ACCTDATA -> accounts.saveAllAndFlush(records.stream().map(AccountRecords::fromRecord).toList());
            case CARDXREF -> xrefs.saveAllAndFlush(records.stream().map(CardXrefRecords::fromRecord).toList());
            case TCATBALF -> balances.saveAllAndFlush(records.stream().map(TranCatBalanceRecords::fromRecord).toList());
            case TRANSACT -> transactions.saveAllAndFlush(records.stream().map(TransactionRecords::fromRecord).toList());
        }
    }

    /** All rows as fixed-width records in KSDS key order. */
    @Transactional(readOnly = true)
    public List<String> unload(Ksds ksds) {
        return switch (ksds) {
            case ACCTDATA -> accounts.findAll(Sort.by("acctId")).stream().map(AccountRecords::toRecord).toList();
            case CARDXREF -> xrefs.findAll(Sort.by("cardNum")).stream().map(CardXrefRecords::toRecord).toList();
            case TCATBALF -> balances.findAll(Sort.by("acctId", "typeCd", "catCd")).stream()
                    .map(TranCatBalanceRecords::toRecord).toList();
            case TRANSACT -> transactions.findAll(Sort.by("tranId")).stream().map(TransactionRecords::toRecord).toList();
        };
    }

    /** Empties the four tables (test and local-run reset; never used by a job). */
    @Transactional
    public void deleteAll() {
        transactions.deleteAllInBatch();
        balances.deleteAllInBatch();
        xrefs.deleteAllInBatch();
        accounts.deleteAllInBatch();
    }
}
