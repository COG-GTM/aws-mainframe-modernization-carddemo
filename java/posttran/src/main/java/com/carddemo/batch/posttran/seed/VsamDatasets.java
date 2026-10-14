package com.carddemo.batch.posttran.seed;

import com.carddemo.batch.posttran.cobol.CobolCompare;
import com.carddemo.batch.posttran.cobol.FixedRecordFiles;
import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.domain.Account;
import com.carddemo.batch.posttran.domain.CardXref;
import com.carddemo.batch.posttran.domain.TranCatBalance;
import com.carddemo.batch.posttran.domain.TransactionRecord;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * IDCAMS REPRO equivalents for the tables that replace the KSDS files: load them from the shipped
 * seed datasets (app/data/ASCII or app/data/EBCDIC) and unload them as fixed-length records in
 * key sequence (EBCDIC collating order, as VSAM would return them).
 */
@Component
public class VsamDatasets {

    public enum Seed {
        ASCII(RecordFormat.ASCII, "ASCII", "acctdata.txt", "cardxref.txt", "tcatbal.txt", "dailytran.txt"),
        EBCDIC(RecordFormat.EBCDIC, "EBCDIC", "AWS.M2.CARDDEMO.ACCTDATA.PS", "AWS.M2.CARDDEMO.CARDXREF.PS",
                "AWS.M2.CARDDEMO.TCATBALF.PS", "AWS.M2.CARDDEMO.DALYTRAN.PS");

        public final RecordFormat format;
        private final String dir;
        private final String acct;
        private final String xref;
        private final String tcatbal;
        private final String dalytran;

        Seed(RecordFormat format, String dir, String acct, String xref, String tcatbal, String dalytran) {
            this.format = format;
            this.dir = dir;
            this.acct = acct;
            this.xref = xref;
            this.tcatbal = tcatbal;
            this.dalytran = dalytran;
        }

        public Path dalytran(Path dataDir) {
            return dataDir.resolve(dir).resolve(dalytran);
        }
    }

    @PersistenceContext
    private EntityManager em;

    /** Replaces ACCOUNT, CARD_XREF and TRAN_CAT_BALANCE with the seed data; TRANSACT starts empty. */
    @Transactional
    public void load(Path dataDir, Seed seed) {
        for (Class<?> c : List.of(TransactionRecord.class, TranCatBalance.class, CardXref.class, Account.class)) {
            em.createQuery("delete from " + c.getSimpleName()).executeUpdate();
        }
        Path dir = dataDir.resolve(seed.dir);
        FixedRecordFiles.read(dir.resolve(seed.acct), seed.format, Account.LENGTH)
                .forEach(r -> em.persist(Account.fromRecord(r)));
        FixedRecordFiles.read(dir.resolve(seed.xref), seed.format, CardXref.LENGTH)
                .forEach(r -> em.persist(CardXref.fromRecord(r)));
        FixedRecordFiles.read(dir.resolve(seed.tcatbal), seed.format, TranCatBalance.LENGTH)
                .forEach(r -> em.persist(TranCatBalance.fromRecord(r)));
    }

    @Transactional(readOnly = true)
    public List<String> unloadTransactions() {
        return unload(TransactionRecord.class, TransactionRecord::toRecord, 16);
    }

    @Transactional(readOnly = true)
    public List<String> unloadAccounts() {
        return unload(Account.class, Account::toRecord, 11);
    }

    @Transactional(readOnly = true)
    public List<String> unloadTranCatBalances() {
        return unload(TranCatBalance.class, TranCatBalance::toRecord, 17);
    }

    @Transactional(readOnly = true)
    public void unloadAll(Path outDir, RecordFormat format) {
        FixedRecordFiles.write(outDir.resolve("transact.dat"), format, TransactionRecord.LENGTH, unloadTransactions());
        FixedRecordFiles.write(outDir.resolve("acctdata.dat"), format, Account.LENGTH, unloadAccounts());
        FixedRecordFiles.write(outDir.resolve("tcatbal.dat"), format, TranCatBalance.LENGTH, unloadTranCatBalances());
    }

    private <T> List<String> unload(Class<T> type, Function<T, String> toRecord, int keyLength) {
        return em.createQuery("select e from " + type.getSimpleName() + " e", type).getResultList().stream()
                .map(toRecord)
                .sorted((a, b) -> CobolCompare.compareAlphanumeric(a.substring(0, keyLength), b.substring(0, keyLength)))
                .toList();
    }
}
