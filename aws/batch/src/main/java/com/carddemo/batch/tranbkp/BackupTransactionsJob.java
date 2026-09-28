package com.carddemo.batch.tranbkp;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.refdata.TableSpec;
import com.carddemo.batch.storage.ObjectStore;
import java.util.Map;
import org.springframework.stereotype.Component;

/**
 * {@code TRANBKP.jcl}: STEP05R {@code REPROC} unload of {@code TRANSACT} → {@code backup/transaction/…}. The
 * STEP05/STEP10 IDCAMS delete/define of the VSAM cluster and AIX are not needed (the table persists).
 */
@Component
public class BackupTransactionsJob implements CardDemoJob {

    private final TableBackup backup;
    private final ObjectStore store;

    public BackupTransactionsJob(TableBackup backup, ObjectStore store) {
        this.backup = backup;
        this.store = store;
    }

    @Override
    public String name() {
        return "backup-transactions";
    }

    @Override
    public JobOutcome run(JobParams p) {
        TableBackup.Result r = backup.backup(TableSpec.TRANSACTION, p.businessDate(), p.runId());
        return JobOutcome.ok(Map.of("rows", r.rows(), "output", store.uri(r.key())));
    }
}
