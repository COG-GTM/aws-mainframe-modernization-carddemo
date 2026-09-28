package com.carddemo.batch.refdata;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobFailure;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.tranbkp.TableBackup;
import java.util.Map;
import org.springframework.stereotype.Component;

/** {@code DEFGDGD.jcl} / {@code TRANEXTR.jcl} STEP10/20 IEBGENER backups → {@code backup/<table>/…}. */
@Component
public class BackupReferenceDataJob implements CardDemoJob {

    private final TableBackup backup;
    private final ObjectStore store;

    public BackupReferenceDataJob(TableBackup backup, ObjectStore store) {
        this.backup = backup;
        this.store = store;
    }

    @Override
    public String name() {
        return "backup-reference-data";
    }

    @Override
    public JobOutcome run(JobParams p) {
        TableSpec spec = TableSpec.of(p.require("table")).orElseThrow(() -> new JobFailure(ReturnCode.INPUT_ERROR,
                "Unsupported --table=" + p.params().get("table")));
        TableBackup.Result r = backup.backup(spec, p.businessDate(), p.runId());
        return JobOutcome.ok(Map.of("table", spec.table(), "rows", r.rows(), "output", store.uri(r.key())));
    }
}
