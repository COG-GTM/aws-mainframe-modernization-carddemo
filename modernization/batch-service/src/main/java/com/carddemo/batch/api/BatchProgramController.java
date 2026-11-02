package com.carddemo.batch.api;

import com.carddemo.batch.export.ExportJobsConfig;
import com.carddemo.batch.statement.StatementJobConfig;
import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobLauncher;
import java.util.Map;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * HTTP launch of the statement and branch-migration jobs (api-style.md: batch jobs under
 * {@code /api/v1/batch/<job-name>}). Each call runs the job synchronously on the service tables
 * (CBIMPORT on the latest CBEXPORT output) and returns the same summary as the CLI.
 */
@RestController
@RequestMapping("/api/v1/batch")
public class BatchProgramController {

    private final BatchJobLauncher launcher;

    public BatchProgramController(BatchJobLauncher launcher) {
        this.launcher = launcher;
    }

    /** CREASTMT: one statement per card cross reference (CBSTM03A/B). */
    @PostMapping("/statements")
    public Map<String, Object> statements() throws Exception {
        return launcher.launch(BatchArguments.of(StatementJobConfig.CREASTMT, null, null)).summary();
    }

    /** CBEXPORT: customers, accounts, cross references, transactions and cards into EXPFILE. */
    @PostMapping("/export")
    public Map<String, Object> export() throws Exception {
        return launcher.launch(BatchArguments.of(ExportJobsConfig.CBEXPORT, null, null)).summary();
    }

    /** CBIMPORT: the latest EXPFILE back into the five record files plus ERROUT. */
    @PostMapping("/import")
    public Map<String, Object> importFile() throws Exception {
        return launcher.launch(BatchArguments.of(ExportJobsConfig.CBIMPORT, null, null)).summary();
    }
}
