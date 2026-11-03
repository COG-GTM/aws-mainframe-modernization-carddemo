package com.carddemo.transaction.api;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.transaction.batch.BatchConfig;
import java.time.LocalDate;
import java.util.Map;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Replaces the JCL submit of POSTTRAN and INTCALC. A scheduler (EventBridge, Airflow, Control-M)
 * calls these endpoints on the same cadence the mainframe batch window used. The run is the same
 * as a command-line launch ({@code --job=POSTTRAN}): output files go to a new generation directory
 * under {@code carddemo.batch.output-dir}, reported as {@code outputDir}.
 */
@RestController
@RequestMapping("/api/v1/batch")
public class BatchJobController {

    private final BatchJobLauncher launcher;

    public BatchJobController(BatchJobLauncher launcher) {
        this.launcher = launcher;
    }

    @PostMapping("/post-transactions")
    public Map<String, Object> postTransactions(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date)
            throws Exception {
        return launcher.launch(BatchArguments.of(BatchConfig.POSTTRAN, date, null)).summary();
    }

    @PostMapping("/interest-calculation")
    public Map<String, Object> interestCalculation(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date,
            @RequestParam(required = false) String parm) throws Exception {
        return launcher.launch(BatchArguments.of(BatchConfig.INTCALC, date, parm)).summary();
    }
}
