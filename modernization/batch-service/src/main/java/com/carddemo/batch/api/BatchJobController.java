package com.carddemo.batch.api;

import com.carddemo.batch.tranrept.TranreptJobConfig;
import com.carddemo.common.batch.BatchJobLauncher;
import java.time.LocalDate;
import java.util.Map;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * HTTP submit of the batch-service jobs; the run is the same as a command-line launch and its
 * outputs go to a new generation under {@code carddemo.batch.output-dir}, reported as {@code outputDir}.
 */
@RestController
@RequestMapping("/api/v1/batch")
public class BatchJobController {

    private final BatchJobLauncher launcher;

    public BatchJobController(BatchJobLauncher launcher) {
        this.launcher = launcher;
    }

    /**
     * TRANREPT for a date range: {@code EXEC PROC=TRANREPT} as CORPT00C submits it, the dates going to
     * the SORT INCLUDE and to DATEPARM. Transactions, CARDXREF, TRANTYPE and TRANCATG come from the services.
     */
    @PostMapping("/transaction-report")
    public Map<String, Object> transactionReport(
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) throws Exception {
        return launcher.launch(TranreptJobConfig.arguments(startDate, endDate)).summary();
    }
}
