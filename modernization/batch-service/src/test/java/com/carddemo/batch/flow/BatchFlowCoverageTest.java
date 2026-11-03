package com.carddemo.batch.flow;

import com.carddemo.testsupport.flow.FlowCoverage;
import org.junit.jupiter.api.Test;

/** Gate g-online-tests for batch-service (CORPT00C). */
class BatchFlowCoverageTest {

    @Test
    void everyBatchServiceScreenKeyHasAFlowTest() {
        FlowCoverage.assertEveryScreenKeyCovered("batch-service", ReportSubmissionFlowTest.class);
    }
}
