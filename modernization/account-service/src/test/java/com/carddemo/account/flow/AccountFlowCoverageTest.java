package com.carddemo.account.flow;

import com.carddemo.testsupport.flow.FlowCoverage;
import org.junit.jupiter.api.Test;

/** Gate g-online-tests for account-service. */
class AccountFlowCoverageTest {

    @Test
    void everyAccountServiceScreenKeyHasAFlowTest() {
        FlowCoverage.assertEveryScreenKeyCovered("account-service", AccountFlowTest.class,
                AccountUpdateMessagesFlowTest.class);
    }
}
