package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchCli;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchJobRegistry;
import com.carddemo.common.batch.ReturnCodes;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class BatchServiceApplicationTest {

    @Autowired
    private TestRestTemplate http;

    @Autowired
    private BatchJobLauncher launcher;

    @Test
    void startsWithHealthAndTheTranreptJob() {
        assertThat(http.getForObject("/actuator/health", String.class)).contains("\"status\":\"UP\"");
        assertThat(launcher.registry().names()).containsExactly("TRANREPT");
        assertThatThrownBy(() -> launcher.launch(BatchArguments.of("CREASTMT", null, null)))
                .isInstanceOf(BatchJobRegistry.UnknownJobException.class);
    }

    @Test
    void cliRejectsBadArgumentsAsAJclError() {
        assertThat(BatchCli.run(BatchServiceApplication.class, "--job=TRANREPT", "--date=not-a-date"))
                .isEqualTo(ReturnCodes.JCL_ERROR);
    }
}
