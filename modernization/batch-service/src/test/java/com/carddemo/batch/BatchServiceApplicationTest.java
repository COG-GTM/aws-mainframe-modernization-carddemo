package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchCli;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchJobRegistry;
import com.carddemo.common.batch.ReturnCodes;
import com.carddemo.common.security.TokenService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class BatchServiceApplicationTest {

    @Autowired
    private TestRestTemplate http;

    @Autowired
    private BatchJobLauncher launcher;

    @Autowired
    private TokenService tokens;

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

    @Test
    void guardsApiWithTheSharedBearerTokenAndLeavesActuatorAndDocsOpen() {
        ResponseEntity<String> anonymous = http.getForEntity("/api/v1/batch", String.class);
        assertThat(anonymous.getStatusCode()).isEqualTo(HttpStatus.UNAUTHORIZED);
        assertThat(anonymous.getHeaders().getFirst(HttpHeaders.WWW_AUTHENTICATE)).isEqualTo("Bearer");
        assertThat(anonymous.getBody()).contains("\"message\":\"Authentication required. Sign on first ...\"");

        HttpHeaders bearer = new HttpHeaders();
        bearer.setBearerAuth(tokens.issue("USER0001", "U").token());
        ResponseEntity<String> signedOn = http.exchange("/api/v1/batch", HttpMethod.GET, new HttpEntity<>(bearer), String.class);
        assertThat(signedOn.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);

        assertThat(http.getForEntity("/v3/api-docs", String.class).getBody()).contains("\"bearerAuth\"");
    }
}
