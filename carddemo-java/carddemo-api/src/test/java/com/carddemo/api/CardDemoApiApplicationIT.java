package com.carddemo.api;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.test.CoreRepositories;
import com.carddemo.data.test.PostgresTestcontainersConfiguration;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@Import(PostgresTestcontainersConfiguration.class)
class CardDemoApiApplicationIT {

    @Autowired
    ApplicationContext context;

    @Autowired
    TestRestTemplate rest;

    @Test
    void healthIsUpWithDatabase() {
        ResponseEntity<String> response = rest.getForEntity("/actuator/health", String.class);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(response.getBody()).contains("\"status\":\"UP\"");
    }

    @Test
    void servesWebModuleIndexPage() {
        ResponseEntity<String> response = rest.getForEntity("/", String.class);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(response.getBody()).contains("<title>CardDemo</title>");
    }

    @Test
    void onlyHealthIsExposed() {
        ResponseEntity<String> response = rest.getForEntity("/actuator/env", String.class);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
    }

    @Test
    void loadsCarddemoDataRepositories() {
        CoreRepositories.assertLoaded(context);
    }
}
