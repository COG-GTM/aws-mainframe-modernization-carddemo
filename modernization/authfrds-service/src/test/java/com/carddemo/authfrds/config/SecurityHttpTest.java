package com.carddemo.authfrds.config;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ActiveProfiles;

/** Real servlet container (error dispatch to /error happens here, unlike MockMvc). No Db2 needed. */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = "management.health.db.enabled=false")
@ActiveProfiles("test")
class SecurityHttpTest {

    @Autowired
    TestRestTemplate http;

    @Test
    @DisplayName("BR-AF-041 over real HTTP: maintainer POST to a test hook without 'testing' -> 404 (not 403)")
    void hookAbsentOverHttp() {
        var r = http.withBasicAuth("test-maintainer", "test-maintainer-pw")
                .postForEntity("/api/v1/ims-stub/reset", null, String.class);
        assertThat(r.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
    }

    @Test
    @DisplayName("BR-AF-040 over real HTTP: anonymous 401, reader write 403, reader GET 200")
    void rolesOverHttp() {
        assertThat(http.getForEntity("/api/v1/accounts", String.class).getStatusCode())
                .isEqualTo(HttpStatus.UNAUTHORIZED);
        assertThat(http.withBasicAuth("test-reader", "test-reader-pw")
                .postForEntity("/api/v1/fraud-records", "{}", String.class).getStatusCode())
                .isEqualTo(HttpStatus.FORBIDDEN);
        assertThat(http.withBasicAuth("test-reader", "test-reader-pw")
                .getForEntity("/api/v1/accounts", String.class).getStatusCode()).isEqualTo(HttpStatus.OK);
    }
}
