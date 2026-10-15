package com.carddemo.common.testing;

import com.carddemo.common.security.CardDemoPrincipal;
import com.carddemo.common.security.JwtService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.jdbc.Sql;

/** Base for parity tests: loads the shared fixture before each test and mints tokens for fixture users. */
@Sql(scripts = "classpath:fixtures/carddemo-fixture.sql")
public abstract class ParityTestSupport {

    @Autowired
    protected JwtService jwtService;

    protected String adminToken() {
        return "Bearer " + jwtService.issue(new CardDemoPrincipal("ADMIN001", "A", "MARGARET", "GOLD"));
    }

    protected String userToken() {
        return "Bearer " + jwtService.issue(new CardDemoPrincipal("USER0001", "U", "LAWRENCE", "THOMAS"));
    }
}
