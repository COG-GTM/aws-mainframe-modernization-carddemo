package com.carddemo.web;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.nimbusds.jose.jwk.source.ImmutableSecret;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.security.oauth2.jose.jws.MacAlgorithm;
import org.springframework.security.oauth2.jwt.JwsHeader;
import org.springframework.security.oauth2.jwt.JwtClaimsSet;
import org.springframework.security.oauth2.jwt.JwtEncoderParameters;
import org.springframework.security.oauth2.jwt.NimbusJwtEncoder;

/** The token is the only source of user id and role (ADR-0017): forged, expired or foreign tokens are refused. */
class JwtSecurityTest extends OnlineWebTest {

    @Autowired
    SecretKey jwtSigningKey;

    private String token(SecretKey key, String issuer, Instant expiresAt, String role) {
        JwtClaimsSet claims = JwtClaimsSet.builder().issuer(issuer).subject("USER0001")
                .issuedAt(expiresAt.minusSeconds(3600)).expiresAt(expiresAt).claim("role", role)
                .claim("usrType", "U").build();
        return "Bearer " + new NimbusJwtEncoder(new ImmutableSecret<>(key))
                .encode(JwtEncoderParameters.from(JwsHeader.with(MacAlgorithm.HS256).build(), claims))
                .getTokenValue();
    }

    @Test
    void aValidTokenIsAccepted() throws Exception {
        mvc.perform(get(MENU).header(HttpHeaders.AUTHORIZATION,
                token(jwtSigningKey, "carddemo", Instant.now().plusSeconds(600), "USER")))
                .andExpect(status().isOk());
    }

    @Test
    void anExpiredTokenIsRefused() throws Exception {
        mvc.perform(get(MENU).header(HttpHeaders.AUTHORIZATION,
                token(jwtSigningKey, "carddemo", Instant.now().minusSeconds(600), "USER")))
                .andExpect(status().isUnauthorized())
                .andExpect(header().string(HttpHeaders.WWW_AUTHENTICATE, "Bearer"))
                .andExpect(jsonPath("$.code").value("SIGNON_REQUIRED"));
    }

    @Test
    void aTokenSignedWithAnotherKeyIsRefused() throws Exception {
        SecretKey other = new SecretKeySpec("another-key-another-key-another-key!".getBytes(StandardCharsets.UTF_8),
                "HmacSHA256");
        mvc.perform(get(MENU).header(HttpHeaders.AUTHORIZATION,
                token(other, "carddemo", Instant.now().plusSeconds(600), "ADMIN")))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void aTokenFromAnotherIssuerIsRefused() throws Exception {
        mvc.perform(get(MENU).header(HttpHeaders.AUTHORIZATION,
                token(jwtSigningKey, "someone-else", Instant.now().plusSeconds(600), "USER")))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void theRoleComesFromTheTokenNotFromTheRequest() throws Exception {
        mvc.perform(get(MENU + "/admin").header(HttpHeaders.AUTHORIZATION, bearer("USER0001",
                com.carddemo.user.UserType.USER)).header("X-Role", "ADMIN").param("role", "ADMIN"))
                .andExpect(status().isForbidden());
    }

    @Test
    void theSignOnEndpointsAreAnonymous() throws Exception {
        mvc.perform(get(LOGIN)).andExpect(status().isOk());
    }

    @Test
    void noSessionCookieIsIssued() throws Exception {
        givenUser("USER0001", "PASSWORD", com.carddemo.user.UserType.USER);
        mvc.perform(login("USER0001", "PASSWORD"))
                .andExpect(status().isOk())
                .andExpect(header().doesNotExist(HttpHeaders.SET_COOKIE));
    }
}
