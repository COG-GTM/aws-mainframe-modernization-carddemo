package com.carddemo.common.security;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import javax.crypto.SecretKey;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Local HS256 token issuer/validator. In AWS the identity-service sign-on is replaced by Amazon Cognito and this
 * validator by Cognito JWKS validation; claims (sub, userType, cognito:groups) keep the same meaning.
 */
@Component
public class JwtService {

    private final SecretKey key;
    private final Duration ttl;
    private final String issuer;

    public JwtService(@Value("${carddemo.security.jwt-secret}") String secret,
                      @Value("${carddemo.security.jwt-ttl:PT1H}") Duration ttl,
                      @Value("${carddemo.security.issuer:carddemo-identity}") String issuer) {
        byte[] bytes = secret.getBytes(StandardCharsets.UTF_8);
        if (bytes.length < 32) {
            throw new IllegalStateException("carddemo.security.jwt-secret must be at least 32 bytes");
        }
        this.key = Keys.hmacShaKeyFor(bytes);
        this.ttl = ttl;
        this.issuer = issuer;
    }

    public String issue(CardDemoPrincipal principal) {
        Instant now = Instant.now();
        return Jwts.builder()
                .setIssuer(issuer)
                .setSubject(principal.userId())
                .claim("userType", principal.userType())
                .claim("firstName", principal.firstName())
                .claim("lastName", principal.lastName())
                .setIssuedAt(Date.from(now))
                .setExpiration(Date.from(now.plus(ttl)))
                .signWith(key)
                .compact();
    }

    public CardDemoPrincipal parse(String token) {
        Claims claims = Jwts.parserBuilder()
                .requireIssuer(issuer)
                .setSigningKey(key)
                .build()
                .parseClaimsJws(token)
                .getBody();
        return new CardDemoPrincipal(claims.getSubject(), claims.get("userType", String.class),
                claims.get("firstName", String.class), claims.get("lastName", String.class));
    }

    public Duration getTtl() {
        return ttl;
    }
}
