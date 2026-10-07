package com.carddemo.web.security;

import com.carddemo.user.UserType;
import java.time.Instant;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.security.oauth2.jose.jws.MacAlgorithm;
import org.springframework.security.oauth2.jwt.JwsHeader;
import org.springframework.security.oauth2.jwt.JwtClaimsSet;
import org.springframework.security.oauth2.jwt.JwtEncoder;
import org.springframework.security.oauth2.jwt.JwtEncoderParameters;
import org.springframework.stereotype.Service;

/**
 * Issues the HS256 token that replaces {@code CDEMO-USER-ID}/{@code CDEMO-USER-TYPE} of the COMMAREA (ADR-0017):
 * {@code sub} = user id, {@code role} = {@code ADMIN}/{@code USER}, {@code usrType} = the level-88 code.
 * Expiry uses the wall clock, not the (possibly pinned) business clock.
 */
@Service
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
public class TokenService {

    public static final String ROLE_CLAIM = "role";
    public static final String USER_TYPE_CLAIM = "usrType";

    private final JwtEncoder encoder;
    private final JwtProperties properties;

    public TokenService(JwtEncoder encoder, JwtProperties properties) {
        this.encoder = encoder;
        this.properties = properties;
    }

    public IssuedToken issue(String userId, UserType userType) {
        Instant now = Instant.now();
        Instant expiresAt = now.plus(properties.ttl());
        JwtClaimsSet claims = JwtClaimsSet.builder()
                .issuer(properties.issuer())
                .subject(userId)
                .issuedAt(now)
                .expiresAt(expiresAt)
                .claim(ROLE_CLAIM, userType.name())
                .claim(USER_TYPE_CLAIM, userType.code())
                .build();
        JwsHeader header = JwsHeader.with(MacAlgorithm.HS256).type("JWT").build();
        return new IssuedToken(encoder.encode(JwtEncoderParameters.from(header, claims)).getTokenValue(), expiresAt);
    }
}
