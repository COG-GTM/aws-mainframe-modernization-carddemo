package com.carddemo.common.security;

import java.time.Duration;
import java.util.List;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

/**
 * {@code carddemo.security.*}. {@code token-secret} is the HS256 key shared by all five services
 * (at least 32 bytes); {@code public-paths} lists extra {@code /api/**} patterns that need no token
 * (only the sign-on endpoint in auth-service).
 */
@ConfigurationProperties("carddemo.security")
public record SecurityProperties(String tokenSecret,
                                 @DefaultValue("PT8H") Duration tokenTtl,
                                 @DefaultValue("carddemo-auth-service") String issuer,
                                 @DefaultValue List<String> publicPaths) {
}
