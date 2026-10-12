package com.carddemo.zerotrust.posting;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.nio.file.Path;
import java.time.Duration;

/**
 * {@code carddemo.zerotrust.*} configuration.
 *
 * @param policy     authorization policy location ({@code classpath:zero-trust-policy.yaml})
 * @param auditLog   path of the hash-chained audit log (JSON Lines)
 * @param workloadId this workload's identity (default {@code svc-posttran-post})
 * @param tokenTtl   requested access-token lifetime (default PT5M, hard max PT15M)
 * @param issuer     token issuer id (default {@code urn:carddemo:local-issuer})
 * @param audience   access-token audience (default {@code urn:carddemo:posting})
 */
@ConfigurationProperties(prefix = "carddemo.zerotrust")
public record ZeroTrustProperties(
        String policy,
        Path auditLog,
        String workloadId,
        Duration tokenTtl,
        String issuer,
        String audience) {

    public ZeroTrustProperties {
        if (workloadId == null) {
            workloadId = "svc-posttran-post";
        }
        if (tokenTtl == null) {
            tokenTtl = Duration.ofMinutes(5);
        }
        if (issuer == null) {
            issuer = "urn:carddemo:local-issuer";
        }
        if (audience == null) {
            audience = "urn:carddemo:posting";
        }
    }
}
