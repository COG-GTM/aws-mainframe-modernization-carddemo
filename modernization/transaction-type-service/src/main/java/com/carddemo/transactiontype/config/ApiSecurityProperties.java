package com.carddemo.transactiontype.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "carddemo.security")
public record ApiSecurityProperties(Credentials maintainer, Credentials reader) {

    public record Credentials(String username, String password) {
        boolean isComplete() {
            return username != null && !username.isBlank() && password != null && !password.isBlank();
        }
    }
}
