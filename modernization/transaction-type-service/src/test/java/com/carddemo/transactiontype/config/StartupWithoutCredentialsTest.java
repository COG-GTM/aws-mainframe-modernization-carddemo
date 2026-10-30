package com.carddemo.transactiontype.config;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.transactiontype.TransactionTypeServiceApplication;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

class StartupWithoutCredentialsTest {

    @Test
    @DisplayName("default profile with no TT_API_* env -> the application refuses to start")
    void applicationRefusesToStart() {
        var app = new SpringApplication(TransactionTypeServiceApplication.class);
        Throwable failure = null;
        try (ConfigurableApplicationContext ignored = app.run("--spring.profiles.active=default",
                "--server.port=0", "--carddemo.security.maintainer.username=", "--carddemo.security.reader.username=")) {
            // started: failure stays null
        } catch (RuntimeException e) {
            failure = e;
        }
        assertThat(failure).isNotNull();
        Throwable root = failure;
        while (root.getCause() != null) {
            root = root.getCause();
        }
        assertThat(root).isInstanceOf(IllegalStateException.class).hasMessageContaining("TT_API_USER");
    }
}
