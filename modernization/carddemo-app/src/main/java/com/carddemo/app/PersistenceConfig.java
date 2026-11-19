package com.carddemo.app;

import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

/** Entities and repositories live in the bounded-context packages {@code com.carddemo.<context>} (D-003), not under {@code app}. */
@Configuration
@EntityScan("com.carddemo")
@EnableJpaRepositories("com.carddemo")
public class PersistenceConfig {
}
