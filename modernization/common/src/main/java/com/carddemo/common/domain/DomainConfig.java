package com.carddemo.common.domain;

import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@Configuration
@EntityScan(basePackageClasses = DomainConfig.class)
@EnableJpaRepositories(basePackageClasses = DomainConfig.class)
public class DomainConfig {
}
