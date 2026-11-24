package com.carddemo.transactiontype;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityScheme;
import io.swagger.v3.oas.annotations.info.Info;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;

@SpringBootApplication
@ConfigurationPropertiesScan
@OpenAPIDefinition(info = @Info(
        title = "CardDemo Transaction Type Service",
        version = "0.1.0",
        description = "Java 21 / Spring Boot 3 implementation of the CardDemo Db2 transaction-type module "
                + "(COTRTLIC, COTRTUPC, COBTUPDT, TRANEXTR), built from specs/aws-transform/transaction-type. HTTP Basic: GET needs READER or MAINTAINER, "
                + "writes need MAINTAINER."),
        security = @SecurityRequirement(name = "basicAuth"))
@SecurityScheme(name = "basicAuth", type = SecuritySchemeType.HTTP, scheme = "basic")
public class TransactionTypeServiceApplication {

    public static void main(String[] args) {
        SpringApplication.run(TransactionTypeServiceApplication.class, args);
    }
}
