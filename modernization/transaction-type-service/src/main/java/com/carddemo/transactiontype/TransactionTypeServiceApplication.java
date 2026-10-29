package com.carddemo.transactiontype;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
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
                + "(COTRTLIC, COTRTUPC, COBTUPDT, TRANEXTR), built from specs/aws-transform/transaction-type."))
public class TransactionTypeServiceApplication {

    public static void main(String[] args) {
        SpringApplication.run(TransactionTypeServiceApplication.class, args);
    }
}
