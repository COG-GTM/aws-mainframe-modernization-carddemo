package com.carddemo.transaction;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import ch.qos.logback.classic.Level;
import com.carddemo.support.LogCapture;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.dao.DataIntegrityViolationException;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * s6.4: a failed INSERT of a transaction (DUPREC) does not put the bound card number into the exception message or
 * any log line: PgJDBC ({@code logServerErrorDetail=false}), Spring JDBC/ORM and the application at DEBUG,
 * Hibernate at its shipped level (WARN/ERROR, {@code SqlExceptionHelper}). Hibernate DEBUG prints entity state
 * ({@code EntityPrinter}), so no profile may enable it ({@code ProfileFilesTest}).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@Testcontainers
class TransactionPanLoggingIT {

    private static final String PAN = "4859452612877065";

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    TransactionRepository transactions;

    private static TransactionRecord record(String id) {
        return new TransactionRecord(id, "01", 1, "POS TERM", "Purchase", new BigDecimal("45.10"), 100,
                "Merchant", "Seattle", "98101", PAN, "2022-06-01 10:11:12.000000", "2022-07-06 13:45:10.000000");
    }

    private static List<String> chain(Throwable e) {
        List<String> messages = new ArrayList<>();
        for (Throwable t = e; t != null; t = t.getCause()) {
            messages.add(String.valueOf(t.getMessage()));
            if (t instanceof java.sql.SQLException sql && sql.getNextException() != null) {
                messages.add(String.valueOf(sql.getNextException().getMessage()));
            }
        }
        return messages;
    }

    @Test
    void aDuplicateInsertLogsNoCardNumber() {
        transactions.deleteAllInBatch();
        transactions.saveAndFlush(Transaction.newRecord(record("0000000000000001")));
        try (LogCapture logs = LogCapture.start(Level.DEBUG, "com.carddemo", "org.postgresql",
                "com.zaxxer", "org.springframework.jdbc", "org.springframework.orm")) {
            assertThatThrownBy(() -> transactions.saveAndFlush(Transaction.newRecord(record("0000000000000001"))))
                    .isInstanceOf(DataIntegrityViolationException.class)
                    .satisfies(e -> assertThat(chain(e)).allSatisfy(m -> assertThat(m).doesNotContain(PAN)));
            assertThatThrownBy(() -> transactions.saveAllAndFlush(List.of(
                    Transaction.newRecord(record("0000000000000002")), Transaction.newRecord(record("0000000000000001")))))
                    .isInstanceOf(DataIntegrityViolationException.class)
                    .satisfies(e -> assertThat(chain(e)).allSatisfy(m -> assertThat(m).doesNotContain(PAN)));
            assertThat(logs.size()).isPositive();
            assertThat(logs.lines()).allSatisfy(l -> assertThat(l).doesNotContain(PAN));
        }
    }
}
