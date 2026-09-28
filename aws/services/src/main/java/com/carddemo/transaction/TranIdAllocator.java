package com.carddemo.transaction;

import com.carddemo.common.Text;
import java.math.BigDecimal;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Next TRAN-ID = highest existing id + 1, zero padded to 16 (legacy STARTBR HIGH-VALUES / READPREV / ADD 1).
 * A transaction-scoped advisory lock serializes allocation until the caller's transaction commits.
 */
@Component
public class TranIdAllocator {

    static final long ADVISORY_LOCK_KEY = 0x434452544E4944L;

    private final JdbcClient jdbc;

    public TranIdAllocator(JdbcClient jdbc) {
        this.jdbc = jdbc;
    }

    @Transactional(propagation = Propagation.MANDATORY)
    public String next() {
        jdbc.sql("SELECT pg_advisory_xact_lock(:key)").param("key", ADVISORY_LOCK_KEY).query().singleRow();
        BigDecimal max = jdbc.sql("SELECT COALESCE(MAX(CAST(tran_id AS NUMERIC)), 0) FROM transaction "
                + "WHERE tran_id ~ '^[0-9]+$'")
                .query(BigDecimal.class).single();
        return Text.leftPadZeros(max.add(BigDecimal.ONE).toPlainString(), 16);
    }
}
