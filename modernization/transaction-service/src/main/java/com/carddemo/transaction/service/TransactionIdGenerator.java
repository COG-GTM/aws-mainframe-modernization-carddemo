package com.carddemo.transaction.service;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * TRAN-ID for COTRN02C and COBIL00C: 16 digits, zero padded, one more than the highest id when the
 * COBOL would have read it (STARTBR HIGH-VALUES, READPREV, ADD 1). A database sequence
 * ({@code tran_id_seq}, V6) hands out the numbers so concurrent callers on several replicas never get
 * the same one; each call also lifts the sequence past the highest numeric TRAN-ID stored, because
 * POSTTRAN writes TRANSACT rows with ids taken from DALYTRAN. The catch-up runs under an advisory lock
 * in its own short database transaction, so it never waits on the caller's transaction.
 */
@Component
public class TransactionIdGenerator {

    static final int LENGTH = 16;

    /** Two-int advisory lock key space; the bill-payment lock uses the single bigint space, so they never collide. */
    private static final int LOCK_CLASS = 0x5452;
    private static final int LOCK_OBJECT = 1;

    private static final String NEXT_ID = """
            SELECT setval('transaction.tran_id_seq', GREATEST(nextval('transaction.tran_id_seq'),
                   COALESCE((SELECT CAST(MAX(tran_id) AS BIGINT) + 1 FROM transaction.transactions
                             WHERE tran_id ~ '^[0-9]{16}$'), 1)))
            """;

    private final JdbcTemplate jdbc;
    private final TransactionTemplate ownTransaction;

    public TransactionIdGenerator(JdbcTemplate jdbc, PlatformTransactionManager transactionManager) {
        this.jdbc = jdbc;
        this.ownTransaction = new TransactionTemplate(transactionManager);
        this.ownTransaction.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
    }

    public String next() {
        Long id = ownTransaction.execute(status -> {
            jdbc.queryForObject("SELECT pg_advisory_xact_lock(?, ?)", Object.class, LOCK_CLASS, LOCK_OBJECT);
            return jdbc.queryForObject(NEXT_ID, Long.class);
        });
        return format(id);
    }

    static String format(long id) {
        return String.format("%0" + LENGTH + "d", id);
    }
}
