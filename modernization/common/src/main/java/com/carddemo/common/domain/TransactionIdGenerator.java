package com.carddemo.common.domain;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * Issues 16-character transaction IDs. COBIL00C/COTRN02C browse to the last TRAN-ID and add 1, which races under
 * concurrency and breaks once INTCALC writes date-prefixed IDs; a database sequence replaces it.
 */
@Component
public class TransactionIdGenerator {

    private final JdbcTemplate jdbc;

    public TransactionIdGenerator(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public String next() {
        Long value = jdbc.queryForObject("select nextval('tran_id_seq')", Long.class);
        return String.format("%016d", value);
    }
}
