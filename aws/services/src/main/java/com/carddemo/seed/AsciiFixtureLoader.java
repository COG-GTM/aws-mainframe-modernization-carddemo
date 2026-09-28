package com.carddemo.seed;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * Loads the legacy sample data (app/data/ASCII, plus the USRSEC users of DUSRSECJ) into an empty schema. Used for
 * local stand-alone runs and tests; production data is loaded by the data-migration pipeline.
 */
@Component
public class AsciiFixtureLoader {

    private static final Logger LOG = LoggerFactory.getLogger(AsciiFixtureLoader.class);

    private final JdbcClient jdbc;
    private final PasswordEncoder passwordEncoder;

    public AsciiFixtureLoader(JdbcClient jdbc, PasswordEncoder passwordEncoder) {
        this.jdbc = jdbc;
        this.passwordEncoder = passwordEncoder;
    }

    public boolean isEmpty() {
        return jdbc.sql("SELECT COUNT(*) FROM account").query(Long.class).single() == 0;
    }

    @Transactional
    public void load(Path asciiDir) {
        jdbc.sql("SET CONSTRAINTS ALL DEFERRED").update();
        loadUsers();
        each(asciiDir.resolve("trantype.txt"), 60, r -> jdbc.sql(
                "INSERT INTO transaction_type (type_cd, description) VALUES (:t, :d)")
                .param("t", r.raw(2)).param("d", r.textOrEmpty(50)).update());
        each(asciiDir.resolve("trancatg.txt"), 60, r -> jdbc.sql(
                "INSERT INTO transaction_category (type_cd, cat_cd, description) VALUES (:t, :c, :d)")
                .param("t", r.raw(2)).param("c", (int) r.unsigned(4)).param("d", r.textOrEmpty(50)).update());
        each(asciiDir.resolve("acctdata.txt"), 300, r -> jdbc.sql("""
                INSERT INTO account (acct_id, active_status, curr_bal, credit_limit, cash_credit_limit, open_date,
                       expiration_date, reissue_date, curr_cyc_credit, curr_cyc_debit, addr_zip, group_id)
                VALUES (:id, :st, :bal, :cl, :ccl, :od, :ed, :rd, :cc, :cd, :zip, :grp)""")
                .param("id", r.unsigned(11)).param("st", r.raw(1)).param("bal", r.signed(12, 2))
                .param("cl", r.signed(12, 2)).param("ccl", r.signed(12, 2)).param("od", r.date(10))
                .param("ed", r.date(10)).param("rd", r.date(10)).param("cc", r.signed(12, 2))
                .param("cd", r.signed(12, 2)).param("zip", r.text(10)).param("grp", r.text(10))
                .update());
        each(asciiDir.resolve("custdata.txt"), 500, r -> jdbc.sql("""
                INSERT INTO customer (cust_id, first_name, middle_name, last_name, addr_line_1, addr_line_2,
                       addr_line_3, addr_state_cd, addr_country_cd, addr_zip, phone_num_1, phone_num_2, ssn,
                       govt_issued_id, dob, eft_account_id, pri_card_holder_ind, fico_credit_score)
                VALUES (:id, :fn, :mn, :ln, :a1, :a2, :a3, :st, :co, :zip, :p1, :p2, :ssn, :gov, :dob, :eft,
                        :pri, :fico)""")
                .param("id", (int) r.unsigned(9)).param("fn", r.textOrEmpty(25)).param("mn", r.text(25))
                .param("ln", r.textOrEmpty(25)).param("a1", r.text(50)).param("a2", r.text(50))
                .param("a3", r.text(50)).param("st", r.text(2)).param("co", r.text(3)).param("zip", r.text(10))
                .param("p1", r.text(15)).param("p2", r.text(15)).param("ssn", r.raw(9)).param("gov", r.text(20))
                .param("dob", r.date(10)).param("eft", r.text(10)).param("pri", r.text(1))
                .param("fico", (int) r.unsigned(3))
                .update());
        each(asciiDir.resolve("carddata.txt"), 150, r -> jdbc.sql("""
                INSERT INTO card (card_num, acct_id, cvv_cd, embossed_name, expiration_date, active_status)
                VALUES (:num, :acct, :cvv, :name, :exp, :st)""")
                .param("num", r.raw(16)).param("acct", r.unsigned(11)).param("cvv", (int) r.unsigned(3))
                .param("name", r.textOrEmpty(50)).param("exp", r.date(10)).param("st", r.raw(1))
                .update());
        each(asciiDir.resolve("cardxref.txt"), 50, r -> jdbc.sql(
                "INSERT INTO card_xref (card_num, cust_id, acct_id) VALUES (:num, :cust, :acct)")
                .param("num", r.raw(16)).param("cust", (int) r.unsigned(9)).param("acct", r.unsigned(11))
                .update());
        each(asciiDir.resolve("dailytran.txt"), 350, r -> jdbc.sql("""
                INSERT INTO transaction (tran_id, type_cd, cat_cd, source, description, amt, merchant_id,
                       merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts)
                VALUES (:id, :t, :c, :src, :d, :amt, :mid, :mn, :mc, :mz, :card, :ots, :pts)""")
                .param("id", r.raw(16)).param("t", r.raw(2)).param("c", (int) r.unsigned(4))
                .param("src", r.text(10)).param("d", r.text(100)).param("amt", r.signed(11, 2))
                .param("mid", (int) r.unsigned(9)).param("mn", r.text(50)).param("mc", r.text(50))
                .param("mz", r.text(10)).param("card", r.raw(16)).param("ots", r.timestamp(26))
                .param("pts", r.timestamp(26))
                .update());
        each(asciiDir.resolve("tcatbal.txt"), 50, r -> jdbc.sql(
                "INSERT INTO tran_cat_balance (acct_id, type_cd, cat_cd, balance) VALUES (:a, :t, :c, :b)")
                .param("a", r.unsigned(11)).param("t", r.raw(2)).param("c", (int) r.unsigned(4))
                .param("b", r.signed(11, 2)).update());
        each(asciiDir.resolve("discgrp.txt"), 50, r -> jdbc.sql(
                "INSERT INTO disclosure_group (acct_group_id, type_cd, cat_cd, int_rate) VALUES (:g, :t, :c, :r)")
                .param("g", r.raw(10).stripTrailing()).param("t", r.raw(2)).param("c", (int) r.unsigned(4))
                .param("r", r.signed(6, 2)).update());
        LOG.info("Loaded CardDemo ASCII fixtures from {}", asciiDir.toAbsolutePath());
    }

    /** USRSEC records of DUSRSECJ: id X(8), first X(20), last X(20), password X(8), type X(1). */
    private void loadUsers() {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(
                new ClassPathResource("seed/usrsec.txt").getInputStream(), StandardCharsets.US_ASCII))) {
            reader.lines().filter(l -> !l.isBlank()).forEach(lines::add);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
        Map<String, String> hashes = new HashMap<>();
        for (String line : lines) {
            FixedRecord r = new FixedRecord(line, 80);
            String id = r.raw(8).strip().toUpperCase();
            String first = r.textOrEmpty(20);
            String last = r.textOrEmpty(20);
            String password = r.raw(8).strip().toUpperCase();
            String type = r.raw(1);
            jdbc.sql("""
                    INSERT INTO user_security (user_id, first_name, last_name, password_hash, user_type)
                    VALUES (:id, :f, :l, :h, :t)""")
                    .param("id", id).param("f", first).param("l", last).param("h", hashes.computeIfAbsent(password, passwordEncoder::encode))
                    .param("t", type).update();
        }
    }

    private static void each(Path file, int recordLength, Consumer<FixedRecord> action) {
        try (BufferedReader reader = Files.newBufferedReader(file, StandardCharsets.US_ASCII)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.isBlank()) {
                    action.accept(new FixedRecord(line, recordLength));
                }
            }
        } catch (IOException ex) {
            throw new UncheckedIOException("Cannot read " + file, ex);
        }
    }
}
