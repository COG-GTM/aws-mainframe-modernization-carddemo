package com.carddemo.app;

import com.carddemo.account.batch.AccountRecordLayout;
import com.carddemo.account.domain.Account;
import com.carddemo.card.batch.CardXrefRecordLayout;
import com.carddemo.card.domain.CardXref;
import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchUsageException;
import com.carddemo.common.batch.ReturnCode;
import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.error.AbendException;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.common.record.RecordReader;
import com.carddemo.reference.batch.TranCatBalanceRecordLayout;
import com.carddemo.reference.domain.TranCatBalance;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionTemplate;

import java.io.PrintStream;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * {@code --job=LOAD --dd=ACCTDATA=<file> --dd=CARDXREF=<file> --dd=TCATBALF=<file> [--dd-encoding=ASCII|CP037]}: seeds
 * the tables POSTTRAN reads from the same files the oracle loads (plan section 4). ASCII files are line-sequential
 * (CR stripped, short lines padded to LRECL: the sample quirks in 02_conventions.md); CP037 files are RECFM=FB.
 * Plain INSERTs, all DDs in one transaction: a key already present is a duplicate key and abends RC 16, nothing loaded.
 */
@Component("LOAD")
public class DataLoader implements BatchJobLauncher.CardDemoJob {

    private record Table<T>(String sql, RecordLayout layout, Function<CobolRecord, T> mapper,
                            Function<T, Object[]> columns) {
    }

    private static final Map<String, Table<?>> TABLES = new LinkedHashMap<>();

    static {
        TABLES.put("ACCTDATA", new Table<Account>("""
                INSERT INTO account.account (acct_id, active_status, curr_bal, credit_limit, cash_credit_limit, open_date,
                expiration_date, reissue_date, curr_cyc_credit, curr_cyc_debit, addr_zip, group_id)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""", AccountRecordLayout.LAYOUT, AccountRecordLayout::toEntity,
                a -> new Object[] {a.getAcctId(), a.getActiveStatus(), a.getCurrBal(), a.getCreditLimit(),
                        a.getCashCreditLimit(), a.getOpenDate(), a.getExpirationDate(), a.getReissueDate(),
                        a.getCurrCycCredit(), a.getCurrCycDebit(), a.getAddrZip(), a.getGroupId()}));
        TABLES.put("CARDXREF", new Table<CardXref>(
                "INSERT INTO card.card_xref (card_num, cust_id, acct_id) VALUES (?, ?, ?)",
                CardXrefRecordLayout.LAYOUT, CardXrefRecordLayout::toEntity,
                x -> new Object[] {x.getCardNum(), x.getCustId(), x.getAcctId()}));
        TABLES.put("TCATBALF", new Table<TranCatBalance>(
                "INSERT INTO reference.tran_cat_balance (acct_id, type_cd, cat_cd, balance) VALUES (?, ?, ?, ?)",
                TranCatBalanceRecordLayout.LAYOUT, TranCatBalanceRecordLayout::toEntity,
                t -> new Object[] {t.getId().acctId(), t.getId().typeCd(), t.getId().catCd(), t.getBalance()}));
    }

    private final JdbcTemplate jdbc;
    private final TransactionTemplate tx;
    private final PrintStream out;

    public DataLoader(JdbcTemplate jdbc, TransactionTemplate tx) {
        this.jdbc = jdbc;
        this.tx = tx;
        this.out = System.out;
    }

    @Override
    public int run(BatchArguments arguments) {
        if (arguments.dds().isEmpty()) {
            throw new BatchUsageException("LOAD needs at least one --dd=<ACCTDATA|CARDXREF|TCATBALF>=<file>");
        }
        for (String dd : arguments.dds().keySet()) {
            if (!TABLES.containsKey(dd)) {
                throw new BatchUsageException("LOAD: unknown DD " + dd + "; known: " + TABLES.keySet());
            }
        }
        Map<String, Integer> counts = tx.execute(status -> {
            Map<String, Integer> loaded = new LinkedHashMap<>();
            TABLES.forEach((dd, table) -> {
                Path file = arguments.dds().get(dd);
                if (file != null) {
                    loaded.put(dd, load(table, dd, file, arguments.ddEncoding()));
                }
            });
            return loaded;
        });
        counts.forEach((dd, n) -> out.println("LOADED " + dd + " : " + n));
        return ReturnCode.OK;
    }

    private <T> int load(Table<T> table, String dd, Path file, CodePage codePage) {
        RecordFormat format = codePage == CodePage.ASCII ? RecordFormat.LINE_SEQUENTIAL : RecordFormat.FIXED_BLOCK;
        List<Object[]> rows = new ArrayList<>();
        try (RecordReader reader = RecordReader.open(file, dd, table.layout(), codePage, format)) {
            for (var r = reader.read(); r.isPresent(); r = reader.read()) {
                rows.add(table.columns().apply(table.mapper().apply(r.get())));
            }
        }
        try {
            jdbc.batchUpdate(table.sql(), rows);
        } catch (DataAccessException ex) {
            // The driver message echoes the key (a full card number for CARDXREF): report only the SQLSTATE.
            String sqlState = ex.getMostSpecificCause() instanceof SQLException sql ? sql.getSQLState() : "unknown";
            throw new AbendException("LOAD " + dd + " insert failed, SQLSTATE " + sqlState
                    + (ex instanceof DuplicateKeyException ? " (duplicate key)" : ""));
        }
        return rows.size();
    }
}
