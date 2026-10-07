package com.carddemo.common.schema;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.codec.Copybook;
import com.carddemo.common.codec.Field;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.common.codec.TestData;
import com.carddemo.common.file.RecordFiles;
import com.carddemo.common.schema.CopybookColumnMap.Entry;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.transaction.support.TransactionTemplate;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * s3.1 acceptance: Flyway applies V1+V2 on PostgreSQL 16, every mapped copybook field is a column of the type the
 * ADRs prescribe, keys/indexes/FKs match the VSAM clusters and AIX paths, and every sample record loads.
 */
@Testcontainers
class CoreSchemaIT {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    static JdbcTemplate jdbc;
    static Flyway flyway;

    /** Sample file -> copybook; load order respects the foreign keys. DALYTRAN.PS.INIT is the empty-file seed. */
    static final Map<String, String> SAMPLES = sampleFiles();

    private static Map<String, String> sampleFiles() {
        Map<String, String> m = new LinkedHashMap<>();
        m.put("TRANTYPE", "CVTRA03Y");
        m.put("TRANCATG", "CVTRA04Y");
        m.put("DISCGRP", "CVTRA02Y");
        m.put("CUSTDATA", "CVCUS01Y");
        m.put("ACCTDATA", "CVACT01Y");
        m.put("CARDDATA", "CVACT02Y");
        m.put("CARDXREF", "CVACT03Y");
        m.put("TCATBALF", "CVTRA01Y");
        m.put("DALYTRAN", "CVTRA06Y");
        m.put("USRSEC", "CSUSR01Y");
        return m;
    }

    static final Map<String, String> ASCII_TWINS = Map.of("TRANTYPE", "trantype.txt", "TRANCATG", "trancatg.txt",
            "DISCGRP", "discgrp.txt", "CUSTDATA", "custdata.txt", "ACCTDATA", "acctdata.txt", "CARDDATA",
            "carddata.txt", "CARDXREF", "cardxref.txt", "TCATBALF", "tcatbal.txt", "DALYTRAN", "dailytran.txt");

    @BeforeAll
    static void migrate() {
        DriverManagerDataSource ds = new DriverManagerDataSource(postgres.getJdbcUrl(), postgres.getUsername(),
                postgres.getPassword());
        flyway = Flyway.configure().dataSource(ds).load();
        flyway.migrate();
        jdbc = new JdbcTemplate(ds);
    }

    @BeforeEach
    void empty() {
        jdbc.execute("truncate user_security, customer, account, card, card_xref, transaction_type,"
                + " transaction_category, disclosure_group, tran_cat_balance, transaction, daily_transaction,"
                + " batch_output_file cascade");
    }

    @Test
    void flywayAppliesTheCoreSchemaOnTopOfTheBatchRepository() {
        assertThat(flyway.info().current().getVersion().getVersion()).isEqualTo("4");
        List<String> tables = jdbc.queryForList("select table_name from information_schema.tables"
                + " where table_schema = 'public' and table_name not like 'batch_job%'"
                + " and table_name not like 'batch_step%' and table_name <> 'flyway_schema_history' order by 1",
                String.class);
        assertThat(tables).containsExactly("account", "batch_output_file", "batch_run", "card", "card_xref", "customer",
                "daily_transaction", "disclosure_group", "tran_cat_balance", "transaction", "transaction_category",
                "transaction_type", "user_security");
    }

    @Test
    void everyMappedFieldHasItsAdrTypeNotNullAndCopybookComment() {
        Map<String, RecordLayout> layouts = CopybookColumnMap.layouts();
        int checked = 0;
        for (Entry e : CopybookColumnMap.entries()) {
            if (!e.stored()) {
                continue;
            }
            Field f = leaf(layouts.get(e.copybook()), e.field());
            Map<String, Object> col = jdbc.queryForMap("""
                    select format_type(a.atttypid, a.atttypmod) as type, a.attnotnull as notnull,
                           col_description(a.attrelid, a.attnum) as comment
                    from pg_attribute a where a.attrelid = ?::regclass and a.attname = ? and not a.attisdropped
                    """, e.table(), e.column());
            assertThat(col.get("type")).as(e.qualifiedColumn()).isEqualTo(CopybookColumnMap.sqlType(f));
            assertThat(col.get("notnull")).as(e.qualifiedColumn()).isEqualTo(true);
            assertThat(col.get("comment")).as(e.qualifiedColumn()).isEqualTo(e.field());
            checked++;
        }
        assertThat(checked).isEqualTo(83);
        assertThat(jdbc.queryForObject("select count(*) from information_schema.columns where table_schema = 'public'"
                + " and data_type in ('real', 'double precision', 'money')", Integer.class))
                .as("ADR-0004: no binary floating point").isZero();
    }

    @Test
    void tablesCarryTheirVsamCopybookName() {
        for (Entry e : CopybookColumnMap.entries()) {
            if (e.stored()) {
                assertThat(jdbc.queryForObject("select obj_description(?::regclass, 'pg_class')", String.class,
                        e.table())).isEqualTo(e.dataset() + " (" + e.copybook() + ")");
            }
        }
    }

    @Test
    void primaryKeysMatchTheVsamRecordKeys() {
        Map<String, List<String>> expected = new LinkedHashMap<>();
        expected.put("user_security", List.of("usr_id"));
        expected.put("customer", List.of("cust_id"));
        expected.put("account", List.of("acct_id"));
        expected.put("card", List.of("card_num"));
        expected.put("card_xref", List.of("card_num"));
        expected.put("transaction", List.of("tran_id"));
        expected.put("daily_transaction", List.of("record_seq"));
        expected.put("tran_cat_balance", List.of("acct_id", "tran_type_cd", "tran_cat_cd"));
        expected.put("disclosure_group", List.of("acct_group_id", "tran_type_cd", "tran_cat_cd"));
        expected.put("transaction_type", List.of("tran_type_cd"));
        expected.put("transaction_category", List.of("tran_type_cd", "tran_cat_cd"));
        expected.forEach((table, key) -> assertThat(indexColumns(table + "_pk")).as(table).isEqualTo(key));
    }

    @Test
    void alternateIndexPathsAndBrowseOrdersHaveIndexes() {
        assertThat(indexColumns("card_acct_id_ix")).as("CARDAIX").containsExactly("acct_id", "card_num");
        assertThat(indexColumns("card_xref_acct_id_ix")).as("CXACAIX").containsExactly("acct_id", "card_num");
        assertThat(indexColumns("transaction_proc_ts_ix")).as("TRANSACT AIX").containsExactly("proc_ts", "tran_id");
        assertThat(indexColumns("daily_transaction_tran_id_ix")).containsExactly("tran_id");
    }

    @Test
    void xrefIsAJunctionOfCardCustomerAndAccount() {
        Set<String> fks = jdbc.queryForList("""
                select a.attname || '->' || c.confrelid::regclass from pg_constraint c
                join pg_attribute a on a.attrelid = c.conrelid and a.attnum = any (c.conkey)
                where c.conrelid = 'card_xref'::regclass and c.contype = 'f'
                """, String.class).stream().collect(Collectors.toSet());
        assertThat(fks).containsExactlyInAnyOrder("card_num->card", "cust_id->customer", "acct_id->account");
    }

    @Test
    void onlyOnlineUpdatedTablesHaveAVersionColumn() {
        assertThat(jdbc.queryForList("select table_name from information_schema.columns where table_schema = 'public'"
                + " and column_name = 'version' and table_name not like 'batch\\_%'"
                + " and table_name <> 'flyway_schema_history' order by 1", String.class))
                .containsExactly("account", "card", "customer", "user_security");
    }

    @ParameterizedTest
    @ValueSource(strings = {"EBCDIC", "ASCII"})
    void everySampleRecordLoadsWithZeroRejects(String encodingName) {
        RecordEncoding encoding = encodingName.equals("EBCDIC") ? RecordEncoding.EBCDIC : RecordEncoding.ASCII;
        int rows = 0;
        for (Map.Entry<String, String> sample : SAMPLES.entrySet()) {
            String dataset = sample.getKey();
            if (encoding.equals(RecordEncoding.ASCII) && !ASCII_TWINS.containsKey(dataset)) {
                continue;
            }
            List<FixedWidthRecord> records = read(dataset, sample.getValue(), encoding);
            int loaded = insert(sample.getValue(), records);
            assertThat(loaded).as(dataset + " " + encoding).isEqualTo(records.size()).isPositive();
            rows += loaded;
        }
        int expected = encoding.equals(RecordEncoding.ASCII) ? 0 : 10;
        expected += 7 + 18 + 51 + 50 + 50 + 50 + 50 + 50 + 300;
        assertThat(rows).isEqualTo(expected);
        assertThat(jdbc.queryForObject("select addr_zip from account where acct_id = 49", String.class))
                .as("ACCTDATA record 49 zip: ZEROAPR in EBCDIC, A000000000 in ASCII (expected codec diff)")
                .isEqualTo(encoding.equals(RecordEncoding.EBCDIC) ? "ZEROAPR" : "A000000000");
        assertThat(jdbc.queryForObject("select count(*) from account where open_date_dt is null"
                + " or expiration_date_dt is null or reissue_date_dt is null", Integer.class)).isZero();
        assertThat(jdbc.queryForObject("select count(*) from card where expiration_date_dt is null", Integer.class))
                .isZero();
        assertThat(jdbc.queryForObject("select count(*) from customer where dob_dt is null", Integer.class)).isZero();
    }

    @Test
    void dailyTransactionsFitTheTransactionTable() {
        int loaded = insertInto("transaction", "CVTRA06Y", read("DALYTRAN", "CVTRA06Y", RecordEncoding.EBCDIC));
        assertThat(loaded).isEqualTo(300);
        List<String> page = jdbc.queryForList("select tran_id from transaction where (proc_ts, tran_id) > (?, ?)"
                + " order by proc_ts, tran_id limit 10", String.class, "", "");
        assertThat(page).hasSize(10);
    }

    @Test
    void dailyFileMayRepeatATransactionId() {
        List<FixedWidthRecord> daily = read("DALYTRAN", "CVTRA06Y", RecordEncoding.EBCDIC);
        List<FixedWidthRecord> repeated = List.of(daily.get(0), daily.get(0));
        assertThat(insertInto("daily_transaction", "CVTRA06Y", repeated)).isEqualTo(2);
        assertThat(jdbc.queryForList("select record_seq from daily_transaction order by 1", Integer.class))
                .containsExactly(1, 2);
    }

    @Test
    void aReferencedDatasetCanBeRefreshedInOneTransaction() {
        for (Map.Entry<String, String> sample : SAMPLES.entrySet()) {
            insert(sample.getValue(), read(sample.getKey(), sample.getValue(), RecordEncoding.EBCDIC));
        }
        TransactionTemplate tx = new TransactionTemplate(new DataSourceTransactionManager(jdbc.getDataSource()));
        tx.executeWithoutResult(status -> {
            jdbc.update("delete from account");
            insert("CVACT01Y", read("ACCTDATA", "CVACT01Y", RecordEncoding.EBCDIC));
        });
        assertThat(jdbc.queryForObject("select count(*) from account", Integer.class)).isEqualTo(50);
        assertThatThrownBy(() -> tx.executeWithoutResult(status -> jdbc.update("delete from account")))
                .as("XREF still references the accounts at commit").rootCause()
                .hasMessageContaining("card_xref_account_fk");
        assertThat(jdbc.queryForObject("select count(*) from card_xref", Integer.class)).isEqualTo(50);
    }

    @Test
    void levelEightyEightChecksRejectUndefinedCodes() {
        assertThatThrownBy(() -> jdbc.update(
                "insert into user_security values ('X', 'F', 'L', 'P', 'X')"))
                .isInstanceOf(DataIntegrityViolationException.class)
                .hasMessageContaining("user_security_usr_type_ck");
        jdbc.update("insert into user_security (usr_id, first_name, last_name, password, usr_type)"
                + " values ('ADMIN001', 'F', 'L', 'P', 'A')");
        assertThat(jdbc.queryForObject("select version from user_security where usr_id = 'ADMIN001'", Long.class))
                .isZero();
    }

    @Test
    void textDatesConvertOnlyWhenTheyAreCalendarDates() {
        assertThat(jdbc.queryForObject("select cobol_iso_date('2022-07-06')::text", String.class))
                .isEqualTo("2022-07-06");
        assertThat(jdbc.queryForObject("select cobol_iso_date('2022-02-30')", String.class)).isNull();
        assertThat(jdbc.queryForObject("select cobol_iso_date('          ')", String.class)).isNull();
        assertThat(jdbc.queryForObject("select cobol_iso_date('20220706')", String.class)).isNull();
        assertThat(jdbc.queryForObject("select cobol_iso_date('0000-01-01')", String.class)).as("no year 0 / BC")
                .isNull();
    }

    private static List<FixedWidthRecord> read(String dataset, String copybook, RecordEncoding encoding) {
        RecordLayout layout = Copybook.layout(copybook);
        return encoding.equals(RecordEncoding.EBCDIC)
                ? RecordFiles.readFixed(dataset, TestData.resolve("app/data/EBCDIC/AWS.M2.CARDDEMO." + dataset + ".PS"),
                        layout, encoding)
                : RecordFiles.readLines(dataset, TestData.resolve("app/data/ASCII/" + ASCII_TWINS.get(dataset)),
                        layout, encoding);
    }

    private static int insert(String copybook, List<FixedWidthRecord> records) {
        String table = CopybookColumnMap.forCopybook(copybook).stream().filter(Entry::stored).findFirst()
                .orElseThrow().table();
        return insertInto(table, copybook, records);
    }

    /** ADR-0003 load: text stored with trailing spaces trimmed, numerics as exact decimals. */
    private static int insertInto(String table, String copybook, List<FixedWidthRecord> records) {
        RecordLayout layout = Copybook.layout(copybook);
        List<Entry> stored = CopybookColumnMap.forCopybook(copybook).stream().filter(Entry::stored).toList();
        List<Field> fields = stored.stream().map(e -> leaf(layout, e.field())).toList();
        boolean sequenced = table.equals("daily_transaction");
        String sql = "insert into " + table + " (" + (sequenced ? "record_seq, " : "") + stored.stream()
                .map(Entry::column).collect(Collectors.joining(", ")) + ") values (" + (sequenced ? "?, " : "")
                + stored.stream().map(e -> "?").collect(Collectors.joining(", ")) + ")";
        List<Object[]> batch = new ArrayList<>();
        for (FixedWidthRecord r : records) {
            Object[] row = new Object[fields.size() + (sequenced ? 1 : 0)];
            int base = 0;
            if (sequenced) {
                row[0] = batch.size() + 1;
                base = 1;
            }
            for (int i = 0; i < fields.size(); i++) {
                Object v = r.get(fields.get(i));
                row[base + i] = v instanceof String s ? s.stripTrailing() : v;
                if (v instanceof BigDecimal d && d.scale() <= 0) {
                    row[base + i] = d.longValueExact();
                }
            }
            batch.add(row);
        }
        int loaded = 0;
        for (int n : jdbc.batchUpdate(sql, batch)) {
            loaded += n;
        }
        return loaded;
    }

    private static Field leaf(RecordLayout layout, String key) {
        return layout.leaves().stream().filter(l -> l.key().equals(key)).findFirst().orElseThrow().field();
    }

    private static List<String> indexColumns(String index) {
        return jdbc.queryForList("""
                select a.attname from pg_index i
                cross join lateral unnest(i.indkey) with ordinality as k(attnum, n)
                join pg_attribute a on a.attrelid = i.indrelid and a.attnum = k.attnum
                where i.indexrelid = ?::regclass order by k.n
                """, String.class, index);
    }
}
