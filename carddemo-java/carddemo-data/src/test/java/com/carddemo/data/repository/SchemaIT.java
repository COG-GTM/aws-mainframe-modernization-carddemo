package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;

/** V1 schema: tables, VSAM alternate indexes and the PIC-range checks. */
class SchemaIT extends RepositoryIT {

    @Autowired
    JdbcTemplate jdbc;

    @Test
    void createsTheElevenCoreTables() {
        List<String> tables = jdbc.queryForList(
                "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'"
                        + " AND table_name <> 'flyway_schema_history' ORDER BY table_name", String.class);

        assertThat(tables).containsExactly("account", "app_user", "card", "card_xref", "customer",
                "daily_transaction", "disclosure_group", "tran_cat_balance", "tran_category", "tran_type",
                "transaction");
    }

    @Test
    void mapsEachVsamAlternateIndexToASecondaryIndex() {
        assertThat(indexDef("card_acct_id_aix")).contains("ON public.card USING btree (acct_id)");
        assertThat(indexDef("card_xref_acct_id_aix")).contains("ON public.card_xref USING btree (acct_id)");
        assertThat(indexDef("transaction_proc_ts_aix")).contains("ON public.transaction USING btree (proc_ts)");
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', textBlock = """
            INSERT INTO card VALUES ('1', 100000000000, 1, 'X', NULL, 'Y')        | card_acct_id_pic
            INSERT INTO card VALUES ('1', 1, 1000, 'X', NULL, 'Y')                | card_cvv_cd_pic
            INSERT INTO tran_cat_balance VALUES (1, '01', 10000, 0)               | tran_cat_balance_cat_cd_pic
            INSERT INTO tran_cat_balance VALUES (1, '01', 1, 1000000000.00)       | numeric field overflow
            INSERT INTO tran_type VALUES ('001', 'too long')                      | character varying(2)
            INSERT INTO app_user VALUES ('U1', 'A', 'B', 'h', 'X')                | app_user_user_type_chk
            """)
    void enforcesPicRanges(String insert, String violation) {
        assertThatThrownBy(() -> jdbc.update(insert))
                .isInstanceOf(DataIntegrityViolationException.class)
                .hasMessageContaining(violation);
    }

    @Test
    void rejectsDuplicatePrimeKeysLikeVsamDuprec() {
        jdbc.update("INSERT INTO tran_type VALUES ('01', 'Purchase')");

        assertThatThrownBy(() -> jdbc.update("INSERT INTO tran_type VALUES ('01', 'Again')"))
                .isInstanceOf(DuplicateKeyException.class);
    }

    @Test
    void recordsV1InFlywayHistory() {
        assertThat(jdbc.queryForList(
                "SELECT version FROM flyway_schema_history WHERE success ORDER BY installed_rank", String.class))
                .containsExactly("0", "1");
    }

    private String indexDef(String name) {
        return jdbc.queryForObject("SELECT indexdef FROM pg_indexes WHERE indexname = ?", String.class, name);
    }
}
