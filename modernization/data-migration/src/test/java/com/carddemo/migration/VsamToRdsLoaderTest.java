package com.carddemo.migration;

import static org.assertj.core.api.Assertions.assertThat;

import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.Test;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

/** End-to-end load of the shipped EBCDIC unloads into the Flyway-managed schema. */
class VsamToRdsLoaderTest {

    @Test
    void loadsAllUnloadsAndReconciles() throws Exception {
        String url = "jdbc:h2:mem:loader;MODE=PostgreSQL;DATABASE_TO_LOWER=TRUE;DB_CLOSE_DELAY=-1";
        Flyway.configure().dataSource(url, "sa", "").locations("classpath:db/migration").load().migrate();
        try (Connection c = DriverManager.getConnection(url, "sa", "")) {
            LoadReport report = new VsamToRdsLoader().load(c, LegacyDataParityTest.DATA, SourceEncoding.EBCDIC);

            assertThat(report.getCounts())
                    .containsEntry("account", 50)
                    .containsEntry("card", 50)
                    .containsEntry("customer", 50)
                    .containsEntry("card_xref", 50)
                    .containsEntry("tran_cat_balance", 50)
                    .containsEntry("daily_transaction", 300)
                    .containsEntry("transaction_type", 7)
                    .containsEntry("transaction_category", 18)
                    .containsEntry("disclosure_group", 51)
                    .containsEntry("user_security", 10);
            assertThat(scalar(c, "select count(*) from account")).isEqualTo("50");
            assertThat(scalar(c, "select user_type from user_security where user_id = 'ADMIN001'")).isEqualTo("A");
            String hash = scalar(c, "select password_hash from user_security where user_id = 'USER0001'");
            assertThat(new BCryptPasswordEncoder().matches("PASSWORD", hash)).isTrue();

            long maxDaily = Long.parseLong(scalar(c, "select max(tran_id) from daily_transaction").trim());
            assertThat(Long.parseLong(scalar(c, "select nextval('tran_id_seq')"))).isEqualTo(maxDaily + 1);

            String sourceTotal = report.getControlTotals().get("account.curr_bal").toPlainString();
            assertThat(scalar(c, "select sum(curr_bal) from account")).isEqualTo(sourceTotal);
        }
    }

    private static String scalar(Connection c, String sql) throws Exception {
        try (Statement st = c.createStatement(); ResultSet rs = st.executeQuery(sql)) {
            rs.next();
            return rs.getString(1);
        }
    }

    @Test
    void failedTruncatingReloadKeepsPreviousData() throws Exception {
        String url = "jdbc:h2:mem:loader-reload;MODE=PostgreSQL;DATABASE_TO_LOWER=TRUE;DB_CLOSE_DELAY=-1";
        Flyway.configure().dataSource(url, "sa", "").locations("classpath:db/migration").load().migrate();
        try (Connection c = DriverManager.getConnection(url, "sa", "")) {
            VsamToRdsLoader loader = new VsamToRdsLoader();
            loader.load(c, LegacyDataParityTest.DATA, SourceEncoding.EBCDIC);
            org.assertj.core.api.Assertions.assertThatThrownBy(
                    () -> loader.load(c, Path.of("does-not-exist"), SourceEncoding.EBCDIC, true))
                    .isInstanceOf(java.io.IOException.class);
            assertThat(scalar(c, "select count(*) from account")).isEqualTo("50");
            assertThat(scalar(c, "select count(*) from user_security")).isEqualTo("10");
        }
    }

    @Test
    void truncatingReloadRequiresTransactUnloadUnlessSampleMode() throws Exception {
        String url = "jdbc:h2:mem:loader-transact;MODE=PostgreSQL;DATABASE_TO_LOWER=TRUE;DB_CLOSE_DELAY=-1";
        Flyway.configure().dataSource(url, "sa", "").locations("classpath:db/migration").load().migrate();
        try (Connection c = DriverManager.getConnection(url, "sa", "")) {
            VsamToRdsLoader loader = new VsamToRdsLoader();
            loader.load(c, LegacyDataParityTest.DATA, SourceEncoding.EBCDIC);
            org.assertj.core.api.Assertions.assertThatThrownBy(
                    () -> loader.load(c, LegacyDataParityTest.DATA, SourceEncoding.EBCDIC, true))
                    .hasMessageContaining("card_transaction");
            assertThat(scalar(c, "select count(*) from account")).isEqualTo("50");
            assertThat(loader.load(c, LegacyDataParityTest.DATA, SourceEncoding.EBCDIC, true, true).getCounts())
                    .containsEntry("account", 50);
        }
    }
}
