package com.carddemo.batch.load;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.PostgresTestcontainersConfiguration;
import com.carddemo.data.Transaction;
import com.carddemo.data.TransactionRepository;
import com.carddemo.legacyio.CardDemoLayouts;
import com.carddemo.legacyio.FileRecord;
import com.carddemo.legacyio.LegacyFileCodec;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;

/**
 * Loads the TRANSACT file the COBOL batch writes into PostgreSQL. The fixture is the GnuCOBOL golden-master
 * {@code baselines/COMBTRAN/transact.txt} (sha256 f87e22da...): 262 POSTTRAN records and 50 INTCALC records, whose
 * timestamps CBTRN02C and CBACT04C write in DB2 format ({@code 2022-07-06-12.00.00.000000}).
 */
@SpringBootTest(properties = "spring.batch.job.enabled=false")
@ActiveProfiles("test")
@Import(PostgresTestcontainersConfiguration.class)
class GoldenMasterTransactLoadIT {

    private static final Path TRANSACT = Path.of("src", "test", "resources", "golden-master", "COMBTRAN", "transact.txt");

    private static final LocalDateTime BATCH_CLOCK = LocalDateTime.of(2022, 7, 6, 12, 0);

    @Autowired
    private TransactionRepository transactions;

    @Autowired
    private JdbcTemplate jdbc;

    @AfterEach
    void clear() {
        transactions.deleteAll();
    }

    @Test
    void combtranOutputLoadsWithBothTimestampFormats() throws Exception {
        List<FileRecord> records = LegacyFileCodec.ascii().read(TRANSACT, CardDemoLayouts.TRANSACTION);
        transactions.deleteAll();
        transactions.saveAll(records.stream().map(r -> SampleDataLoader.transaction(r.record())).toList());

        assertThat(transactions.count()).isEqualTo(312);
        assertThat(jdbc.queryForObject("select count(*) from transactions where proc_ts = ?", Integer.class,
                BATCH_CLOCK)).isEqualTo(312);

        Transaction posted = transactions.findById("0000000000683580").orElseThrow();
        assertThat(posted.getOrigTs()).isEqualTo(LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        assertThat(posted.getProcTs()).isEqualTo(BATCH_CLOCK);
        assertThat(posted.getAmount()).isEqualByComparingTo("504.77");
        assertThat(posted.getCardNum()).isEqualTo("4859452612877065");

        Transaction interest = transactions.findById("2022071800000001").orElseThrow();
        assertThat(interest.getTypeCd()).isEqualTo("01");
        assertThat(interest.getCatCd()).isEqualTo(5);
        assertThat(interest.getSource()).isEqualTo("System");
        assertThat(interest.getDescription()).isEqualTo("Int. for a/c 00000000001");
        assertThat(interest.getAmount()).isEqualByComparingTo("14.56");
        assertThat(interest.getOrigTs()).isEqualTo(BATCH_CLOCK);
        assertThat(interest.getProcTs()).isEqualTo(BATCH_CLOCK);
        assertThat(jdbc.queryForObject("select count(*) from transactions where tran_id like '20220718%'",
                Integer.class)).isEqualTo(50);

        BigDecimal sourceTotal = records.stream().map(r -> r.record().getDecimal("TRAN-AMT"))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        assertThat(jdbc.queryForObject("select sum(amount) from transactions", BigDecimal.class))
                .isEqualByComparingTo(sourceTotal);
    }

    @Test
    void timestampAcceptsBothLayouts() {
        assertThat(SampleDataLoader.timestamp("2022-06-10 19:27:53.000000"))
                .isEqualTo(LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        assertThat(SampleDataLoader.timestamp("2022-07-06-12.00.00.123456"))
                .isEqualTo(LocalDateTime.of(2022, 7, 6, 12, 0, 0, 123_456_000));
        assertThat(SampleDataLoader.timestamp("")).isNull();
    }
}
