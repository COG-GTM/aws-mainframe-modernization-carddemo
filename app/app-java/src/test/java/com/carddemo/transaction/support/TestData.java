package com.carddemo.transaction.support;

import com.carddemo.transaction.cobol.Pic;
import com.carddemo.transaction.cobol.ZonedDecimal;
import com.carddemo.transaction.io.SeedDataLoader;
import com.carddemo.transaction.repository.AccountRepository;
import com.carddemo.transaction.repository.CardRepository;
import com.carddemo.transaction.repository.CardXrefRepository;
import com.carddemo.transaction.repository.CustomerRepository;
import com.carddemo.transaction.repository.TransactionCategoryBalanceRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import com.carddemo.transaction.CardDemoProperties;
import org.springframework.context.annotation.Primary;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;

/**
 * Resets the store to the app/data/ASCII sample extracts and builds DALYTRAN fixtures.
 */
@TestConfiguration
public class TestData {

    public static final Path SAMPLE_DIR = Path.of("../data/ASCII");
    public static final Path SAMPLE_DALYTRAN = SAMPLE_DIR.resolve("dailytran.txt");
    public static final Instant FIXED_NOW = Instant.parse("2026-10-27T10:15:30.123Z");

    @Bean
    @Primary
    public Clock fixedClock() {
        return Clock.fixed(FIXED_NOW, ZoneOffset.UTC);
    }

    @Bean
    public Fixtures fixtures(CardDemoProperties props, TransactionRepository t, TransactionCategoryBalanceRepository b,
                             CardXrefRepository x, CardRepository c, CustomerRepository cu, AccountRepository a,
                             JobLauncher launcher) {
        SeedDataLoader seed = new SeedDataLoader(props, a, c, x, cu, b);
        return new Fixtures(seed, t, b, x, c, cu, a, launcher);
    }

    public record Fixtures(SeedDataLoader seed, TransactionRepository transactions,
                           TransactionCategoryBalanceRepository tcatbals, CardXrefRepository xrefs,
                           CardRepository cards, CustomerRepository customers, AccountRepository accounts,
                           JobLauncher launcher) {

        /** Empty every table and REPRO the sample extracts back in. */
        public void reseed() {
            transactions.deleteAll();
            tcatbals.deleteAll();
            xrefs.deleteAll();
            cards.deleteAll();
            customers.deleteAll();
            accounts.deleteAll();
            seed.run(null);
        }

        public JobExecution run(Job job, Path dalytran) throws Exception {
            return launcher.run(job, new JobParametersBuilder()
                    .addString("dalytran", dalytran.toString())
                    .addLong("run", System.nanoTime())
                    .toJobParameters());
        }
    }

    /** Builds one 350-byte DALYTRAN-RECORD (CVTRA06Y). */
    public static String dalytran(String id, String typeCd, int catCd, BigDecimal amt, String cardNum, String origTs) {
        String rec = Pic.x(id, 16) + Pic.x(typeCd, 2) + Pic.zeroFill(catCd, 4) + Pic.x("POS TERM", 10)
                + Pic.x("Test purchase " + id, 100) + ZonedDecimal.encode(amt, 9, 2) + Pic.zeroFill(800000000, 9)
                + Pic.x("Test Merchant", 50) + Pic.x("Test City", 50) + Pic.x("12345", 10) + Pic.x(cardNum, 16)
                + Pic.x(origTs, 26) + Pic.x("", 26);
        return Pic.x(rec, 350);
    }

    public static Path writeDalytran(Path dir, List<String> records) throws IOException {
        Path file = dir.resolve("DALYTRAN.txt");
        Files.write(file, records);
        return file;
    }
}
