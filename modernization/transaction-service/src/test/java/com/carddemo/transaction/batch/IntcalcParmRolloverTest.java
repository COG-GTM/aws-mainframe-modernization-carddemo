package com.carddemo.transaction.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchRun;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordReader;
import com.carddemo.common.record.layout.TranRecordLayout;
import com.carddemo.transaction.domain.CategoryBalance;
import com.carddemo.transaction.domain.CategoryBalanceId;
import com.carddemo.transaction.domain.DisclosureGroup;
import com.carddemo.transaction.domain.DisclosureGroupId;
import com.carddemo.transaction.repository.CategoryBalanceRepository;
import com.carddemo.transaction.repository.DisclosureGroupRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.support.StubGateways;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Primary;

/** An undated INTCALC run whose steps straddle midnight still writes its own transactions to SYSTRAN. */
@SpringBootTest
@Import({StubGateways.class, IntcalcParmRolloverTest.DayPerReadClock.class})
class IntcalcParmRolloverTest {

    private static final long ACCOUNT = 11111111111L;

    @TempDir
    Path work;

    @Autowired
    private BatchJobLauncher launcher;

    @Autowired
    private TransactionRepository transactions;

    @Autowired
    private CategoryBalanceRepository categoryBalances;

    @Autowired
    private DisclosureGroupRepository disclosureGroups;

    @Autowired
    private StubGateways.StubCardGateway cards;

    @Autowired
    private StubGateways.StubAccountGateway accounts;

    @Test
    void systranUsesThePARMTheCalculationStepResolved() throws Exception {
        transactions.deleteAll();
        categoryBalances.deleteAll();
        disclosureGroups.deleteAll();
        cards.clear();
        accounts.clear();
        cards.register("4111111111111111", 100000001L, ACCOUNT);
        accounts.register(ACCOUNT, "", new BigDecimal("1200.00"));
        disclosureGroups.save(new DisclosureGroup(new DisclosureGroupId("DEFAULT", "01", 1), new BigDecimal("12.00")));
        categoryBalances.save(new CategoryBalance(new CategoryBalanceId(ACCOUNT, "01", 1), new BigDecimal("1200.00")));

        BatchRun run = launcher.launch(new BatchArguments("INTCALC", null, null, work.resolve("out"), Map.of(),
                null, null));

        assertThat(run.returnCode()).isZero();
        String parm = (String) ((Map<?, ?>) run.summary().get("results")).get("parm");
        assertThat(RecordReader.readAll(run.outputDir().resolve("SYSTRAN"), TranRecordLayout.LAYOUT, CodePage.ASCII,
                RecordFormat.FIXED)).extracting(r -> r.get(TranRecordLayout.TRAN_ID))
                .containsExactly(parm + "000001");
    }

    /** Every read of the clock is one day later than the previous one. Unannotated: imported only here. */
    static class DayPerReadClock {

        @Bean
        @Primary
        Clock dayPerReadClock() {
            AtomicInteger reads = new AtomicInteger();
            Instant start = Instant.parse("2022-07-18T23:59:59Z");
            return new Clock() {
                @Override
                public ZoneId getZone() {
                    return ZoneOffset.UTC;
                }

                @Override
                public Clock withZone(ZoneId zone) {
                    return this;
                }

                @Override
                public Instant instant() {
                    return start.plus(Duration.ofDays(reads.getAndIncrement()));
                }
            };
        }
    }
}
