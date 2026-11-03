package com.carddemo.transaction.batch;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.transaction.domain.CategoryBalance;
import com.carddemo.transaction.domain.CategoryBalanceId;
import com.carddemo.transaction.domain.DisclosureGroup;
import com.carddemo.transaction.domain.DisclosureGroupId;
import com.carddemo.transaction.repository.CategoryBalanceRepository;
import com.carddemo.transaction.repository.DisclosureGroupRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.support.StubGateways;
import java.math.BigDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;

/** Parity tests for the CBACT04C port. */
@SpringBootTest
@Import(StubGateways.class)
class InterestCalculationServiceTest {

    private static final String CARD = "4111111111111111";
    private static final long ACCOUNT = 11111111111L;

    @Autowired
    private InterestCalculationService interestCalculationService;

    @Autowired
    private CategoryBalanceRepository categoryBalances;

    @Autowired
    private DisclosureGroupRepository disclosureGroups;

    @Autowired
    private TransactionRepository transactions;

    @Autowired
    private StubGateways.StubCardGateway cards;

    @Autowired
    private StubGateways.StubAccountGateway accounts;

    @BeforeEach
    void setUp() {
        transactions.deleteAll();
        categoryBalances.deleteAll();
        disclosureGroups.deleteAll();
        cards.clear();
        accounts.clear();
        cards.register(CARD, 100000001L, ACCOUNT);
        categoryBalances.save(new CategoryBalance(new CategoryBalanceId(ACCOUNT, "01", 1),
                new BigDecimal("1200.00")));
    }

    @Test
    void computesMonthlyInterestAsBalanceTimesRateOver1200() {
        accounts.register(ACCOUNT, "GOLD", new BigDecimal("1200.00"));
        disclosureGroups.save(new DisclosureGroup(new DisclosureGroupId("GOLD", "01", 1),
                new BigDecimal("12.00")));

        InterestCalculationService.InterestReport report = interestCalculationService.calculateInterest();

        assertThat(report.totalInterest()).isEqualByComparingTo("12.00");
        // The only account is also the last one, which CBACT04C never rewrites.
        assertThat(accounts.settlements()).isEmpty();
        assertThat(transactions.findAll()).singleElement().satisfies(transaction -> {
            assertThat(transaction.getTypeCd()).isEqualTo("01");
            assertThat(transaction.getCatCd()).isEqualTo(5);
            assertThat(transaction.getSource()).isEqualTo("System");
        });
        // CBACT04C opens TCATBAL as INPUT: the category principal survives the run.
        assertThat(categoryBalances.findById(new CategoryBalanceId(ACCOUNT, "01", 1)).orElseThrow()
                .getBalance()).isEqualByComparingTo("1200.00");
    }

    @Test
    void truncatesTheMonthlyInterestLikeTheCobolComputeWithoutRounded() {
        // 1234.56 * 12.00 / 1200 = 12.3456: PIC S9(09)V99 keeps 12.34 (HALF_UP would give 12.35).
        categoryBalances.deleteAll();
        categoryBalances.save(new CategoryBalance(new CategoryBalanceId(ACCOUNT, "01", 1),
                new BigDecimal("1234.56")));
        accounts.register(ACCOUNT, "GOLD", new BigDecimal("1234.56"));
        disclosureGroups.save(new DisclosureGroup(new DisclosureGroupId("GOLD", "01", 1),
                new BigDecimal("12.00")));

        InterestCalculationService.InterestReport report = interestCalculationService.calculateInterest();

        assertThat(report.totalInterest()).isEqualByComparingTo("12.34");
        assertThat(transactions.findAll()).singleElement()
                .satisfies(transaction -> assertThat(transaction.getAmount()).isEqualByComparingTo("12.34"));
    }

    @Test
    void failsTheRunWhenAnAccountHasNoCardCrossReference() {
        accounts.register(ACCOUNT, "GOLD", new BigDecimal("1200.00"));
        disclosureGroups.save(new DisclosureGroup(new DisclosureGroupId("GOLD", "01", 1),
                new BigDecimal("12.00")));
        cards.clear();

        assertThatThrownBy(() -> interestCalculationService.calculateInterest())
                .isInstanceOf(IllegalStateException.class);

        assertThat(accounts.settlements()).isEmpty();
    }

    @Test
    void fallsBackToTheDefaultDisclosureGroup() {
        accounts.register(ACCOUNT, "NOSUCH", new BigDecimal("1200.00"));
        disclosureGroups.save(new DisclosureGroup(
                new DisclosureGroupId(DisclosureGroup.DEFAULT_GROUP_ID, "01", 1),
                new BigDecimal("24.00")));

        InterestCalculationService.InterestReport report = interestCalculationService.calculateInterest();

        assertThat(report.totalInterest()).isEqualByComparingTo("24.00");
    }

    @Test
    void settlesAZeroRateAccountWithoutWritingATransactionSoItsCyclesStillReset() {
        long last = 22222222222L;
        accounts.register(ACCOUNT, "NOSUCH", new BigDecimal("1200.00"));
        accounts.register(last, "NOSUCH", new BigDecimal("0.00"));
        cards.register("4222222222222222", 100000002L, last);
        categoryBalances.save(new CategoryBalance(new CategoryBalanceId(last, "01", 1), BigDecimal.ZERO));
        disclosureGroups.save(new DisclosureGroup(
                new DisclosureGroupId(DisclosureGroup.DEFAULT_GROUP_ID, "01", 1), new BigDecimal("0.00")));

        InterestCalculationService.InterestReport report = interestCalculationService.calculateInterest("2022071800");

        assertThat(report.accountsSettled()).isEqualTo(1);
        assertThat(accounts.settledByAccount()).containsOnlyKeys(ACCOUNT);
        assertThat(report.totalInterest()).isEqualByComparingTo("0.00");
        // 1050-UPDATE-ACCOUNT runs at every account break, so the cycle buckets reset regardless.
        assertThat(accounts.settlements()).containsExactly(BigDecimal.ZERO);
        assertThat(transactions.count()).isZero();
    }

    @Test
    void abendsWhenTheDefaultDisclosureGroupIsMissingToo() {
        // 1200-A-GET-DEFAULT-INT-RATE: a DEFAULT read that is not status 00 abends the program.
        accounts.register(ACCOUNT, "NOSUCH", new BigDecimal("1200.00"));

        assertThatThrownBy(() -> interestCalculationService.calculateInterest("2022071800"))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("DEFAULT DISCLOSURE GROUP");
        assertThat(accounts.settlements()).isEmpty();
    }

    @Test
    void abendsWhenACategoryBalanceHasNoAccount() {
        // 1100-GET-ACCT-DATA: INVALID KEY -> ACCOUNT NOT FOUND -> 9999-ABEND-PROGRAM.
        assertThatThrownBy(() -> interestCalculationService.calculateInterest("2022071800"))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("ACCOUNT NOT FOUND");
        assertThat(transactions.count()).isZero();
    }

    @Test
    void writesOneTransactionPerRatedCategoryLike1300BWriteTx() {
        long second = 22222222222L;
        accounts.register(ACCOUNT, "", new BigDecimal("1200.00"));
        accounts.register(second, "", new BigDecimal("0.00"));
        cards.register("4222222222222222", 100000002L, second);
        categoryBalances.save(new CategoryBalance(new CategoryBalanceId(ACCOUNT, "02", 1), new BigDecimal("50.00")));
        categoryBalances.save(new CategoryBalance(new CategoryBalanceId(ACCOUNT, "03", 1), new BigDecimal("-987.65")));
        categoryBalances.save(new CategoryBalance(new CategoryBalanceId(second, "01", 1), new BigDecimal("100.00")));
        disclosureGroups.save(new DisclosureGroup(
                new DisclosureGroupId(DisclosureGroup.DEFAULT_GROUP_ID, "01", 1), new BigDecimal("15.00")));
        disclosureGroups.save(new DisclosureGroup(
                new DisclosureGroupId(DisclosureGroup.DEFAULT_GROUP_ID, "02", 1), new BigDecimal("0.00")));
        disclosureGroups.save(new DisclosureGroup(
                new DisclosureGroupId(DisclosureGroup.DEFAULT_GROUP_ID, "03", 1), new BigDecimal("25.00")));

        InterestCalculationService.InterestReport report = interestCalculationService.calculateInterest("2022071800");

        // 1200.00 * 15 / 1200 = 15.00; 02/0001 has rate 0 (no transaction); -987.65 * 25 / 1200 = -20.576 -> -20.57.
        assertThat(report.transactionsWritten()).isEqualTo(3);
        // Only the first account is rewritten: the loop ends on EOF before the last account's 1050-UPDATE-ACCOUNT.
        assertThat(report.accountsSettled()).isEqualTo(1);
        assertThat(report.totalInterest()).isEqualByComparingTo("-4.32");
        assertThat(accounts.settledByAccount()).containsExactly(org.assertj.core.api.Assertions.entry(
                ACCOUNT, new BigDecimal("-5.57")));
        assertThat(transactions.findAll(org.springframework.data.domain.Sort.by("tranId")))
                .satisfiesExactly(
                        t -> {
                            assertThat(t.getTranId()).isEqualTo("2022071800000001");
                            assertThat(t.getAmount()).isEqualByComparingTo("15.00");
                            assertThat(t.getDescription()).isEqualTo("Int. for a/c 11111111111");
                            assertThat(t.getCardNum()).isEqualTo(CARD);
                            assertThat(t.getMerchantId()).isZero();
                            assertThat(t.getMerchantName()).isEmpty();
                            assertThat(t.getMerchantCity()).isEmpty();
                            assertThat(t.getMerchantZip()).isEmpty();
                        },
                        t -> {
                            assertThat(t.getTranId()).isEqualTo("2022071800000002");
                            assertThat(t.getAmount()).isEqualByComparingTo("-20.57");
                        },
                        t -> {
                            assertThat(t.getTranId()).isEqualTo("2022071800000003");
                            assertThat(t.getAmount()).isEqualByComparingTo("1.25");
                            assertThat(t.getDescription()).isEqualTo("Int. for a/c 22222222222");
                        });
    }

    @Test
    void refusesAParmWhoseTranIdsAreAlreadyInTransact() {
        accounts.register(ACCOUNT, "", new BigDecimal("1200.00"));
        disclosureGroups.save(new DisclosureGroup(
                new DisclosureGroupId(DisclosureGroup.DEFAULT_GROUP_ID, "01", 1), new BigDecimal("15.00")));
        interestCalculationService.calculateInterest("2022071800");

        assertThatThrownBy(() -> interestCalculationService.calculateInterest("2022071800"))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("2022071800");
        assertThat(transactions.count()).isEqualTo(1);
        assertThat(accounts.settlements()).isEmpty();
    }
}
