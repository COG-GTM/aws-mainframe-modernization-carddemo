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
        assertThat(accounts.settlements()).containsExactly(new BigDecimal("12.00"));
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
    void skipsCategoriesWithoutAnyDisclosureRate() {
        accounts.register(ACCOUNT, "NOSUCH", new BigDecimal("1200.00"));

        InterestCalculationService.InterestReport report = interestCalculationService.calculateInterest();

        assertThat(report.accountsSettled()).isZero();
        assertThat(transactions.count()).isZero();
    }
}
