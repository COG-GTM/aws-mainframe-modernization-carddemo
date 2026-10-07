package com.carddemo.architecture;

import static org.assertj.core.api.Assertions.assertThat;

import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.lang.ArchRule;
import com.tngtech.archunit.lang.EvaluationResult;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * Proves each rule actually fails on a violation, using the deliberately broken classes under {@code archfixture}
 * (an empty production code base would otherwise pass every rule trivially).
 */
class ModularMonolithRulesSelfTest {

    private static final String ROOT = "archfixture";
    private static JavaClasses fixtures;

    @BeforeAll
    static void importFixtures() {
        fixtures = new ClassFileImporter().importPackages(ROOT);
    }

    @Test
    void commonMustNotDependOnADomain() {
        assertThat(violations(ModularMonolithRules.commonDependsOnNoDomain(ROOT)))
                .contains("archfixture.common.LeakyCommon")
                .contains("archfixture.user.UserRecord");
    }

    @Test
    void noDomainMayDependOnTheWebLayer() {
        assertThat(violations(ModularMonolithRules.noDomainDependsOnWeb(ROOT)))
                .contains("archfixture.card.CardCallsWeb")
                .doesNotContain("<archfixture.web.LoginEndpoint.accounts>");
    }

    @Test
    void domainMayOnlyUseItsAllowedDomains() {
        String report = violations(ModularMonolithRules.domainsFollowDependencyMatrix(ROOT));
        assertThat(report)
                .contains("archfixture.customer.CustomerUsesCardInternals")
                .contains("archfixture.account.AccountCallsBatch")
                .doesNotContain("archfixture.account.AccountService");
    }

    @Test
    void internalPackageIsPrivateToItsDomain() {
        assertThat(violations(ModularMonolithRules.internalPackagesArePrivate(ROOT)))
                .contains("archfixture.customer.CustomerUsesCardInternals")
                .contains("archfixture.card.internal.CardStore");
    }

    @Test
    void cyclesBetweenDomainsAreRejected() {
        assertThat(violations(ModularMonolithRules.packagesAreFreeOfCycles(ROOT)))
                .contains("archfixture.account.AccountCallsBatch")
                .contains("archfixture.batch.PostingJob");
    }

    @Test
    void springBatchApiStaysInBatch() {
        assertThat(violations(ModularMonolithRules.springBatchOnlyInBatch(ROOT)))
                .contains("archfixture.user.UserRecord")
                .contains("org.springframework.batch.core.Job")
                .doesNotContain("archfixture.batch.PostingJob");
    }

    @Test
    void binaryFloatingPointIsRejected() {
        assertThat(violations(ModularMonolithRules.noBinaryFloatingPoint(ROOT)))
                .contains("archfixture.transaction.FloatingAmount.amount")
                .contains("archfixture.transaction.FloatingAmount.rate")
                .doesNotContain("archfixture.transaction.FloatingAmount.exact");
    }

    @Test
    void legalDependenciesPassEveryRuleWhenViolatorsAreExcluded() {
        JavaClasses legal = new ClassFileImporter().importClasses(
                archfixture.account.AccountService.class, archfixture.card.CardApi.class,
                archfixture.web.LoginEndpoint.class);
        for (ArchRule rule : ModularMonolithRules.all(ROOT)) {
            assertThat(rule.evaluate(legal).hasViolation()).as(rule.getDescription()).isFalse();
        }
    }

    private static String violations(ArchRule rule) {
        EvaluationResult result = rule.evaluate(fixtures);
        assertThat(result.hasViolation()).as("expected a violation of: " + rule.getDescription()).isTrue();
        return String.join("\n", result.getFailureReport().getDetails());
    }
}
