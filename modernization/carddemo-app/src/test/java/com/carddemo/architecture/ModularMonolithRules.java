package com.carddemo.architecture;

import static com.tngtech.archunit.core.domain.JavaClass.Predicates.resideInAnyPackage;
import static com.tngtech.archunit.core.domain.JavaClass.Predicates.resideOutsideOfPackage;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noFields;
import static com.tngtech.archunit.library.dependencies.SlicesRuleDefinition.slices;

import com.tngtech.archunit.lang.ArchRule;
import com.tngtech.archunit.lang.CompositeArchRule;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Module boundaries of the CardDemo modular monolith (ADR-0001), parameterised by root package so the same rules
 * run against production code ({@code com.carddemo}) and against deliberately broken fixtures.
 */
public final class ModularMonolithRules {

    public static final String COMMON = "common";

    /** The web/API layer: controllers, security, DTOs. It may use every domain; nothing below may use it. */
    public static final String WEB = "web";

    /** Domain package to the other domain packages it may use (besides {@code common}). */
    public static final Map<String, Set<String>> ALLOWED_DOMAIN_DEPENDENCIES = allowedDependencies();

    private ModularMonolithRules() {
    }

    private static Map<String, Set<String>> allowedDependencies() {
        Map<String, Set<String>> allowed = new LinkedHashMap<>();
        allowed.put("customer", Set.of());
        allowed.put("user", Set.of());
        allowed.put("card", Set.of());
        allowed.put("account", Set.of("card", "customer"));
        allowed.put("transaction", Set.of("account", "card", "customer"));
        allowed.put("batch", Set.of("customer", "user", "card", "account", "transaction"));
        return Map.copyOf(allowed);
    }

    public static ArchRule commonDependsOnNoDomain(String root) {
        String[] domains = ALLOWED_DOMAIN_DEPENDENCIES.keySet().stream()
                .map(d -> root + "." + d + "..")
                .toArray(String[]::new);
        return noClasses().that().resideInAPackage(root + "." + COMMON + "..")
                .should().dependOnClassesThat().resideInAnyPackage(domains)
                .because("common is the shared kernel (ADR-0001)")
                .allowEmptyShould(true);
    }

    public static ArchRule noDomainDependsOnWeb(String root) {
        String[] below = Stream.concat(Stream.of(COMMON), ALLOWED_DOMAIN_DEPENDENCIES.keySet().stream())
                .map(d -> root + "." + d + "..")
                .toArray(String[]::new);
        return noClasses().that().resideInAnyPackage(below)
                .should().dependOnClassesThat().resideInAPackage(root + "." + WEB + "..")
                .because("the web layer depends on the domains, never the reverse (ADR-0001, ADR-0017)")
                .allowEmptyShould(true);
    }

    public static ArchRule domainsFollowDependencyMatrix(String root) {
        List<ArchRule> rules = new ArrayList<>();
        ALLOWED_DOMAIN_DEPENDENCIES.forEach((domain, allowed) -> {
            List<String> packages = new ArrayList<>();
            packages.add(root + "." + domain + "..");
            packages.add(root + "." + COMMON + "..");
            allowed.forEach(a -> packages.add(root + "." + a + ".."));
            rules.add(classes().that().resideInAPackage(root + "." + domain + "..")
                    .should().onlyDependOnClassesThat(resideOutsideOfPackage(root + "..")
                            .or(resideInAnyPackage(packages.toArray(String[]::new))))
                    .because(domain + " may only use common and " + allowed + " (ADR-0001)")
                    .allowEmptyShould(true));
        });
        return CompositeArchRule.of(rules).as("domain packages follow the ADR-0001 dependency matrix");
    }

    public static ArchRule internalPackagesArePrivate(String root) {
        List<ArchRule> rules = new ArrayList<>();
        for (String domain : ALLOWED_DOMAIN_DEPENDENCIES.keySet()) {
            rules.add(noClasses().that().resideOutsideOfPackage(root + "." + domain + "..")
                    .should().dependOnClassesThat().resideInAPackage(root + "." + domain + ".internal..")
                    .because(domain + ".internal is private to the " + domain + " domain (ADR-0001)")
                    .allowEmptyShould(true));
        }
        return CompositeArchRule.of(rules).as("<domain>.internal packages are only used by their own domain");
    }

    public static ArchRule packagesAreFreeOfCycles(String root) {
        return slices().matching(root + ".(*)..").should().beFreeOfCycles().allowEmptyShould(true);
    }

    public static ArchRule springBatchOnlyInBatch(String root) {
        return noClasses().that().resideInAPackage(root + "..")
                .and().resideOutsideOfPackage(root + ".batch..")
                .should().dependOnClassesThat().resideInAPackage("org.springframework.batch..")
                .because("jobs, steps and readers belong to the batch domain (ADR-0001)")
                .allowEmptyShould(true);
    }

    public static ArchRule noBinaryFloatingPoint(String root) {
        return noFields().that().areDeclaredInClassesThat().resideInAPackage(root + "..")
                .should().haveRawType(double.class)
                .orShould().haveRawType(float.class)
                .orShould().haveRawType(Double.class)
                .orShould().haveRawType(Float.class)
                .because("COBOL numerics map to BigDecimal/long, never binary floating point (ADR-0004)")
                .allowEmptyShould(true);
    }

    public static List<ArchRule> all(String root) {
        return List.of(
                commonDependsOnNoDomain(root),
                noDomainDependsOnWeb(root),
                domainsFollowDependencyMatrix(root),
                internalPackagesArePrivate(root),
                packagesAreFreeOfCycles(root),
                springBatchOnlyInBatch(root),
                noBinaryFloatingPoint(root));
    }
}
