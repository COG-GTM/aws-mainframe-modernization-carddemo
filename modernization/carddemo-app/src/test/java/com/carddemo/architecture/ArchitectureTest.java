package com.carddemo.architecture;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;

/** Applies the ADR-0001 module rules to the production classes of {@code carddemo-app}. */
@AnalyzeClasses(packages = ArchitectureTest.ROOT, importOptions = ImportOption.DoNotIncludeTests.class)
class ArchitectureTest {

    static final String ROOT = "com.carddemo";

    @ArchTest
    static final ArchRule commonDependsOnNoDomain = ModularMonolithRules.commonDependsOnNoDomain(ROOT);

    @ArchTest
    static final ArchRule domainsFollowDependencyMatrix = ModularMonolithRules.domainsFollowDependencyMatrix(ROOT);

    @ArchTest
    static final ArchRule internalPackagesArePrivate = ModularMonolithRules.internalPackagesArePrivate(ROOT);

    @ArchTest
    static final ArchRule packagesAreFreeOfCycles = ModularMonolithRules.packagesAreFreeOfCycles(ROOT);

    @ArchTest
    static final ArchRule springBatchOnlyInBatch = ModularMonolithRules.springBatchOnlyInBatch(ROOT);

    @ArchTest
    static final ArchRule noBinaryFloatingPoint = ModularMonolithRules.noBinaryFloatingPoint(ROOT);
}
