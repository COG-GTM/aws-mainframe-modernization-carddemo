package com.carddemo.interest.rules;

import com.carddemo.interest.domain.DisclosureGroup;
import com.carddemo.interest.domain.DisclosureGroupKey;
import com.carddemo.interest.domain.TransactionCategory;
import com.carddemo.interest.exception.DisclosureGroupNotFoundException;
import com.carddemo.interest.repository.DisclosureGroupRepository;

import java.math.BigDecimal;
import java.util.Optional;

/**
 * Reads rates from the disclosure-group table, falling back to the {@code DEFAULT} pricing group.
 */
public final class DisclosureGroupRateResolver implements RateResolver {

    private final DisclosureGroupRepository disclosureGroups;

    public DisclosureGroupRateResolver(DisclosureGroupRepository disclosureGroups) {
        this.disclosureGroups = disclosureGroups;
    }

    /**
     * Business rule BR-3 — interest-rate selection with {@code DEFAULT} fallback.
     *
     * <p>COBOL paragraphs {@code 1200-GET-INTEREST-RATE}
     * ({@code app/cbl/CBACT04C.cbl:415-440}) and {@code 1200-A-GET-DEFAULT-INT-RATE}
     * ({@code app/cbl/CBACT04C.cbl:443-460}):
     * the disclosure file is read on {@code account group id + transaction type + transaction
     * category}; a "record not found" (file status {@code '23'}) is tolerated, the group id is
     * replaced by the literal {@code 'DEFAULT'} ({@code app/cbl/CBACT04C.cbl:437}) and the read is
     * retried. If the {@code DEFAULT} row is missing too, the COBOL job abends — here the
     * equivalent {@link DisclosureGroupNotFoundException} is thrown.
     */
    @Override
    public BigDecimal annualRatePercent(String accountGroupId, TransactionCategory category) {
        DisclosureGroupKey requested = DisclosureGroupKey.of(accountGroupId, category);
        Optional<DisclosureGroup> disclosed = disclosureGroups.find(requested);
        if (disclosed.isEmpty()) {
            disclosed = disclosureGroups.find(requested.withDefaultGroup());
        }
        return disclosed.orElseThrow(() -> new DisclosureGroupNotFoundException(requested))
                .annualRatePercent();
    }
}
