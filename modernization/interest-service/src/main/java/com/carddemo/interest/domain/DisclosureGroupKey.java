package com.carddemo.interest.domain;

/**
 * Key of a disclosure-group record: the account's pricing group plus the transaction category.
 *
 * <p>{@code DIS-GROUP-KEY} ({@code app/cpy/CVTRA02Y.cpy:5-8}). The group id is a fixed 10-character
 * COBOL field, so lookups normalise to the trimmed form and the fallback group is the literal
 * {@code DEFAULT} moved at {@code app/cbl/CBACT04C.cbl:437}.
 */
public record DisclosureGroupKey(String accountGroupId, TransactionCategory category) {

    /** Group id used when an account's own group has no disclosure record. */
    public static final String DEFAULT_GROUP_ID = "DEFAULT";

    public DisclosureGroupKey {
        accountGroupId = accountGroupId == null ? "" : accountGroupId.trim();
    }

    public static DisclosureGroupKey of(String accountGroupId, TransactionCategory category) {
        return new DisclosureGroupKey(accountGroupId, category);
    }

    public DisclosureGroupKey withDefaultGroup() {
        return new DisclosureGroupKey(DEFAULT_GROUP_ID, category);
    }
}
