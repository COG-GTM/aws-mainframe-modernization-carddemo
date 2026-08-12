package com.carddemo.interest.exception;

import com.carddemo.interest.domain.DisclosureGroupKey;

/**
 * Neither the account's own disclosure group nor the {@code DEFAULT} group prices a category.
 *
 * <p>COBOL equivalent: {@code 1200-A-GET-DEFAULT-INT-RATE}
 * ({@code app/cbl/CBACT04C.cbl:443-459}) abends when the fallback read does not return
 * status {@code '00'}. The missing-rate condition is therefore fatal for the whole cycle, not a
 * per-record skip.
 */
public class DisclosureGroupNotFoundException extends InterestBatchException {

    private static final long serialVersionUID = 1L;

    public DisclosureGroupNotFoundException(DisclosureGroupKey requested) {
        super("No DISCGRP record for group '" + requested.accountGroupId() + "' category "
                + requested.category() + " and no DEFAULT fallback");
    }
}
