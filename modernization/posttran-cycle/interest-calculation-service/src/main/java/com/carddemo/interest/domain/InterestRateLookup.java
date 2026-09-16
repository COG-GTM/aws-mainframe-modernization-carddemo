package com.carddemo.interest.domain;

import com.carddemo.recordio.layout.DisclosureGroup;
import com.carddemo.recordio.store.KeyedRecordStore;
import com.carddemo.recordio.store.RecordNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;

/**
 * CBACT04C 1200-GET-INTEREST-RATE / 1200-A-GET-DEFAULT-INT-RATE (lines 415-461): read DISCGRP by
 * (account group id, type, category); on status 23 retry with group id {@code 'DEFAULT'}; if the
 * DEFAULT row is missing too the program abends.
 */
public final class InterestRateLookup {

    public static final String DEFAULT_GROUP = "DEFAULT";
    private static final Logger LOG = LoggerFactory.getLogger(InterestRateLookup.class);

    private final KeyedRecordStore<DisclosureGroup> disclosureGroups;

    public InterestRateLookup(KeyedRecordStore<DisclosureGroup> disclosureGroups) {
        this.disclosureGroups = disclosureGroups;
    }

    public BigDecimal rateFor(String accountGroupId, String typeCode, int categoryCode) {
        String key = DisclosureGroup.key(accountGroupId, typeCode, categoryCode);
        return disclosureGroups.read(key).map(DisclosureGroup::interestRate).orElseGet(() -> {
            LOG.info("DISCLOSURE GROUP RECORD MISSING");
            LOG.info("TRY WITH DEFAULT GROUP CODE");
            String defaultKey = DisclosureGroup.key(DEFAULT_GROUP, typeCode, categoryCode);
            return disclosureGroups.read(defaultKey).map(DisclosureGroup::interestRate)
                    .orElseThrow(() -> new RecordNotFoundException("DISCGRP", defaultKey,
                            "ERROR READING DEFAULT DISCLOSURE GROUP"));
        });
    }
}
