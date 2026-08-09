package com.carddemo.interest.repository;

import com.carddemo.interest.domain.DisclosureGroup;
import com.carddemo.interest.domain.DisclosureGroupKey;

import java.util.Optional;

/**
 * Access to the disclosure-group rate table ({@code DISCGRP} DD).
 *
 * <p>Replaces the keyed {@code READ DISCGRP-FILE} of {@code 1200-GET-INTEREST-RATE}
 * ({@code app/cbl/CBACT04C.cbl:415-420}); the {@code DEFAULT}-group retry lives in
 * {@link com.carddemo.interest.rules.DisclosureGroupRateResolver}, not here.
 */
public interface DisclosureGroupRepository {

    Optional<DisclosureGroup> find(DisclosureGroupKey key);
}
