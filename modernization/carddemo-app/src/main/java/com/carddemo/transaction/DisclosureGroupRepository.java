package com.carddemo.transaction;

import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

/**
 * DISCGRP access paths: CBACT04C {@code 1200-GET-INTEREST-RATE} reads by DIS-GROUP-KEY and, on status 23
 * (not found), re-reads with DIS-ACCT-GROUP-ID = {@code 'DEFAULT'} ({@code 1200-A-GET-DEFAULT-INT-RATE}).
 */
public interface DisclosureGroupRepository extends JpaRepository<DisclosureGroup, DisclosureGroupId> {

    String DEFAULT_GROUP = "DEFAULT";

    @Query("select d from DisclosureGroup d order by d.id.acctGroupId, d.id.tranTypeCd, d.id.tranCatCd")
    List<DisclosureGroup> findAllInKeyOrder();

    default Optional<DisclosureGroup> findWithDefault(String acctGroupId, String tranTypeCd, int tranCatCd) {
        return findById(new DisclosureGroupId(acctGroupId, tranTypeCd, tranCatCd))
                .or(() -> findById(new DisclosureGroupId(DEFAULT_GROUP, tranTypeCd, tranCatCd)));
    }
}
