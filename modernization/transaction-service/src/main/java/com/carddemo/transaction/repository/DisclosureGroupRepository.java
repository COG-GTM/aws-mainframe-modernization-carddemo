package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.DisclosureGroup;
import com.carddemo.transaction.domain.DisclosureGroupId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DisclosureGroupRepository extends JpaRepository<DisclosureGroup, DisclosureGroupId> {
}
