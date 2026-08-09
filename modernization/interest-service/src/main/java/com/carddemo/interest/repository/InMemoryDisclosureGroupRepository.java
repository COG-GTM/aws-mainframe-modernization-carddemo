package com.carddemo.interest.repository;

import com.carddemo.interest.domain.DisclosureGroup;
import com.carddemo.interest.domain.DisclosureGroupKey;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

/** Keyed in-memory view of the {@code DISCGRP} rate table. */
public final class InMemoryDisclosureGroupRepository implements DisclosureGroupRepository {

    private final Map<DisclosureGroupKey, DisclosureGroup> groupsByKey = new LinkedHashMap<>();

    public InMemoryDisclosureGroupRepository(Collection<DisclosureGroup> groups) {
        groups.forEach(group -> groupsByKey.putIfAbsent(group.key(), group));
    }

    @Override
    public Optional<DisclosureGroup> find(DisclosureGroupKey key) {
        return Optional.ofNullable(groupsByKey.get(key));
    }
}
