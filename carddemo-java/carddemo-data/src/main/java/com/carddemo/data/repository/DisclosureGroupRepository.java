package com.carddemo.data.repository;

import com.carddemo.data.entity.DisclosureGroup;
import com.carddemo.data.entity.DisclosureGroupId;
import org.springframework.data.jpa.repository.JpaRepository;

/** DISCGRP: keyed READ by DIS-GROUP-KEY (account group, type, category). */
public interface DisclosureGroupRepository extends JpaRepository<DisclosureGroup, DisclosureGroupId> {
}
