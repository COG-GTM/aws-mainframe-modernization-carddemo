package com.carddemo.data.repository;

import com.carddemo.data.entity.DisclosureGroup;
import com.carddemo.data.entity.DisclosureGroupId;

import org.springframework.data.jpa.repository.JpaRepository;

/** Replaces VSAM KSDS DISCGRP. */
public interface DisclosureGroupRepository extends JpaRepository<DisclosureGroup, DisclosureGroupId> {
}
