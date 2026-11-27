package com.carddemo.data.repository;

import com.carddemo.data.entity.PendingAuthorizationSummary;

import org.springframework.data.jpa.repository.JpaRepository;

/** Replaces IMS root segment PAUTSUM0 access (GU/GN on DBPAUTP0). */
public interface PendingAuthorizationSummaryRepository extends JpaRepository<PendingAuthorizationSummary, Long> {
}
