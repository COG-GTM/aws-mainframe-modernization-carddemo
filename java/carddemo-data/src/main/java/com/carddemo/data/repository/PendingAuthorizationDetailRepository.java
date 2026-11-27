package com.carddemo.data.repository;

import com.carddemo.data.entity.PendingAuthorizationDetail;
import com.carddemo.data.entity.PendingAuthorizationDetailId;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/** Replaces IMS child segment PAUTDTL1 access (GNP under PAUTSUM0). */
public interface PendingAuthorizationDetailRepository extends JpaRepository<PendingAuthorizationDetail, PendingAuthorizationDetailId> {

    /** GNP under a PAUTSUM0 parent: details in key order (most recent first, keys are inverted date/time). */
    List<PendingAuthorizationDetail> findByIdAccountIdOrderById(Long accountId);
}
