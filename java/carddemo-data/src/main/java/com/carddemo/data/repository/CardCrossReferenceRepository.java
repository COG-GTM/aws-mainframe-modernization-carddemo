package com.carddemo.data.repository;

import com.carddemo.data.entity.CardCrossReference;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/** Replaces CICS file CCXREF (key XREF-CARD-NUM) and alternate index path CXACAIX (XREF-ACCT-ID). */
public interface CardCrossReferenceRepository extends JpaRepository<CardCrossReference, String> {

    /** CXACAIX path: cross references of an account. */
    List<CardCrossReference> findByAccountId(Long accountId);
}
