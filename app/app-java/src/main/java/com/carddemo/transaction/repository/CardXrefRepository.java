package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.CardXref;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface CardXrefRepository extends JpaRepository<CardXref, String> {

    /** Keyed READ through the CXACAIX alternate index (XREF-ACCT-ID). */
    Optional<CardXref> findFirstByAcctIdOrderByCardNumAsc(Long acctId);
}
