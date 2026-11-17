package com.carddemo.data.repository;

import com.carddemo.data.entity.CardXref;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

/** CCXREF keyed by XREF-CARD-NUM, plus the CXACAIX path on XREF-ACCT-ID. */
public interface CardXrefRepository extends JpaRepository<CardXref, String> {

    /** CXACAIX (NONUNIQUEKEY): every cross-reference of an account, in prime-key order. */
    List<CardXref> findByAcctIdOrderByCardNum(Long acctId);
}
