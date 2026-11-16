package com.carddemo.data;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CardXrefRepository extends JpaRepository<CardXref, String> {

    /** CARDXREF alternate index path on XREF-ACCT-ID. */
    List<CardXref> findByAcctIdOrderByCardNum(Long acctId);
}
