package com.carddemo.card.repository;

import com.carddemo.card.domain.CardXref;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CardXrefRepository extends JpaRepository<CardXref, String> {

    /** XREFAIX alternate index on XREF-ACCT-ID, read by CBACT04C paragraph 1110-GET-XREF-DATA. */
    List<CardXref> findByAcctId(Long acctId);

    List<CardXref> findByCustId(Long custId);

    Optional<CardXref> findFirstByAcctIdOrderByCardNum(Long acctId);
}
