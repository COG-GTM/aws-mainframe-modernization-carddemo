package com.carddemo.data;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CardRepository extends JpaRepository<Card, String> {

    /** CARDDATA alternate index path on CARD-ACCT-ID. */
    List<Card> findByAcctIdOrderByCardNum(Long acctId);
}
