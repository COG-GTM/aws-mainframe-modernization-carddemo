package com.carddemo.data.repository;

import com.carddemo.data.entity.Card;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

/** CARDDAT keyed by CARD-NUM, plus the CARDAIX path on CARD-ACCT-ID. */
public interface CardRepository extends JpaRepository<Card, String> {

    /** CARDAIX (NONUNIQUEKEY): every card of an account, in prime-key order. */
    List<Card> findByAcctIdOrderByCardNum(Long acctId);

    /** STARTBR GTEQ / READNEXT browse over CARDDAT from a card number. */
    List<Card> findByCardNumGreaterThanEqualOrderByCardNum(String cardNum, Pageable page);
}
