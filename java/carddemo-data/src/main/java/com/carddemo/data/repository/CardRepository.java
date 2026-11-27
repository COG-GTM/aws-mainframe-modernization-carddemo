package com.carddemo.data.repository;

import com.carddemo.data.entity.Card;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/** Replaces CICS file CARDDAT (key CARD-NUM) and its alternate index path CARDAIX (CARD-ACCT-ID). */
public interface CardRepository extends JpaRepository<Card, String> {

    /** CARDAIX path: cards of an account. */
    List<Card> findByAccountIdOrderByCardNumber(Long accountId);

    /** STARTBR/READNEXT browse used by the card list screen (COCRDLIC). */
    Page<Card> findByCardNumberGreaterThanEqualOrderByCardNumber(String cardNumber, Pageable pageable);
}
