package com.carddemo.card.repository;

import com.carddemo.card.domain.Card;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CardRepository extends JpaRepository<Card, String> {

    /** CARDAIX alternate index: all cards of an account (COCRDLIC filtered by account). */
    Page<Card> findByAcctId(Long acctId, Pageable pageable);

    List<Card> findByAcctIdIn(List<Long> acctIds);

    Page<Card> findByCardNumIn(List<String> cardNums, Pageable pageable);

    Page<Card> findByCardNumInAndAcctId(List<String> cardNums, Long acctId, Pageable pageable);

    /** COCRDLIC 9000-READ-FORWARD: STARTBR GTEQ on CARD-NUM + READNEXT, in key order. */
    List<Card> findByCardNumGreaterThanEqualOrderByCardNumAsc(String startKey, Pageable pageable);

    /** COCRDLIC 9100-READ-BACKWARDS: READPREV, cards before the key, nearest first. */
    List<Card> findByCardNumLessThanOrderByCardNumDesc(String key, Pageable pageable);
}
