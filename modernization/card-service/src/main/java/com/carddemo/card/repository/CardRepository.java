package com.carddemo.card.repository;

import com.carddemo.card.domain.Card;
import jakarta.persistence.LockModeType;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Lock;

public interface CardRepository extends JpaRepository<Card, String>, JpaSpecificationExecutor<Card> {

    /** CARDAIX alternate index: all cards of an account. */
    Page<Card> findByAcctId(Long acctId, Pageable pageable);

    List<Card> findByAcctIdIn(List<Long> acctIds);

    /** COCRDUPC 9200-WRITE-PROCESSING: {@code READ ... UPDATE} holds the record until the rewrite. */
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Optional<Card> findByCardNum(String cardNum);

    /** COCRDLIC 9000-READ-FORWARD: STARTBR GTEQ on CARD-NUM + READNEXT, in key order. */
    List<Card> findByCardNumGreaterThanEqualOrderByCardNumAsc(String startKey, Pageable pageable);

    /** COCRDLIC 9100-READ-BACKWARDS: READPREV, cards before the key, nearest first. */
    List<Card> findByCardNumLessThanOrderByCardNumDesc(String key, Pageable pageable);
}
