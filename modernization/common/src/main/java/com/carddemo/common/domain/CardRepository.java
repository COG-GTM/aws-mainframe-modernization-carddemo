package com.carddemo.common.domain;

import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface CardRepository extends JpaRepository<Card, String> {

    List<Card> findByAcctIdOrderByCardNumAsc(Long acctId);

    @Query("select c from Card c where c.cardNum >= :start and (:acctId is null or c.acctId = :acctId)"
            + " and (:cardNum is null or c.cardNum = :cardNum) order by c.cardNum asc")
    List<Card> browseForward(@Param("start") String start, @Param("acctId") Long acctId,
                             @Param("cardNum") String cardNum, Pageable page);

    @Query("select c from Card c where c.cardNum < :start and (:acctId is null or c.acctId = :acctId)"
            + " and (:cardNum is null or c.cardNum = :cardNum) order by c.cardNum desc")
    List<Card> browseBackward(@Param("start") String start, @Param("acctId") Long acctId,
                              @Param("cardNum") String cardNum, Pageable page);
}
