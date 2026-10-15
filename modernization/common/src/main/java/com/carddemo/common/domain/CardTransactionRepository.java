package com.carddemo.common.domain;

import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CardTransactionRepository extends JpaRepository<CardTransaction, String> {

    List<CardTransaction> findByTranIdGreaterThanEqualOrderByTranIdAsc(String startKey, Pageable page);

    List<CardTransaction> findByTranIdLessThanOrderByTranIdDesc(String startKey, Pageable page);

    List<CardTransaction> findByProcTsGreaterThanEqualAndProcTsLessThanOrderByCardNumAscTranIdAsc(
            LocalDateTime fromInclusive, LocalDateTime toExclusive);

    List<CardTransaction> findByCardNumOrderByTranIdAsc(String cardNum);
}
