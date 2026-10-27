package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface TransactionRepository extends JpaRepository<Transaction, String> {

    /** STARTBR GTEQ + READNEXT. */
    List<Transaction> findTop11ByTranIdGreaterThanEqualOrderByTranIdAsc(String tranId);

    /** STARTBR at last key + READNEXT past it (PF8). */
    List<Transaction> findTop11ByTranIdGreaterThanOrderByTranIdAsc(String tranId);

    /** STARTBR at first key + READPREV past it (PF7). */
    List<Transaction> findTop11ByTranIdLessThanOrderByTranIdDesc(String tranId);

    /** STARTBR HIGH-VALUES + READPREV. */
    Optional<Transaction> findTopByOrderByTranIdDesc();
}
