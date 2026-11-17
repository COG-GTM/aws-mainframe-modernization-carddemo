package com.carddemo.data.repository;

import com.carddemo.data.entity.Transaction;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

/** TRANSACT keyed by TRAN-ID, plus the alternate index on TRAN-PROC-TS. */
public interface TransactionRepository extends JpaRepository<Transaction, String> {

    /** STARTBR at HIGH-VALUES + READPREV: the highest TRAN-ID, from which the next id is derived. */
    Optional<Transaction> findTopByOrderByTranIdDesc();

    /** STARTBR GTEQ / READNEXT browse over TRANSACT from a transaction id. */
    List<Transaction> findByTranIdGreaterThanEqualOrderByTranId(String tranId, Pageable page);

    /** TRANSACT AIX (NONUNIQUEKEY) on TRAN-PROC-TS: transactions processed in [from, to). */
    List<Transaction> findByProcTsGreaterThanEqualAndProcTsLessThanOrderByProcTsAscTranIdAsc(
            LocalDateTime from, LocalDateTime to);
}
