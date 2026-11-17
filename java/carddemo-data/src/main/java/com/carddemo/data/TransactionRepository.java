package com.carddemo.data;

import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionRepository extends JpaRepository<Transaction, String> {

    /** TRANSACT alternate index path on TRAN-PROC-TS. */
    List<Transaction> findByProcTsBetweenOrderByProcTs(LocalDateTime from, LocalDateTime to);
}
