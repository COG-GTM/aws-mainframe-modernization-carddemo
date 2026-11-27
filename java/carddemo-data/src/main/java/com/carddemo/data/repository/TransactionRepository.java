package com.carddemo.data.repository;

import com.carddemo.data.entity.Transaction;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/** Replaces VSAM KSDS TRANSACT and its AIX on TRAN-PROC-TS. */
public interface TransactionRepository extends JpaRepository<Transaction, String> {

    /** STARTBR/READNEXT browse used by the transaction list screen (COTRN00C). */
    Page<Transaction> findByIdGreaterThanEqualOrderById(String id, Pageable pageable);

    /** READPREV from HIGH-VALUES: highest transaction id, used to allocate the next id (COTRN02C, COBIL00C). */
    Optional<Transaction> findTopByOrderByIdDesc();

    /** AIX range on TRAN-PROC-TS (transaction report TRANREPT). */
    List<Transaction> findByProcessedTimestampBetweenOrderByProcessedTimestamp(LocalDateTime from, LocalDateTime to);

    List<Transaction> findByCardNumberOrderById(String cardNumber);
}
