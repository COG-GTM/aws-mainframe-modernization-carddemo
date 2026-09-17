package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.Transaction;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionRepository extends JpaRepository<Transaction, String> {

    /** COTRN00C browses TRANSACT; the card filter replaces the 3270 browse start key. */
    Page<Transaction> findByCardNum(String cardNum, Pageable pageable);
}
