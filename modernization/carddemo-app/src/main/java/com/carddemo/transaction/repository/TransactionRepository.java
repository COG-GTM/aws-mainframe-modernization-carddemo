package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;

/** Table {@code transaction.transactions} (TRANSACT). */
public interface TransactionRepository extends JpaRepository<Transaction, String> {
}
