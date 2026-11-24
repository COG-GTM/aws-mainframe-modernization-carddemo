package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.TransactionCategoryBalance;
import com.carddemo.transaction.domain.TransactionCategoryBalanceId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionCategoryBalanceRepository
        extends JpaRepository<TransactionCategoryBalance, TransactionCategoryBalanceId> {
}
