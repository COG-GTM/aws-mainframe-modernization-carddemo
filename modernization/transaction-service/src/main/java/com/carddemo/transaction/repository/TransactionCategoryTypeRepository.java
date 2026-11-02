package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.TransactionCategoryId;
import com.carddemo.transaction.domain.TransactionCategoryType;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionCategoryTypeRepository
        extends JpaRepository<TransactionCategoryType, TransactionCategoryId> {
}
