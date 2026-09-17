package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.TransactionType;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionTypeRepository extends JpaRepository<TransactionType, String> {
}
