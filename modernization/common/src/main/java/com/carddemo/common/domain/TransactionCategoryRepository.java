package com.carddemo.common.domain;

import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionCategoryRepository extends JpaRepository<TransactionCategory, TransactionCategoryId> {
}
