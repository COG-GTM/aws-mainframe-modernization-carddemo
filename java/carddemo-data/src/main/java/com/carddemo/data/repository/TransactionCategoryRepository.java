package com.carddemo.data.repository;

import com.carddemo.data.entity.TransactionCategory;
import com.carddemo.data.entity.TransactionCategoryId;

import org.springframework.data.jpa.repository.JpaRepository;

/** Replaces VSAM KSDS TRANCATG (DB2 TRANSACTION_TYPE_CATEGORY in the optional module). */
public interface TransactionCategoryRepository extends JpaRepository<TransactionCategory, TransactionCategoryId> {
}
