package com.carddemo.data.repository;

import com.carddemo.data.entity.TransactionType;

import org.springframework.data.jpa.repository.JpaRepository;

/** Replaces VSAM KSDS TRANTYPE (DB2 TRANSACTION_TYPE in the optional module). */
public interface TransactionTypeRepository extends JpaRepository<TransactionType, String> {
}
