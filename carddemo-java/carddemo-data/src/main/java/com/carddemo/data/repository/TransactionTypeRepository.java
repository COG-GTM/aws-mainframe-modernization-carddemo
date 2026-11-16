package com.carddemo.data.repository;

import com.carddemo.data.entity.TransactionType;
import org.springframework.data.jpa.repository.JpaRepository;

/** TRANTYPE: keyed READ by TRAN-TYPE. */
public interface TransactionTypeRepository extends JpaRepository<TransactionType, String> {
}
