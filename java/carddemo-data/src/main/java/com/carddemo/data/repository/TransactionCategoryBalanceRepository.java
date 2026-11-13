package com.carddemo.data.repository;

import com.carddemo.data.entity.TransactionCategoryBalance;
import com.carddemo.data.entity.TransactionCategoryBalanceId;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/** Replaces VSAM KSDS TCATBALF. */
public interface TransactionCategoryBalanceRepository extends JpaRepository<TransactionCategoryBalance, TransactionCategoryBalanceId> {

    /** Generic key browse on TRANCAT-ACCT-ID (interest calculation, CBACT04C). */
    List<TransactionCategoryBalance> findByIdAccountIdOrderById(Long accountId);
}
