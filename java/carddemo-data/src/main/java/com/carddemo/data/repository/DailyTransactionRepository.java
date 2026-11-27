package com.carddemo.data.repository;

import com.carddemo.data.entity.DailyTransaction;

import org.springframework.data.jpa.repository.JpaRepository;

/** Staging of the sequential DALYTRAN input of POSTTRAN (CBTRN02C). */
public interface DailyTransactionRepository extends JpaRepository<DailyTransaction, String> {
}
