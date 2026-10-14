package com.carddemo.batch.posttran.domain;

import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionRecordRepository extends JpaRepository<TransactionRecord, String> {
}
