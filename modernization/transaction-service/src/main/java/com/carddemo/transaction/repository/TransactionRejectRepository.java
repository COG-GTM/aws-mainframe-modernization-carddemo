package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.TransactionReject;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionRejectRepository extends JpaRepository<TransactionReject, Long> {
}
