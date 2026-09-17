package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.DailyTransaction;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DailyTransactionRepository extends JpaRepository<DailyTransaction, String> {

    List<DailyTransaction> findByProcessedFalseOrderByTranId();
}
