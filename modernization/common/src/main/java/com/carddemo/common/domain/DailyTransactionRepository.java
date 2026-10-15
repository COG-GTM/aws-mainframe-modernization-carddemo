package com.carddemo.common.domain;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DailyTransactionRepository extends JpaRepository<DailyTransaction, String> {

    List<DailyTransaction> findByStatusOrderByTranIdAsc(String status);
}
