package com.carddemo.common.domain;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DailyTransactionRejectRepository extends JpaRepository<DailyTransactionReject, Long> {

    List<DailyTransactionReject> findByBatchIdOrderByIdAsc(String batchId);
}
