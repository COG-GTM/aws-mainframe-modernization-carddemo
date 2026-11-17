package com.carddemo.data.repository;

import com.carddemo.data.entity.DailyTransaction;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

/** DALYTRAN staging: read back in input-file order, as the sequential READ loop of CBTRN02C does. */
public interface DailyTransactionRepository extends JpaRepository<DailyTransaction, Long> {

    List<DailyTransaction> findAllByOrderBySeqNo();
}
