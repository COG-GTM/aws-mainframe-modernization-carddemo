package com.carddemo.transaction;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

/** DALYTRAN access paths: sequential read in file order (CBTRN01C, CBTRN02C); lookup by DALYTRAN-ID for rejects. */
public interface DailyTransactionRepository extends JpaRepository<DailyTransaction, Integer> {

    List<DailyTransaction> findAllByOrderByRecordSeqAsc();

    List<DailyTransaction> findByTranIdOrderByRecordSeqAsc(String tranId);
}
