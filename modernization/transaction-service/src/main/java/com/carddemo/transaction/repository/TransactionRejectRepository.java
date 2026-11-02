package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.TransactionReject;
import java.time.Instant;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionRejectRepository extends JpaRepository<TransactionReject, Long> {

    /** The rejects a posting run wrote (DALYREJS of that run). */
    List<TransactionReject> findByRejectedAtGreaterThanEqualOrderByRejectIdAsc(Instant since);
}
