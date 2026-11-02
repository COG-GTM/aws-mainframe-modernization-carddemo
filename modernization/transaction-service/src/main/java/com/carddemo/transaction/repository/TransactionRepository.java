package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.Transaction;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionRepository extends JpaRepository<Transaction, String> {

    /** COTRN00C browses TRANSACT; the card filter replaces the 3270 browse start key. */
    Page<Transaction> findByCardNum(String cardNum, Pageable pageable);

    /** COTRN00C STARTBR on TRAN-ID at the typed start key, READNEXT forward; paged. */
    Page<Transaction> findByTranIdGreaterThanEqual(String startKey, Pageable pageable);

    /** As {@link #findByTranIdGreaterThanEqual}, restricted to one card. */
    Page<Transaction> findByCardNumAndTranIdGreaterThanEqual(String cardNum, String startKey, Pageable pageable);

    /** COTRN00C STARTBR on TRAN-ID + READNEXT: records from the start key forward, in key order. */
    List<Transaction> findByTranIdGreaterThanEqualOrderByTranIdAsc(String startKey, Pageable pageable);

    /** COTRN00C READPREV: records before the key, nearest first. */
    List<Transaction> findByTranIdLessThanOrderByTranIdDesc(String key, Pageable pageable);

    /** COBIL00C / COTRN02C: STARTBR with HIGH-VALUES + READPREV reads the highest TRAN-ID. */
    Optional<Transaction> findTopByOrderByTranIdDesc();

    /**
     * TRANSACT AIX on TRAN-PROC-TS (ix_transactions_proc_ts). TRANREPT selects records whose
     * processing date is in the report range and sorts them by TRAN-CARD-NUM.
     */
    List<Transaction> findByProcTsGreaterThanEqualAndProcTsLessThanOrderByCardNumAscTranIdAsc(
            LocalDateTime from, LocalDateTime toExclusive);

    /** The SYSTRAN records of an INTCALC run: CBACT04C TRAN-IDs start with the run's PARM-DATE. */
    List<Transaction> findBySourceAndTranIdStartingWithOrderByTranIdAsc(String source, String tranIdPrefix);

    boolean existsByTranIdStartingWith(String tranIdPrefix);
}
