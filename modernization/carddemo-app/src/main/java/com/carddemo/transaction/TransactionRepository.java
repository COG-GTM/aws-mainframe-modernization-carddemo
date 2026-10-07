package com.carddemo.transaction;

import com.carddemo.common.data.KeysetPage;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Limit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

/**
 * TRANSACT access paths: {@code READ} by TRAN-ID (COTRN01C), {@code WRITE} (COTRN02C, COBIL00C, CBTRN02C,
 * CBACT04C), the highest TRAN-ID ({@code STARTBR} at HIGH-VALUES + {@code READPREV} in COTRN02C/COBIL00C, used to
 * assign the next id), the COTRN00C browse (ten transactions per screen) and the AIX on TRAN-PROC-TS, ordered by
 * index {@code transaction_proc_ts_ix (proc_ts, tran_id)} (ADR-0011).
 */
public interface TransactionRepository extends JpaRepository<Transaction, String> {

    /** COTRN00C {@code WS-IDX >= 11}: transactions per screen. */
    int COTRN00C_SCREEN_ROWS = 10;

    List<Transaction> findAllByOrderByTranIdAsc();

    Optional<Transaction> findFirstByOrderByTranIdDesc();

    List<Transaction> findByTranIdGreaterThanEqualOrderByTranIdAsc(String startTranId, Limit limit);

    List<Transaction> findByTranIdGreaterThanOrderByTranIdAsc(String lastTranIdShown, Limit limit);

    List<Transaction> findByTranIdLessThanOrderByTranIdDesc(String firstTranIdShown, Limit limit);

    /** AIX {@code STARTBR GTEQ} on TRAN-PROC-TS. */
    List<Transaction> findByProcTsGreaterThanEqualOrderByProcTsAscTranIdAsc(String startProcTs, Limit limit);

    /** AIX {@code READNEXT} continuation after the row ({@code procTs}, {@code tranId}). */
    @Query("select t from Transaction t where t.procTs > :procTs or (t.procTs = :procTs and t.tranId > :tranId)"
            + " order by t.procTs, t.tranId")
    List<Transaction> findAfterProcTs(@Param("procTs") String procTs, @Param("tranId") String tranId, Limit limit);

    default KeysetPage<Transaction> browseFrom(String startTranId) {
        return KeysetPage.forward(l -> findByTranIdGreaterThanEqualOrderByTranIdAsc(startTranId, l),
                COTRN00C_SCREEN_ROWS);
    }

    default KeysetPage<Transaction> nextPage(String lastTranIdShown) {
        return KeysetPage.forward(l -> findByTranIdGreaterThanOrderByTranIdAsc(lastTranIdShown, l),
                COTRN00C_SCREEN_ROWS);
    }

    default KeysetPage<Transaction> previousPage(String firstTranIdShown) {
        return KeysetPage.backward(l -> findByTranIdLessThanOrderByTranIdDesc(firstTranIdShown, l),
                COTRN00C_SCREEN_ROWS);
    }
}
