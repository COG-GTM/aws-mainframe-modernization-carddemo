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

    /** PostgreSQL advisory lock key that serialises online transaction-id assignment ("TRANSACT" in ASCII). */
    long TRAN_ID_LOCK = 0x5452414E53414354L;

    List<Transaction> findAllByOrderByTranIdAsc();

    /**
     * Transaction-scoped advisory lock on {@code key}, held until commit/rollback: COTRN02C and COBIL00C take it
     * before reading the highest id ({@code READPREV} from HIGH-VALUES) so two concurrent writers never compute the
     * same next id. A {@code SELECT ... FOR UPDATE} on the max row would not do: the second writer would re-read the
     * old max row after the wait and still miss the row the first writer inserted.
     */
    @Query(value = "select 1 from pg_advisory_xact_lock(:key)", nativeQuery = true)
    Integer lockIdAssignment(@Param("key") long key);

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

    /**
     * TRANREPT {@code STEP10} extract ({@code INCLUDE COND=(TRAN-PROC-DT,GE,start,AND,TRAN-PROC-DT,LE,end)} with
     * TRAN-PROC-DT = TRAN-PROC-TS(1:10)), in byte order, sorted by card number then TRAN-ID (the KSDS input order).
     */
    @Query(value = "select * from transaction where substr(proc_ts, 1, 10) collate \"C\" between :start and :end"
            + " order by card_num collate \"C\", tran_id collate \"C\"", nativeQuery = true)
    List<Transaction> findByProcDateWindow(@Param("start") String start, @Param("end") String end);

    /**
     * CREASTMT STEP010 ({@code SORT FIELDS=(263,16,CH,A,1,16,CH,A)}): every transaction by card number, then
     * transaction id, in byte order.
     */
    @Query(value = "select * from transaction order by card_num collate \"C\", tran_id collate \"C\"",
            nativeQuery = true)
    List<Transaction> findAllForStatements();

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
