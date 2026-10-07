package com.carddemo.transaction.online;

import com.carddemo.common.AbendException;
import com.carddemo.common.DuplicateRecordException;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import jakarta.persistence.EntityExistsException;
import java.sql.SQLException;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Transaction-id assignment and {@code WRITE} for COTRN02C (R-28, R-31) and COBIL00C (R-12, R-19, R-20).
 *
 * <p>COBOL reads the last record ({@code STARTBR} at HIGH-VALUES + {@code READPREV}) and adds one. Here the same read
 * runs after a transaction-scoped PostgreSQL advisory lock ({@link TransactionRepository#TRAN_ID_LOCK}), so
 * concurrent online writers are serialised from the read of the highest id until their commit and never compute the
 * same id. Ids keep the COBOL format: 16 digits, zero-padded.
 */
@Component
public class TransactionIds {

    public static final String MSG_LOOKUP_FAILED = "Unable to lookup Transaction...";
    public static final String MSG_DUPLICATE = "Tran ID already exist...";
    static final String UNIQUE_VIOLATION = "23505";
    private static final long ID_MODULUS = 10_000_000_000_000_000L;

    private final TransactionRepository transactions;

    public TransactionIds(TransactionRepository transactions) {
        this.transactions = transactions;
    }

    /**
     * The next id, holding the id lock until the caller's transaction ends; replaces {@code STARTBR-TRANSACT-FILE}
     * (HIGH-VALUES), {@code READPREV-TRANSACT-FILE} and {@code ENDBR-TRANSACT-FILE}.
     */
    @Transactional(propagation = Propagation.MANDATORY)
    public String next() {
        try {
            transactions.lockIdAssignment(TransactionRepository.TRAN_ID_LOCK);
            return next(transactions.findFirstByOrderByTranIdDesc().map(Transaction::getTranId).orElse(null));
        } catch (DataAccessException e) {
            throw AbendException.carddemo(MSG_LOOKUP_FAILED, e);
        }
    }

    /**
     * {@code WS-TRAN-ID-N = TRAN-ID + 1} into {@code PIC 9(16)}: an empty file starts at 1 (READPREV ENDFILE →
     * zeros), 9999999999999999 wraps to zeros as the 16-digit field truncates.
     */
    static String next(String lastTranId) {
        if (lastTranId == null) {
            return String.format("%016d", 1);
        }
        String id = lastTranId.strip();
        if (!TransactionBrowse.isDigits(id, 16)) {
            throw new AbendException(AbendException.CARDDEMO_ABEND_CODE, MSG_LOOKUP_FAILED + " last id " + id
                    + " is not numeric");
        }
        return String.format("%016d", (Long.parseLong(id) + 1) % ID_MODULUS);
    }

    /**
     * {@code WRITE-TRANSACT-FILE}: {@code WRITE FILE('TRANSACT')}: DUPKEY/DUPREC → 409 {@code Tran ID already
     * exist...}; other → abend.
     */
    @Transactional(propagation = Propagation.MANDATORY)
    public Transaction write(TransactionRecord record, String failure) {
        try {
            return transactions.saveAndFlush(Transaction.newRecord(record));
        } catch (DataIntegrityViolationException e) {
            if (isDuplicateKey(e)) {
                throw new DuplicateRecordException(MSG_DUPLICATE);
            }
            throw AbendException.carddemo(failure, e);
        } catch (DataAccessException e) {
            throw AbendException.carddemo(failure, e);
        }
    }

    static boolean isDuplicateKey(Throwable e) {
        for (Throwable t = e; t != null; t = t.getCause()) {
            if (t instanceof DuplicateKeyException || t instanceof EntityExistsException) {
                return true;
            }
            if (t instanceof SQLException sql && UNIQUE_VIOLATION.equals(sql.getSQLState())) {
                return true;
            }
            if (t.getCause() == t) {
                break;
            }
        }
        return false;
    }
}
