package com.carddemo.transaction.online;

import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionRepository;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/** COTRN01C (CT01) {@code PROCESS-ENTER-KEY} / {@code READ-TRANSACT-FILE}: one transaction by id. */
@Component
public class TransactionLookup {

    public static final String TRAN_ID_FIELD = "tranId";
    public static final String MSG_EMPTY = "Tran ID can NOT be empty...";
    public static final String MSG_NOT_FOUND = "Transaction ID NOT found...";
    public static final String MSG_LOOKUP_FAILED = "Unable to lookup Transaction...";

    private final TransactionRepository transactions;

    public TransactionLookup(TransactionRepository transactions) {
        this.transactions = transactions;
    }

    /** R-9 blank → 400; R-10 the id as typed (no numeric check); R-13 NOTFND → 404; R-14 other → abend. */
    @Transactional(readOnly = true)
    public Transaction byId(String tranId) {
        if (ScreenInput.isSpacesOrLowValues(tranId)) {
            throw new InvalidRequestException(TRAN_ID_FIELD, MSG_EMPTY);
        }
        String key = ScreenInput.rightTrim(tranId);
        try {
            return transactions.findById(key).orElseThrow(() -> new RecordNotFoundException(MSG_NOT_FOUND));
        } catch (DataAccessException e) {
            throw AbendException.carddemo(MSG_LOOKUP_FAILED, e);
        }
    }
}
