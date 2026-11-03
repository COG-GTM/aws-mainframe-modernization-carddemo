package com.carddemo.transaction.service;

import com.carddemo.common.error.NotFoundException;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.repository.TransactionRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COTRN00C (list) and COTRN01C (view). */
@Service
public class TransactionQueryService {

    private final TransactionRepository transactions;

    public TransactionQueryService(TransactionRepository transactions) {
        this.transactions = transactions;
    }

    /**
     * COTRN00C: STARTBR on TRAN-ID at the typed id (LOW-VALUES when blank), READNEXT in key order.
     * {@code startTransactionId} is zero padded to the 16 digits of TRAN-ID; {@code cardNumber} is an
     * extra filter the COBOL screen does not have.
     */
    @Transactional(readOnly = true)
    public Page<Transaction> list(String startTransactionId, String cardNumber, Pageable pageable) {
        String startKey = startTransactionId == null || startTransactionId.isBlank()
                ? ""
                : TransactionIdGenerator.format(Long.parseLong(startTransactionId.trim()));
        return cardNumber == null || cardNumber.isBlank()
                ? transactions.findByTranIdGreaterThanEqual(startKey, pageable)
                : transactions.findByCardNumAndTranIdGreaterThanEqual(cardNumber, startKey, pageable);
    }

    /** COTRN01C READ-TRANSACT-FILE. The COBOL's READ ... UPDATE is never followed by a REWRITE, so it is a plain read. */
    @Transactional(readOnly = true)
    public Transaction get(String transactionId) {
        return transactions.findById(transactionId)
                .orElseThrow(() -> new NotFoundException("Transaction ID NOT found..."));
    }
}
