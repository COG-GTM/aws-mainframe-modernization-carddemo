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

    @Transactional(readOnly = true)
    public Page<Transaction> list(String cardNumber, Pageable pageable) {
        return cardNumber == null
                ? transactions.findAll(pageable)
                : transactions.findByCardNum(cardNumber, pageable);
    }

    @Transactional(readOnly = true)
    public Transaction get(String transactionId) {
        return transactions.findById(transactionId)
                .orElseThrow(() -> new NotFoundException("Transaction " + transactionId + " not found"));
    }
}
