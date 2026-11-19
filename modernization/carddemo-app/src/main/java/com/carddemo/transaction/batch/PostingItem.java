package com.carddemo.transaction.batch;

import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.service.ValidationResult;

/** One DALYTRAN record with its {@code 1500-VALIDATE-TRAN} outcome: posted when valid, rejected otherwise. */
public record PostingItem(DailyTransaction transaction, ValidationResult validation) {
}
