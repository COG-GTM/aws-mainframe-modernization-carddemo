package com.carddemo.batch.posttran.logic;

import com.carddemo.batch.posttran.domain.Account;
import com.carddemo.batch.posttran.domain.CardXref;

/** WS-VALIDATION-TRAILER plus the records 1500-VALIDATE-TRAN read (needed by 2000-POST-TRANSACTION). */
public record ValidationResult(int reasonCode, String description, CardXref xref, Account account) {

    public static final int INVALID_CARD = 100;
    public static final int ACCOUNT_NOT_FOUND = 101;
    public static final int OVERLIMIT = 102;
    public static final int AFTER_EXPIRATION = 103;

    public boolean valid() {
        return reasonCode == 0;
    }
}
